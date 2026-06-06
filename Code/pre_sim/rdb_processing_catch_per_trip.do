

/*
This code creates a distribution of trip counts by number of fish caught (ie, harvest + discards, or A + B1 + B2) at the the mode-month level for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM).

This code cleans the raw MRIP catch per trip data compiled during Part A of rdb_calibration_catch_per_trip_part1.do and formats the data for use in the rec dashboard. 

(this code could be here in its own do file or tacked on to the end of Part A in rdb_calibration_catch_per_trip_part1.do)
(will decide later where it should be)


 Name: rdb_processing_catch_per_trip.do
 Inputs: baseline_mrip_catch_processed.dta
 Outputs: rdb_mrip_catch_per_trip.xlsx
 Description: Builds a distribution of trip counts by number of fish caught (ie, harvest + discards, or A + B1 + B2) at the the mode-month level for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM), sourced from MRIP microdata.
 General strategy:
  1. Read in data
  2. Collapse trip count at our aggregation level for Cod and then Haddock
  3. Stack Cod and Haddock data 
  4. Add descriptive columns for dashboard
	
*/


cd $misc_data_cd

use baseline_mrip_catch_processed.dta, clear

*keep only necessary columns
keep my_dom_id_string strat_id psu_id id_code year wp_int cod_cat hadd_cat

*parse out month, mode, stock_abbrev from my_dom_id_string
split my_dom_id_string, parse(_)
rename my_dom_id_string1 month
rename my_dom_id_string2 mode
rename my_dom_id_string3 stock_abbrev
rename my_dom_id_string4 common_dom

gen month1 = substr(strat_id, 5, 2)
destring month1, replace

*grabbing state and wave but won't use them right now
gen st = substr(strat_id, 7, 2)
gen state="MA" if st=="25"
replace state="NH" if st=="33"
replace state="ME" if st=="23"

gen wave = substr(psu_id, 5, 1)
destring wave, replace

//why are there trips where cod_cat is 6.038303 and hadd_cat is 20.85714, etc?  Ask lou
// Rounding to nearest whole number for now. It's an issue bc then the sum of all the catch per trips won't equal total catch. Do we want rows were one trip caught 5.987235 fish? 
tab cod_cat
tab hadd_cat
replace cod_cat = round(cod_cat)
replace hadd_cat = round(hadd_cat)


//Insert month column after wave? go back to trips and catch and add a month column with NA's?
//For state, put NA or 'new england' as state?
//write out mode as 'for hire' or 'private'? or its fine as fh and pr?


* Create cod and haddock datasets with trip counts by number of fish caught and then stack them 
// Cod
preserve
collapse (sum) wp_int, by(month year mode cod_cat stock_abbrev)
//do I need to gen rows for number of fish caught between 0-20 for cod where there are 0 trips?
*create metric column from cod_cat (check with group on this label)
gen metric = string(cod_cat) + " fish caught"
drop cod_cat
// add columns for common, species_itis
gen common = "atlanticcod"
gen species_itis = 164712
rename wp_int value
*save a tempfile
tempfile codfile
save `codfile', replace 
restore

// Haddock
collapse (sum) wp_int, by(month year mode hadd_cat stock_abbrev)
//gen rows for number of fish between 0-63 for hadd where there are  0 trips?
*create metric column from hadd_cat '0 fish caught', '1 fish caught', etc
gen metric = string(hadd_cat) + " fish caught"
drop hadd_cat
// add columns for common, species_itis
gen common = "haddock"
gen species_itis = 164744
rename wp_int value
*stack in cod 
append using `codfile'


*add dataframe columns common to both species: data version (05-12-2026 for now), units, fishery, state
gen units = "number of trips" //or just "trips"?
gen fishery= "NE Groundfish"
//ask lou when he pulled the dta's here. this folder says he saved them May 12: https://drive.google.com/drive/folders/1wIlpn5Q8_iBnZ0NUlKVVpzyI7x97zAdi   
//Did he pull in updated MRIP data that day? We should pull on same day and rerun the wrapper
gen data_version="2026-05-12"
//state will be NA
gen state=.
destring month, replace
recode month (1/2=1) (3/4=2) (5/6=3) (7/8=4) (9/10=5)  (11/12=6), gen(wave)

*reorder columns. sort data on month, mode, common.
order fishery common species_itis stock_abbrev state mode data_version year wave month metric value units
sort month mode common metric

*export to excel
export excel "$misc_data_cd\rdb_mrip_catch_per_trip.xlsx", firstrow(variables) replace
*import excel using "$misc_data_cd\rdb_mrip_catch_per_trip.xlsx", clear first

// need to get this on google drive





//strat_id has: year, month, st, region, mode_fx, kod, strat_interval
//psu has: year, wave, st, region, mode_fx, asg_code
//id_code: Assignment number (1 digit), interviewer code (4 digit), date (YYYYMMDD), Interview number (3 digit)




//I can grab things from the id variables like state if we want and make a version that aggregates at wave state mode level? Min-Yang said stick to what lou did.
// mode_fx is in strat_id, so I could separate the charter and headboats although I dont think we care about that





//SIMULATED CATCH PER TRIP, not MRIP

cd $misc_data_cd

u simulated_catch_totals3.dta, clear


*keep only necessary columns
keep mode month dtrip cod_cat_sim hadd_cat_sim


//get median, max, and min of cod catch per trip (cod_cat_sim) and haddock catch per trip (hadd_cat_sim) by mode and month, then append them 

*cod median
preserve
collapse (median) cod_cat_sim, by(month mode)
gen metric = "median catch per trip"   // later just change metric column at the end to catch per trip?
//should median go in units so it's median number of fish??   ASK KIM HER PREFERENCE
gen units="number of fish (median)" 
// add columns for common, species_itis
gen common = "atlanticcod"
gen species_itis = 164712
rename cod_cat_sim value
*save a tempfile
tempfile c_med
save `c_med', replace 
restore

*cod min
preserve
collapse (min) cod_cat_sim, by(month mode)
gen metric = "min catch per trip" 
gen units="number of fish (LL)" 
gen common = "atlanticcod"
gen species_itis = 164712
rename cod_cat_sim value
tempfile c_min
save `c_min', replace 
restore

*cod max
preserve
collapse (max) cod_cat_sim, by(month mode)
gen metric = "max catch per trip"  
gen units="number of fish (UL)"  
gen common = "atlanticcod"
gen species_itis = 164712
rename cod_cat_sim value
tempfile c_max
save `c_max', replace 
restore


*haddock median
preserve
collapse (median) hadd_cat_sim, by(month mode)
gen metric = "median catch per trip"
gen units="number of fish (median)" 
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
*save a tempfile
tempfile h_med
save `h_med', replace 
restore

*haddock min
preserve
collapse (min) hadd_cat_sim, by(month mode)
gen metric = "min catch per trip" 
gen units="number of fish (LL)" 
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
tempfile h_min
save `h_min', replace 
restore

*haddock max
preserve
collapse (max) hadd_cat_sim, by(month mode)
gen metric = "max catch per trip" 
gen units="number of fish (UL)"  
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
tempfile h_max
save `h_max', replace 
restore

clear
//then append all 6 of them
append using `c_med' `c_min' `c_max' `h_med' `h_min' `h_max'



*add dataframe columns common to both species
gen wave=1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)

*wave 6 is from 2024
gen year=2025 if month<=10
replace year=2024 if month>=11

gen stock_abbrev="WGOM"
gen fishery= "NE Groundfish"

//ask lou when he pulled the dta's here. this folder says he saved them May 12: https://drive.google.com/drive/folders/1wIlpn5Q8_iBnZ0NUlKVVpzyI7x97zAdi   
//Did he pull in updated MRIP data that day? We should pull on same day and rerun the wrapper
gen data_version="2026-05-12"
//state will be NA
gen state=.

*reorder columns. sort data on month, common, mode.
order fishery common species_itis stock_abbrev state mode data_version year wave month metric value units
sort month common mode 


save "$misc_data_cd\rdb_sim_catch_per_trip.dta", replace 


graph bar value if metric=="median catch per trip", over(month) by(mode) ytitle("Median catch per trip by mode-month")  scheme(stmono1) //xtitle("Month") 




//for documentation. it's the median, UL, and LL of the 100 draws of  mean catch per trip

// You could do the whole trip count by number of fish thing because you have d trip but I just dont know if we want to show it that way. Talk to Min yang next week





//mode-wave level
cd $misc_data_cd

u simulated_catch_totals3.dta, clear

*keep only necessary columns
keep mode month dtrip cod_cat_sim hadd_cat_sim tot_cod_cat_sim tot_hadd_cat_sim draw

gen wave=1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)

collapse (sum) dtrip tot_cod_cat_sim tot_hadd_cat_sim, by(mode wave draw)

gen cod_cat_sim=tot_cod_cat_sim/dtrip
gen hadd_cat_sim=tot_hadd_cat_sim/dtrip

//get median, max, and min of cod catch per trip (cod_cat_sim) and haddock catch per trip (hadd_cat_sim) by mode-wave, then append them 

*cod median
preserve
collapse (median) cod_cat_sim, by(mode wave)
gen metric = "median catch per trip"   // later just change metric column at the end to catch per trip?
//should median go in units so it's median number of fish??   ASK KIM HER PREFERENCE
gen units="number of fish (median)" 
// add columns for common, species_itis
gen common = "atlanticcod"
gen species_itis = 164712
rename cod_cat_sim value
*save a tempfile
tempfile c_med_w
save `c_med_w', replace 
restore

*cod min
preserve
collapse (min) cod_cat_sim, by(mode wave)
gen metric = "min catch per trip" 
gen units="number of fish (LL)" 
gen common = "atlanticcod"
gen species_itis = 164712
rename cod_cat_sim value
tempfile c_min_w
save `c_min_w', replace 
restore

*cod max
preserve
collapse (max) cod_cat_sim, by(mode wave)
gen metric = "max catch per trip"  
gen units="number of fish (UL)"  
gen common = "atlanticcod"
gen species_itis = 164712
rename cod_cat_sim value
tempfile c_max_w
save `c_max_w', replace 
restore


*haddock median
preserve
collapse (median) hadd_cat_sim, by(mode wave)
gen metric = "median catch per trip"
gen units="number of fish (median)" 
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
*save a tempfile
tempfile h_med_w
save `h_med_w', replace 
restore

*haddock min
preserve
collapse (min) hadd_cat_sim, by(mode wave)
gen metric = "min catch per trip" 
gen units="number of fish (LL)" 
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
tempfile h_min_w
save `h_min_w', replace 
restore

*haddock max
preserve
collapse (max) hadd_cat_sim, by(mode wave)
gen metric = "max catch per trip" 
gen units="number of fish (UL)"  
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
tempfile h_max_w
save `h_max_w', replace 
restore

clear
//then append all 6 of them
append using `c_med_w' `c_min_w' `c_max_w' `h_med_w' `h_min_w' `h_max_w'


*add dataframe columns common to both species
*wave 6 is from 2024
gen year=2025 if wave<=5
replace year=2024 if wave==6

gen stock_abbrev="WGOM"
gen fishery= "NE Groundfish"

//ask lou when he pulled the dta's here. this folder says he saved them May 12: https://drive.google.com/drive/folders/1wIlpn5Q8_iBnZ0NUlKVVpzyI7x97zAdi   
//Did he pull in updated MRIP data that day? We should pull on same day and rerun the wrapper
gen data_version="2026-05-12"
//state will be NA
gen state=.

*reorder columns. sort data on wave, common, mode.
order fishery common species_itis stock_abbrev state mode data_version year wave metric value units
sort wave common mode 

save "$misc_data_cd\rdb_sim_catch_per_trip_wave.dta", replace 




graph bar value if metric=="median catch per trip", over(wave) by(mode) ytitle("Median catch per trip by mode-wave")  scheme(stmono1) //xtitle("Month") 







