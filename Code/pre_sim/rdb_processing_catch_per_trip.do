*********** WGOM COD & HADDOCK CATCH PER TRIP ***********

/*
This code pulls the median, minimum, and maximum of the 100 draws of simulated mean catch per trip (ie, harvest + discards, or A + B1 + B2) at the the mode-month level for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM). 

This code cleans the simulated catch per trip data compiled in compare_calibration_data_to_MRIP.do and saved in simulated_catch_totals3.dta and formats the data for use in the rec dashboard. 


 Name: rdb_processing_catch_per_trip.do
 Inputs: simulated_catch_totals3.dta
 Outputs: rdb_sim_catch_per_trip.dta
 Description: Grabs the median, min, and max of catch per trip at the the mode-month level for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM), based on 100 random draws of mean catch per trip.
 General strategy:
  1. Read in data
  2. Collapse data to get median, min, and max catch per trip at the mode-month level for Cod and then Haddock
  3. Stack median, min, and max catch per trip for Cod and Haddock  
  4. Add descriptive columns for dashboard
  5. Run rdb_catch_per_trip_to_drive.R to push the processed data to Google Drive as an Rds
  
*/


u "$misc_data_cd\simulated_catch_totals3.dta", clear

*keep only necessary columns
keep mode month dtrip cod_cat_sim hadd_cat_sim


** Take median, max, and min of cod catch per trip (cod_cat_sim) and haddock catch per trip (hadd_cat_sim) by mode and month, then append them:

*cod median
preserve
collapse (median) cod_cat_sim, by(month mode)
gen metric = "median catch per trip"  
gen units="number of fish" 
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
gen units="number of fish" 
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
gen units="number of fish"  
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
gen units="number of fish" 
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
tempfile h_med
save `h_med', replace 
restore

*haddock min
preserve
collapse (min) hadd_cat_sim, by(month mode)
gen metric = "min catch per trip" 
gen units="number of fish" 
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
gen units="number of fish"  
gen common = "haddock"
gen species_itis = 164744
rename hadd_cat_sim value
tempfile h_max
save `h_max', replace 
restore

clear
// clear then append all 6 of them
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

//Update this 
gen data_version="2026-05-12"
//state will be NA
gen state=.
gen source="model intermediate"

*reorder columns. sort data on month, common, mode.
order fishery common species_itis stock_abbrev state mode data_version year wave month metric value units source
sort month common mode

//get rid of var label on value 
label variable value ""


save "$misc_data_cd\rdb_sim_catch_per_trip.dta", replace 


*graph bar value if metric=="median catch per trip", over(month) over(common) by(mode) ytitle("Median catch per trip by mode-month")  scheme(stmono1) //xtitle("Month") 

*graph bar value if metric=="median catch per trip" & common=="atlanticcod", over(month) by(mode) ytitle("Median cod catch per trip by mode-month")  scheme(stmono1) //xtitle("Month") 

*graph bar value if metric=="median catch per trip" & common=="haddock", over(month) by(mode) ytitle("Median hadd catch per trip by mode-month")  scheme(stmono1) //xtitle("Month") 


// better to show month level rather than wave because of variability across months (May catch per trip is much higher than June - see bar chart above)



