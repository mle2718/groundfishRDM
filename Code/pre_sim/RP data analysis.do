

u "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\atl_states_2017_expsurvey.dta", clear 
renvarlab, lower 

u "C:\Users\andrew.carr-harris\Desktop\trip_master_final.dta", clear 
renvarlab, lower 

global input_data_cd "E:\Lou_projects\groundfishRDM\input_data" /* Lou's local data path */
global input_code_cd "C:\Users\andrew.carr-harris\Desktop\Git\groundfishRDM\Code\pre_sim"

*Enter a directory with the expenditure survey data 
u "$input_data_cd\gulf_atl_2022.dta", clear
renvarlab *, lower


* As per Sabrina, run the following code before using the 2022 data. This code sets certain expenditure variables to missing depending on the trip mode. 
* For-Hire trips: set boat fuel and boat rental to missing
replace bfuelexp = . if mode == "For-Hire"
replace brentexp = . if mode == "For-Hire"

* Private Boat trips: set guide costs and crew tips to missing
replace guideexp = . if mode == "Private Boat"
replace crewexp  = . if mode == "Private Boat"

* Shore trips: set all of those to missing
replace bfuelexp = . if mode == "Shore"
replace crewexp  = . if mode == "Shore"
replace guideexp = . if mode == "Shore"
replace brentexp = . if mode == "Shore"

/*
*keep only the states we need (MA-NC) 
keep if inlist(st, 25, 44, 9, 36, 34, 10, 24, 51, 37, 23, 33)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33
*/

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override

*replace some durable expenses included in "other" category as zero
replace othexp=0 if inlist(oth_cat, "2 LICENSES", "BOAT REPAIR", "Boat Towing", "CART", "FISHING LICENSE")
replace othexp=0 if inlist(oth_cat,"LICENSE", "LICENSES", "MONEY SPENT AT CASINO", "NEW ROD", "SEATOW", "SPA", "HAT")

* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

keep if inlist(st, 23, 33, 25)

keep  strat_id psu_id id_code total_exp

tempfile costs
save `costs', replace 

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

**MRIP catch data
global yr_wvs 20221 20222 20223 20224 20225 20226  
global yearlist 2022
global wavelist 1 2 3 4 5 6


**************************************************Model calibration ************************************************** 
// 1) Pull the MRIP data

/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year in $yearlist{
	foreach wave in  $wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}


clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")


* Format MRIP data for estimation 
gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

* Ensure only relevant states 
keep if inlist(st, 23, 33, 25)

gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted ) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target ) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 

*New MRIP site allocations
preserve 
import delimited using "$input_data_cd/MRIP_COD_ALL_SITE_LIST.csv", clear 
keep if inlist(state, "MA", "ME")
keep state intsite nmfs_stock_area nmfs_stat_area
sort intsite nmfs_stock_area  
replace nmfs_stock_area="WGOM" if inlist(nmfs_stat_area, 521, 526, 541, 514, 513, 515)
replace nmfs_stock_area="XX" if !inlist(nmfs_stat_area, 521, 526, 541, 514, 513, 515)
keep nmfs_stock_area intsite nmfs_stat_area state
duplicates drop
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite state using `mrip_sites',  keep(1 3) nogen

/*classify into WGOM or not WGOM */
gen str3 area_s="XX"
replace area_s="WGOM" if st2=="33"
replace area_s=nmfs_stock_area if inlist(st2, "25", "23") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=area_s+"_"+month+"_"+mode1+"_"+common_dom

* Define the list of species to process
local species "atlanticcod haddock"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "atlanticcod" local short "cod"
    if "`s'" == "haddock"     local short "hadd"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)

    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
}

rename sum_cod_tot_cat cod_cat
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_hadd_tot_cat hadd_cat
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup  count_obs1 
keep strat_id psu_id id_code cod_keep cod_rel hadd_keep hadd_rel common_dom area_s  mode1 wp_int

merge 1:1  strat_id psu_id id_code  using `costs'
 
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace
destring month1, replace

keep if _merge==3
tab mode1
drop _merge 

gen trip_type="groundfish" if common_dom=="ATLCO" & area_s=="WGOM"
tab mode1 if trip_type=="groundfish"

drop if mode1=="sh"
replace trip_type="other" if trip_type!="groundfish"

gen groundfish=1 if trip_type=="groundfish"
mvencode groundfish, mv(0) override
order groundfish strat_id psu_id id_code cod_keep cod_rel hadd_keep hadd_rel
drop common_dom area_s

preserve
u   "E:\Lou_projects\groundfishRDM\process_data\preference_params.dta", clear
collapse (mean) beta*
keep beta_sqrt_cod_keep beta_sqrt_cod_release beta_sqrt_hadd_keep beta_sqrt_hadd_release beta_sqrt_cod_hadd_keep beta_cost
gen tab=1
tempfile params
save `params', replace 
restore

gen tab=1
merge m:1 tab using `params' 

gen Q_gf= beta_sqrt_cod_keep*sqrt(cod_keep) +beta_sqrt_cod_release*sqrt(cod_rel) ///
				+beta_sqrt_hadd_keep*sqrt(hadd_keep) + beta_sqrt_hadd_release*sqrt(hadd_rel) ///
				+beta_sqrt_cod_hadd_keep*sqrt(cod_keep)*sqrt(hadd_keep) ///
				+beta_cost*total_exp

gen exp_Q_gf=exp(Q_gf)
egen sum_exp_Q_gf= sum(exp_Q_gf), by(month)
gen log_exp_Q_gf= log(sum_exp_Q_gf)


gen cod_open =1 if inlist(month, 9, 10)
mvencode cod_open, mv(0)

reg groundfish log_exp_Q_gf i.month cod_open


				





