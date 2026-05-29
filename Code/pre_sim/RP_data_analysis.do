
*Put the MRIP catch data in the directory

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

**MRIP catch data
global yr_wvs 20221 20222 20223 20224 20225 20226  ///
20111 20112 20113 20114 20115 20116  ///
20171 20172 20173 20174 20175 20176  
global yearlist 2011 2017 2022
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


/*
u "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\atl_states_2017_expsurvey.dta", clear 
renvarlab, lower 

u "C:\Users\andrew.carr-harris\Desktop\trip_master_final.dta", clear 
renvarlab, lower 
*/

global input_data_cd "E:\Lou_projects\groundfishRDM\2027_mgt_cycle\miscellaneous" /* Lou's local data path */
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

keep if year==2022

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
replace common_dom="BSB"  if inlist(prim1_common, "blackseabass") 
replace common_dom="TUNA"  if inlist(prim1_common, "bluefintuna", "yellowfintuna", "tunagenus") 
replace common_dom="BLU"  if inlist(prim1_common, "bluefish") 
replace common_dom="POL"  if inlist(prim1_common, "pollock") 
replace common_dom="SCUP"  if inlist(prim1_common, "scup") 
replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="TAU"  if inlist(prim1_common, "tautog") 

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


order strat_id psu_id id_code    
keep strat_id psu_id id_code common_dom area_s  mode1 wp_int
duplicates drop 

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
order groundfish strat_id psu_id id_code 
drop common_dom area_s

gen cod_open =1 if inlist(month, 9, 10)
mvencode cod_open, mv(0)

gen haddock_open =1 if !inlist(month, 4)
mvencode haddock_open, mv(0)

gen year=2022

tempfile y2022
save `y2022', replace

* 2011 data 
u "C:\Users\andrew.carr-harris\Desktop\trip_master_final.dta", clear 
renvarlab * , lower 

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

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override

*replace some durable expenses included in "other" category as zero
replace othexp=0 if inlist(oth_cat, "2 LICENSES", "BOAT REPAIR", "Boat Towing", "CART", "FISHING LICENSE")
replace othexp=0 if inlist(oth_cat,"LICENSE", "LICENSES", "MONEY SPENT AT CASINO", "NEW ROD", "SEATOW", "SPA", "HAT")

* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

svyset psu_id [pweight= sample_wt], strata(strat_id) singleunit(certainty)


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

keep if year==2011

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
replace common_dom="BSB"  if inlist(prim1_common, "blackseabass") 
replace common_dom="TUNA"  if inlist(prim1_common, "bluefintuna", "yellowfintuna", "tunagenus") 
replace common_dom="BLU"  if inlist(prim1_common, "bluefish") 
replace common_dom="POL"  if inlist(prim1_common, "pollock") 
replace common_dom="SCUP"  if inlist(prim1_common, "scup") 
replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="TAU"  if inlist(prim1_common, "tautog") 

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


order strat_id psu_id id_code    
keep strat_id psu_id id_code common_dom area_s  mode1 wp_int
duplicates drop 

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
order groundfish strat_id psu_id id_code 
drop common_dom area_s

gen cod_open =1 if inlist(month,4, 5, 6, 7, 8, 9, 10)
mvencode cod_open, mv(0)

gen haddock_open =1 if !inlist(month, 4)
mvencode haddock_open, mv(0)

gen year=2011

tempfile y2011
save `y2011', replace 


* 2017 data 
u "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\atl_states_2017_expsurvey.dta", clear 
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

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override

*replace some durable expenses included in "other" category as zero
replace othexp=0 if inlist(oth_cat, "2 LICENSES", "BOAT REPAIR", "Boat Towing", "CART", "FISHING LICENSE")
replace othexp=0 if inlist(oth_cat,"LICENSE", "LICENSES", "MONEY SPENT AT CASINO", "NEW ROD", "SEATOW", "SPA", "HAT")

* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

svyset psu_id [pweight= sample_wt], strata(strat_id) singleunit(certainty)


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

keep if year==2017

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
replace common_dom="BSB"  if inlist(prim1_common, "blackseabass") 
replace common_dom="TUNA"  if inlist(prim1_common, "bluefintuna", "yellowfintuna", "tunagenus") 
replace common_dom="BLU"  if inlist(prim1_common, "bluefish") 
replace common_dom="POL"  if inlist(prim1_common, "pollock") 
replace common_dom="SCUP"  if inlist(prim1_common, "scup") 
replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="TAU"  if inlist(prim1_common, "tautog") 

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


order strat_id psu_id id_code    
keep strat_id psu_id id_code common_dom area_s  mode1 wp_int
duplicates drop 

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
order groundfish strat_id psu_id id_code 
drop common_dom area_s

gen cod_open =1 if inlist(month,9, 10)
mvencode cod_open, mv(0)

gen haddock_open =1 if !inlist(month, 4)
mvencode haddock_open, mv(0)

gen year=2017


append using `y2011'
append using `y2022'


encode mode1, gen(mode2)
svyset [pweight=wp_int] 
svy: logit groundfish i.year   i.cod_open##i.month i.mode2
svy: logit groundfish i.year   i.cod_open##i.month i.mode2


gen trip_id=_n
probit groundfish i.year  i.cod_open##i.month 


egen avg_cost=mean(total_exp), by(month trip_type)
gen orig_trip_type=groundfish

expand 2, gen(dup)
order trip_id
sort trip_id

bysort trip_id (dup): gen alt=_n
order alt
gen choice=1 if dup==0
order choice
order dup
mvencode choice, mv(0)

replace groundfish=0 if orig_trip_type==1 & dup==1
replace groundfish=1 if orig_trip_type==0 & dup==1

sort trip_id alt


replace total_exp=avg if dup==1

clogit choice total_exp groundfish, group(trip_id)


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
				+beta_cost*total_exp if trip_type=="groundfish"

gen exp_Q_gf=exp(Q_gf)
egen sum_exp_Q_gf= sum(exp_Q_gf), by(month)
gen log_exp_Q_gf= log(sum_exp_Q_gf)


gen cod_open =1 if inlist(month, 9, 10)
mvencode cod_open, mv(0)

reg groundfish log_exp_Q_gf cod_open

				





