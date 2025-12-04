
***This code creates trip cost distributions based on the Sabrina's 2017 trip expenditure survey data

set seed $seed

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


*keep only the states we need (ME-NC) 
keep if inlist(st, 23, 33, 25)


mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override

* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

gen st2 = string(st,"%02.0f")
gen state="CT" if st2=="09" 
replace state="DE" if st2=="10"
replace state="ME" if st2=="23"
replace state="MD" if st2=="24"
replace state="MA" if st2=="25"
replace state="NJ" if st2=="34"
replace state="NY" if st2=="36"
replace state="NC" if st2=="37"
replace state="RI" if st2=="44"
replace state="VA" if st2=="51"
replace state="NH" if st2=="33"

*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

*Adjust for inflation
replace total_exp = total_exp*$inflation_expansion


* Generate total expenditures each state/mode combination

tempfile new
save `new', replace

global costs

levelsof mode1, local(modes)
foreach m of local modes{
	
	u `new', clear
	keep if mode1=="`m'"

	replace wp_int=round(wp_int)
	expand wp_int
	count
	if `r(N)'<10000{
		local expand = round(10000/`r(N)')+1
		expand `expand'
		sample 10000, count

	}
	else{
	sample 10000, count
	}
	
	tempfile costs`m'
	save `costs`m'', replace
	global costs "$costs "`costs`m''" " 
}

clear
dsconcat $costs
keep mode1 total_exp
rename total_exp cost 
save "$iterative_input_data_cd\trip_costs.dta", replace 