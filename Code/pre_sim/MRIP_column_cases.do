


* This code only needs to be run once after new MRIP data enters the repo
* It handles  casing for the datasets
*
qui foreach wave in	$yr_wvs {				

capture confirm file "${misc_data_cd}/trip_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/trip_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/trip_`wave'.dta", replace
}

else{
	
}

capture confirm file ""${misc_data_cd}/size_b2_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/size_b2_`wave'.dta", clear
	renvarlab, lower
	save ""${misc_data_cd}/size_b2_`wave'.dta", replace
}

else{
	
}

capture confirm file "${misc_data_cd}/size_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/size_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/size_`wave'.dta", replace
}

else{
	
}

capture confirm file "${misc_data_cd}/catch_`wave'.dta"
if _rc==0{
	use "${misc_data_cd}/catch_`wave'.dta", clear
	renvarlab, lower
	save "${misc_data_cd}/catch_`wave'.dta", replace
}

else{
	
}

}


