* Handle strings and such 


foreach l in $catchlist $triplist $b2list $sizelist {

	use `l', clear
	foreach var of varlist strat_id psu_id id_code zip{
		cap tostring `var', replace
	}

	foreach var of varlist year wave st{
		destring `var', replace
	}
	
	/* filter based on the global yr_wvs */
	
	gen yr_wave=year*10+wave	
	gen yrwave_keep=0
	qui foreach l of global yr_wvs{
		replace yrwave_keep=1 if yr_wave==`l'
	}
	keep if yrwave_keep==1
	drop yrwave_keep yr_wave
	
save `l', replace
}