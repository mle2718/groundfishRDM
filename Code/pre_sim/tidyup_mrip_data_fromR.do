* Handle strings and such 


foreach l in $catchlist $triplist $b2list $sizelist {

	use `l', clear
	foreach var of varlist strat_id psu_id id_code zip{
		cap tostring `var', replace
	}

	foreach var of varlist year wave st{
		destring `var', replace
	}


save `l', replace
}