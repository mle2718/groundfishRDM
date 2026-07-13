/* Tidyup mrip data that comes from R's tacklebox*/
/* In the data processing chain, this runs after 
	rscript using "$input_code_cd\get_mrip_oracle.R", args($first_mrip_year $last_mrip_year)
	
Inputs -- mrip trip, catch, size, sizeb2 files
Outputs -- same, with these modifications:
	strat_id psu_id id_code zip are forced to strings
	year wave st are forced to numeric
	
	Filterned to include yearwave patterns from $yr_wvs in the model_wrapper.do file.	
*/

foreach l in $catchlist $triplist $b2list $sizelist {

	use `l', clear
	/* enforce certain variables as strings */
	foreach var of varlist strat_id psu_id id_code zip{
		cap tostring `var', replace
	}
	/* enforce other variables as numeric */
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