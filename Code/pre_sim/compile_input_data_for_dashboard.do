

* input catch per trip data
cd "E:\Lou_projects\groundfishRDM\process_data"

clear 
tempfile base 
save `base', replace emptyok

forv i = 1/201{
	
	import excel using "calib_catch_draws_`i'.xlsx", clear first
	gen cod_catch=cod_keep + cod_rel_sim
	gen hadd_catch=hadd_keep + hadd_rel
	collapse (mean) cod_catch hadd_catch, by(my) 
	split my, parse(_)
	rename my_dom_id_string1 month
	rename my_dom_id_string2 mode
	destring month, replace 
	drop my*
	gen draw = `i'
	order mode month draw cod hadd 
	
	append using `base'
	save `base', replace
}

u `base', clear 

export delimited using "E:\Lou_projects\groundfishRDM\input_data\mean_catch_per_trip.csv", replace 


* input directed trip data - 3/30/2026 - I added a few lines in directed_trips_calibration.do to pull this info before transforming to the daily level. 

* input baseline and projected catch at length - 3/30/2026 - these data are pulled directed from Lou's repository  "E:\Lou_projects\groundfishRDM\input_data" 








