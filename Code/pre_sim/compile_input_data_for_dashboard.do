/*******************************************************************************
 Script:       compile_input_data_for_dashboard.do
 Purpose:      Compiles the per-draw calibrated catch-per-trip Excel files into
               a single mean_catch_per_trip.csv for the dashboard: averages cod
               and haddock catch by month/mode within each draw, stacks all
               draws, and exports.
 Inputs:       calib_catch_draws_1.xlsx ... calib_catch_draws_201.xlsx
               (in the hardcoded process_data directory below).
 Outputs:      mean_catch_per_trip.csv (in the hardcoded input_data directory).
 Dependencies: None via globals. NOTE: paths are hardcoded developer-specific
               absolute paths (E:\Lou_projects\...), so this file is not
               portable as written.
 Pipeline:     Standalone / unwrapped — no confirmed caller (per
               DATAFLOW_GROUNDFISH.md). Runs after the catch-per-trip
               calibration steps have produced the per-draw Excel files.
 Dev paths:    2 hardcoded absolute paths to a developer's local machine
               (E:\), at lines 20 and 50 (plus one more named in an
               explanatory comment at line 55).
*******************************************************************************/


* input catch per trip data
cd "E:\Lou_projects\groundfishRDM\process_data"

clear
tempfile base
save `base', replace emptyok

display "Compiling catch-per-trip from 201 per-draw Excel files (this can take a while) ..."

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

display "Finished compiling per-draw catch-per-trip; exporting CSV."

u `base', clear

export delimited using "E:\Lou_projects\groundfishRDM\input_data\mean_catch_per_trip.csv", replace 


* input directed trip data - 3/30/2026 - I added a few lines in directed_trips_calibration.do to pull this info before transforming to the daily level. 

* input baseline and projected catch at length - 3/30/2026 - these data are pulled directed from Lou's repository  "E:\Lou_projects\groundfishRDM\input_data" 








