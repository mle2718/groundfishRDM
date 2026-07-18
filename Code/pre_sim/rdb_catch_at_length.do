/*******************************************************************************
 Script:       rdb_catch_at_length.do
 Purpose:      Cleans the simulated catch-at-length data and formats it for the
               recDST dashboard: takes medians across 101 draws of the observed
               and fitted (smoothed) catch-at-length probabilities for WGOM cod
               and haddock, converts lengths from cm to inches, stacks observed
               and fitted, and adds descriptive columns.
 Inputs:       $misc_data_cd/baseline_catch_at_length.csv (written by
               catch_at_length_calibration.do).
 Outputs:      $misc_data_cd/rdb_cat_len.dta
 Dependencies: Global $misc_data_cd (set in model_wrapper.do).
 Pipeline:     Wrapped by model_wrapper.do, gated by `prep_catch_at_length_for_dash'
               (default ON). Followed by rdb_catch_at_len_to_drive.R, which pushes
               the output to Google Drive as an Rds.
 Note:         The prior header comment listed the input as
               baseline_catch_at_length_observed.csv and the output as
               rdb_cat_len.dta.dta; the code actually reads
               baseline_catch_at_length.csv and writes rdb_cat_len.dta (header
               updated to match the code).
*******************************************************************************/



import delimited "$misc_data_cd\baseline_catch_at_length.csv", clear

*Take medians of 101 draws of the observed and fitted probabilities for catch at length

/******************************************************************************/
/******************************************************************************/
/* Section A: Median observed catch-at-length probability */
/******************************************************************************/
/******************************************************************************/

//observed probability
preserve
collapse (median) observed_prob, by(season species length)
//convert to inches
replace length=length/2.54
//reduce number of decimals for the metric column
gen len_r = round(length, 0.1)
tostring len_r, replace format("%12.1f") force
gen metric = season+" "+len_r
drop season len_r
gen units="observed proportion of catch" 
rename observed_prob value
tempfile obs
save `obs', replace 
restore

/******************************************************************************/
/******************************************************************************/
/* Section B: Median fitted (smoothed) catch-at-length probability */
/******************************************************************************/
/******************************************************************************/

//fitted (smoothed) probability
collapse (median) fitted_prob, by(season species length)
//convert to inches
replace length=length/2.54
//reduce number of decimals for the metric column
gen len_r = round(length, 0.1)
tostring len_r, replace format("%12.1f") force
gen metric = season+" "+len_r
drop season len_r
gen units="fitted proportion of catch" 
rename fitted_prob value

//stack the fitted and observed probabilities
append using `obs'

/*check to make sure everything looks right
split metric, gen(season)

twoway (line value length if species=="cod" & units=="observed proportion of catch") (line value length if species=="cod"  & units=="fitted proportion of catch"), by(season1, title("Cod catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (in)") ytitle("Proportion")
 
twoway (line value length if species=="hadd" & units=="observed proportion of catch") (line value length if species=="hadd"  & units=="fitted proportion of catch"), by(season1, title("Hadd catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (in)") ytitle("Proportion")

drop season1-season4
*/


/******************************************************************************/
/******************************************************************************/
/* Section C: Add descriptive columns for the dashboard and save */
/******************************************************************************/
/******************************************************************************/

// Add columns for dashboard
gen common = "atlanticcod" if species=="cod"
replace common = "haddock" if species=="hadd"
gen species_itis = 164712 if species=="cod"
replace species_itis = 164744 if species=="hadd"
drop species length

// Update this 
gen data_version="2026-06-29"

gen source="model intermediate"
gen stock_abbrev="WGOM"
gen fishery= "NE Groundfish"

//get rid of var label on value 
label variable value ""

//should year go in?
gen year=2025

// kim fine with extracting the lengths in inches that only have one decimal place from the metric column

order fishery common species_itis stock_abbrev data_version year metric value units source

save "$misc_data_cd\rdb_cat_len.dta", replace 



