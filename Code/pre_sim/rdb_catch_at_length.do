*********** WGOM COD & HADDOCK CATCH AT LENGTH ***********

/*
This code pulls the median of 101 draws of simulated catch at length probabilities for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM). We take the observed probabilities of catch at length and the fitted (smoothed) probabilities of catch at length in inches. 

This code cleans the simulated catch at length data compiled in catch_at_length_calibration.do and saved in baseline_catch_at_length.csv and formats the data for use in the recDST data dashboard. 


 Name: rdb_catch_at_length.do
 Inputs: baseline_catch_at_length_observed.csv
 Outputs: rdb_cat_len.dta.dta
 Description: Grabs the median proportions caught at length for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM), based on 101 random draws of mean simulated total catch.
 General strategy:
  1. Read in data
  2. Collapse data to get median probabilities caught at length (observed and fitted) for Cod and Haddock
  3. Add descriptive columns for dashboard
  4. Run rdb_catch_at_len_to_drive.R to push the processed data to Google Drive as an Rds
  
*/



import delimited "$misc_data_cd\baseline_catch_at_length.csv", clear

*Take medians of 101 draws of the observed and fitted probabilities for catch at length 

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



