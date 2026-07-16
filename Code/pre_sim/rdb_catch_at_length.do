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


//To test change everything to 0 in execution control in model wrapper other than prep_cal_for_dashboard and Rpush_cal_to_gdrive if you've already run the catch-at-length calibration code you will have what you need. And if not you can save the baseline_catch_at_length.csv from the misc folder locally

/*
loc pull_assessment = 0		 		// Pull Assessment data
loc processMRIP = 0		 			// deal with casing MRIP data
loc assemblemriplists = 0		 	// deal with casing MRIP data

loc estimate_dtrips = 0				// Estimate Directed Trips 
loc costs_per_trip = 0  			// Create Distributions of costs per trip (run 1x)
loc draw_angler_preferences = 0		// Create draw of angler preference parameters (run 1x)
loc catch_per_trip1 = 0				// Part 1 of catch per trip
loc copula_in_R = 0					// Copula model in R
loc catch_per_trip2 = 0				// Part 2 of catch per trip
loc compare_calibration_MRIP = 0	// compare calibration output to MRIP
loc prep_cpt_for_dashboard= 0		// prep data for dashboard
loc Rpush_cpt_to_gdrive =0 			// Push to google drive in R
loc angler_demogs	=0				// add additonal angler demographics
loc generate_baseline=0				// Generate baseline-year catch-at-length
loc prep_cal_for_dashboard= 1		// Prep catch at length data for dashboard
loc Rpush_cal_to_gdrive =1 			// Push to google drive in R
loc catch_at_length_project=0			// Generate projection-year catch-at-length
loc run_calibration=0						// Run calibration routine in R
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

//should year go in?
gen year=2025

// kim fine with extracting the lengths in inches that only have one decimal place from the metric column

order fishery common species_itis stock_abbrev data_version year metric value units source

save "$misc_data_cd\rdb_cat_len.dta", replace 



