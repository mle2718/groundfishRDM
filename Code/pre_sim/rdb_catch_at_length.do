*********** WGOM COD & GOM HADDOCK CATCH AT LENGTH ***********

/*
This code pulls the median of 101 draws of simulated catch at length probabilities for Western Gulf of Maine (WGOM)  Atlantic Cod and Gulf of Maine (GOM) Haddock. We provide the observed probabilities of catch at length and the fitted (smoothed) probabilities of catch at length in inches for the baseline year. We also provide the fitted (smoothed) probabilities of catch at length in inches for the projection year. Catch at length for groundfish is provided at the season level where the 'summer' season is May - August and the 'winter' is September - April.  

This code cleans the simulated catch at length data files compiled in catch_at_length_calibration.do and catch_at_length_projection.do and formats the data for use in the recDST data dashboard. 


 Name: rdb_catch_at_length.do
 Inputs: baseline_catch_at_length_observed.csv; projected_catch_at_length.csv
 Outputs: rdb_cat_len.dta
 Description: Grabs the median of 101 draws of observed and fitted proportions caught at length for Atlantic Cod and Haddock by season for the baseline year. The 101 draws are created by multiplying discard at length and harvest at length probabilities for the baseline year by 101 random draws of simulated total harvest and total discards. The total numbers of fish harvested and discarded at length are added to get total numbers caught at length, which is then converted to probabilities. Those probabilities are then fitted to a gamma distribution. See catch_at_length_calibration.do for the code. 
 
We also take the medians of 101 draws of fitted proportions caught at length by season and species for the projection year. The projected catch at length distribution is based on the baseline observed catch at length distribution and the projected numbers at age from the stock assessment. See catch_at_length_projection.do for the code. 

 General strategy:
  1. Read in data
  2. Collapse data to get median probabilities caught at length for Cod and Haddock by season (both observed and fitted for baseline year and fitted probabilities for projection year)
  3. Add descriptive columns for dashboard
  4. Run rdb_catch_at_len_to_drive.R to push the processed data to Google Drive as an Rds
  
*/


* Import 101 draws of baseline catch at length probabilities
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
gen units="baseline observed proportion of catch" 
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
gen units="baseline fitted proportion of catch" 
rename fitted_prob value

//stack the fitted and observed probabilities
append using `obs'

tempfile base
save `base', replace 

/*check to make sure everything looks right
split metric, gen(season)

twoway (line value length if species=="cod" & units=="baseline observed proportion of catch") (line value length if species=="cod"  & units=="baseline fitted proportion of catch"), by(season1, title("Cod catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (in)") ytitle("Proportion")
 
twoway (line value length if species=="hadd" & units=="baseline observed proportion of catch") (line value length if species=="hadd"  & units=="baseline fitted proportion of catch"), by(season1, title("Hadd catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (in)") ytitle("Proportion")

drop season1-season4
*/


* Import 101 draws of projected catch at length probabilities
import delimited "$misc_data_cd\projected_catch_at_length.csv", clear

*Take medians of 101 draws of the projected fitted probabilities for catch at length
collapse (median) fitted_prob, by(season species length)
//convert to inches
replace length=length/2.54
//reduce number of decimals for the metric column
gen len_r = round(length, 0.1)
tostring len_r, replace format("%12.1f") force
gen metric = season+" "+len_r
drop season len_r
gen units="projected fitted proportion of catch" 
rename fitted_prob value

//append with the baseline fitted and observed probabilities
append using `base'


/*check to make sure everything looks right
split metric, gen(season)

twoway (line value length if species=="cod" & units=="baseline fitted proportion of catch") (line value length if species=="cod"  & units=="projected fitted proportion of catch"), by(season1, title("Cod catch at length")) legend( label(1 "Baseline") label(2 "Projected")) xtitle("Length (in)") ytitle("Proportion")
 
twoway (line value length if species=="hadd" & units=="baseline fitted proportion of catch") (line value length if species=="hadd"  & units=="projected fitted proportion of catch"), by(season1, title("Hadd catch at length")) legend( label(1 "Baseline") label(2 "Projected")) xtitle("Length (in)") ytitle("Proportion")

drop season1-season4
*/



// Add columns for dashboard
gen common = "atlanticcod" if species=="cod"
replace common = "haddock" if species=="hadd"
gen species_itis = 164712 if species=="cod"
replace species_itis = 164744 if species=="hadd"
drop species length

gen source="model intermediate"
gen stock_abbrev="WGOM"
replace stock_abbrev="GOM" if common=="haddock"
gen fishery= "NE Groundfish"

//get rid of var label on value 
label variable value ""

//technically some of the data came from 2024 but 2025 is the regulatory baseline year
gen year=2025
replace year=2026 if units=="projected fitted proportion of catch"


order fishery common species_itis stock_abbrev year metric value units source

save "$misc_data_cd\rdb_cat_len.dta", replace 




