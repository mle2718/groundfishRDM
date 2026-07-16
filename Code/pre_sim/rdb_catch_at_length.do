*********** WGOM COD & HADDOCK CATCH AT LENGTH ***********

/*
This code pulls the median of 101 draws of simulated catch at length for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM). We take the observed probabilities of catch at length and the smoothed catch at length probability distribution. We also take the raw observed numbers of fish discarded at length and harvested at length (unweighted for cod and weighted for haddock). 

This code cleans the simulated catch at length data compiled in catch_at_length_calibration.do and saved in baseline_catch_at_length_observed.csv and formats the data for use in the rec dashboard. 


 Name: rdb__catch_at_length.do
 Inputs: baseline_catch_at_length_observed.csv
 Outputs: rdb_sim_catch_at_length.dta
 Description: Grabs the median number of fish caught at length for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM), based on 101 random draws of mean simulated total catch.
 General strategy:
  1. Read in data
  2. Collapse data to get median number of fish caught at length for Cod and then Haddock
  3. Add descriptive columns for dashboard
  4. Run rdb_catch_at_length_to_drive.R to push the processed data to Google Drive as an Rds
  
*/



// change everything to 0 in execution control in model wrapper other than assemblemriplists and generate_baseline




//medians of the observed and fitted probabilities for catch at length (doesn't have harvest and discards at length)
// observed is same as if you generated from rdb_cat_len.dta like above but this one trims the rows at the ends of the length distribution that are 0's
import delimited "$misc_data_cd\baseline_catch_at_length.csv", clear

collapse (median) observed_prob fitted_prob, by(season species length)
gen inches=length/2.54
gen inches_r = round(inches)
* CANT tostring the length in inches because now it has a bunch of decimals. do I round? round up? ask lou 
tostring inches_r, gen(length1)
gen metric = season+" at "+length1+" "+"in"
gen units="proportion of catch" 


//egen sum=sum(observed_prob), by(species season ) 
//egen sum1=sum(fitted_prob), by(species season ) 


twoway line observed_prob fitted_prob length if species=="cod", sort by(season, title("Cod catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (cm)")

twoway line observed_prob fitted_prob length if species=="hadd", sort by(season, title("Hadd catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (cm)")


//collapse to inches bins
collapse (sum) observed_prob fitted_prob, by(season species inches_r)
tostring inches_r, gen(length1)
gen metric = season+" at "+length1+" "+"in"
gen units="proportion of catch" 
rename inches_r length 

//now the fitted doesn't look smooth. need to switch to inches in the cal calibration do
twoway line observed_prob fitted_prob length if species=="cod", sort by(season, title("Cod catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (in)")

twoway line observed_prob fitted_prob length if species=="hadd", sort by(season, title("Hadd catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (in)")





//for dashboard

import delimited "$misc_data_cd\baseline_catch_at_length.csv", clear


//observed probability
preserve
collapse (median) observed_prob, by(season species length)
replace length=length/2.54
tostring length, gen(length1)
gen metric = season+" at "+length1+" "+"in"
drop season length
gen units="observed proportion of catch at length" 
rename observed_prob value

tempfile obs
save `obs', replace 
restore

//fitted (smoothed) probability
collapse (median) fitted_prob, by(season species length)
replace length=length/2.54
tostring length, gen(length1)
gen metric = season+" at "+length1+" "+"in"
drop season length
gen units="fitted proportion of catch at length" 
rename fitted_prob value

//stack the fitted and observed probabilities
append using `obs'


// add columns for common, species_itis
gen common = "atlanticcod" if species=="cod"
replace common = "haddock" if species=="hadd"
gen species_itis = 164712 if species=="cod"
replace species_itis = 164744 if species=="hadd"
drop species

// Update this 
gen data_version="2026-06-29"

gen source="model intermediate"
gen stock_abbrev="WGOM"
gen fishery= "NE Groundfish"

//should year go in here?
gen year=2025


order fishery common species_itis stock_abbrev data_version year season metric value units source








