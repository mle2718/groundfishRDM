*********** WGOM COD & HADDOCK CATCH AT LENGTH ***********

/*
This code pulls the median of the 101 draws of simulated mean catch at length for Atlantic Cod and Haddock in the Western Gulf of Maine (WGOM). 

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


import delimited "$misc_data_cd\baseline_catch_at_length_observed.csv", clear


collapse (median) n_fish, by(length season species)
tostring length, gen(length1)
gen metric = length1+" "+"cm"
gen units="number of fish" 

// add columns for common, species_itis
gen common = "atlanticcod" if species=="cod"
replace common = "haddock" if species=="hadd"
gen species_itis = 164712 if species=="cod"
replace species_itis = 164744 if species=="hadd"
drop species


*test





