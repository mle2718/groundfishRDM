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

//raw observed numbers released at length on headboats and observed harvest at length from MRIP
u "$misc_data_cd\rdb_raw_cat_len.dta", clear 

rename l_cm_bin length
rename nfish_ab1 harvest 
rename nfish_b2 discards

replace season="summer" if season=="sum"
replace season="winter" if season=="win"

replace harvest = 0 if missing(harvest)
replace discards = 0 if missing(discards)

sort species season length

//harvets and discards in same plot doesn't work
twoway line harvest discards length if species=="cod", sort by(season, title("Raw cod harvest & discards at length, #'s of fish")) legend(label(1 "Harvest") label(2 "Discards")) xtitle("Length (cm)")
twoway line harvest discards length if species=="hadd", sort by(season, title("Raw hadd harvest & discards at length, #'s of fish")) legend(label(1 "Harvest") label(2 "Discards")) xtitle("Length (cm)")

//cod
twoway line discards length if species=="cod", sort by(season, title("Raw cod discards at length, #'s of fish")) xtitle("Length (cm)")
twoway line harvest length if species=="cod", sort by(season, title("Raw cod harvest at length, #'s of fish")) xtitle("Length (cm)")

//hadd
twoway line discards length if species=="hadd", sort by(season, title("Raw hadd discards at length, #'s of fish")) xtitle("Length (cm)")
twoway line harvest length if species=="hadd", sort by(season, title("Raw hadd harvest at length, #'s of fish")) xtitle("Length (cm)")


//
egen har_total=sum(harvest), by(species season) 
egen disc_total=sum(discards), by(species season) 
gen h_prop=harvest/har_total
gen d_prop=discards/disc_total
twoway line h_prop d_prop length if species=="cod" & season=="summer"

twoway line harvest discards length if species=="cod" & season=="summer"
twoway line harvest discards length if species=="cod" & season=="winter"
twoway line harvest discards length if species=="hadd" & season=="summer"
twoway line harvest discards length if species=="hadd" & season=="winter"




//median numbers of fish harvested, discarded, and caught at length. proportions are multiplied by 101 draws of total harvest and total discards
u "$misc_data_cd\rdb_cat_len.dta", clear // file for processing catch at at length for dashboard

rename l_cm_bin length
rename n_ab1 harvest 
rename n_b2 discards
rename n_fish catch

collapse (median) harvest discards catch, by(season species length)
tostring length, gen(length1)
gen metric = length1+" "+"cm"
gen units="number of fish"

//all 3
twoway line harvest discards catch length if species=="cod", sort by(season, title("Cod catch at length numbers of fish")) legend(label(1 "Harvest") label(2 "Discards") label(3 "Catch")) xtitle("Length (cm)")

twoway line harvest discards catch length if species=="hadd", sort by(season, title("Hadd catch at length numbers of fish")) legend(label(1 "Harvest") label(2 "Discards") label(3 "Catch")) xtitle("Length (cm)")

//harvest discards only
twoway line harvest discards length if species=="cod", sort by(season, title("Cod harvest&discards at length numbers of fish")) legend(label(1 "Harvest") label(2 "Discards")) xtitle("Length (cm)")

twoway line harvest discards length if species=="hadd", sort by(season, title("Hadd harvest&discards at length numbers of fish")) legend(label(1 "Harvest") label(2 "Discards")) xtitle("Length (cm)")

//catch only
twoway line catch length if species=="cod", sort by(season, title("Cod catch at length numbers of fish")) xtitle("Length (cm)")

twoway line catch length if species=="hadd", sort by(season, title("Hadd catch at length numbers of fish")) xtitle("Length (cm)")



//old
twoway line harvest discards length if species=="cod" & season=="summer"
twoway line harvest discards length if species=="cod" & season=="winter"
twoway line harvest discards length if species=="hadd" & season=="summer"
twoway line harvest discards length if species=="hadd" & season=="winter"





//median probability catch at length - and has probabilities for harvest and discards at length (constant across draws)
u "$misc_data_cd\rdb_cat_len.dta", clear 

//probabilities for catch at length
gen tot_cat=ab1+b2
gen prop_cal=n_fish/tot_cat

rename l_cm_bin length

collapse (median) prop_cal prop_ab1 prop_b2, by(season species length)
tostring length, gen(length1)
gen metric = length1+" "+"cm"
gen units="proportion of catch (observed)" 

twoway line prop_ab1 prop_b2 prop_cal length if species=="cod", sort by(season, title("Cod catch at length proportions")) legend(label(1 "Harvest") label(2 "Discards") label(3 "Catch")) xtitle("Length (cm)")

twoway line prop_ab1 prop_b2 prop_cal length if species=="hadd", sort by(season, title("Hadd catch at length proportions")) legend(label(1 "Harvest") label(2 "Discards") label(3 "Catch")) xtitle("Length (cm)")


egen sum=sum(prop_cal), by(species season ) 

/// delete
twoway line prop_ab1 prop_b2 length if species=="cod" & season=="summer"
twoway line prop_cal length if species=="cod" & season=="summer"
twoway line prop_ab1 prop_b2 prop_cal length if species=="cod" & season=="summer"
twoway line prop_ab1 prop_b2 prop_cal length if species=="cod" & season=="winter"
twoway line prop_ab1 prop_b2 prop_cal length if species=="hadd" & season=="summer"
twoway line prop_ab1 prop_b2 prop_cal length if species=="hadd" & season=="winter"

/// delete 
rename prop_ab1 harvest
rename prop_b2 discards
//it's bad for cod summer bc over 80% of harvest is one length
twoway line harvest discards length if species=="cod" & season=="summer"
twoway line harvest discards length if species=="cod" & season=="winter"
twoway line harvest discards length if species=="hadd" & season=="summer"
twoway line harvest discards length if species=="hadd" & season=="winter"





//medians of the observed and fitted probabilities for catch at length (doesn't have harvest and discards at length)
// observed is same as if you generated from rdb_cat_len.dta like above but this one trims the rows at the ends of the length distribution that are 0's
import delimited "$misc_data_cd\baseline_catch_at_length.csv", clear

collapse (median) observed_prob fitted_prob, by(season species length)
tostring length, gen(length1)
gen metric = length1+" "+"cm"
gen units="proportion of catch" 

egen sum=sum(observed_prob), by(species season ) 
egen sum1=sum(fitted_prob), by(species season ) 

* delete these
//twoway line observed_prob length if species=="cod" & season=="summer"
//twoway line observed_prob fitted_prob length if species=="cod" & season=="summer"


twoway line observed_prob fitted_prob length if species=="cod", sort by(season, title("Cod catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (cm)")

twoway line observed_prob fitted_prob length if species=="hadd", sort by(season, title("Hadd catch at length")) legend( label(1 "Observed") label(2 "Fitted")) xtitle("Length (cm)")








//stuff from thurs. has some of the dashboard columns
//import delimited "$misc_data_cd\baseline_catch_at_length_observed.csv", clear


collapse (median) n_fish, by(length season species)
tostring length, gen(length1)
gen metric = length1+" "+"cm"
gen units="number of fish" 
rename n_fish value


// add columns for common, species_itis
gen common = "atlanticcod" if species=="cod"
replace common = "haddock" if species=="hadd"
gen species_itis = 164712 if species=="cod"
replace species_itis = 164744 if species=="hadd"
drop species

graph bar value if common=="atlanticcod", over(length) by(season) ytitle("number of fish")  scheme(stmono1) 

graph bar value if common=="haddock", over(length) ytitle("number of fish")  scheme(stmono1) 


//Update this 
gen data_version="2026-06-29"

gen wave=.
gen state=.
gen mode=.
gen year=.
gen month=.
gen source="model intermediate"
gen stock_abbrev="WGOM"
gen fishery= "NE Groundfish"


drop length length1

order fishery common species_itis stock_abbrev state mode data_version year season wave month metric value units source





