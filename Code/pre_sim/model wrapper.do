

/****RDM input code wrapper****/

*** Groundfish ***

*************************
****Data availability****
*************************

*We make projections for the next fishing year in Dec/Jan. 

*MRIP catch and effort data: most recent 6 waves of MRIP data. 
		*For FY 2026 projections we use MRIP data from 2025 waves1-5 and 2024 wave 6 to compute catch-per-trip, directed trips, and catch-at-length 

*MRIP-only length data: last full calender year, to align with stock assessment data 

				
*Stock assessment projections data:
		*Jan 1 2024 NAA to compute historical rec. selectivity ***CHECK
		*Jan 1 2026 NAA to compute projected catch-at-length ***CHECK
		
*NEFSC trawl survey data from 2024, 2023, 2022 years used to create age-length keys
		*CHECK

*MRIP data is stored in  
	*"smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	*Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\

* Dependencies
*ssc install xsvmat 
*ssc install gammafit 



**************************************************ADJUST GLOBALS************************************************** 	
* these need to be changed every year 
global calibration_year "(year==2025 & inlist(wave, 1, 2, 3)) | (year==2024 & inlist(wave, 4, 5, 6))"  // last six waves of data 

global calibration_date_start td(01jul2024)
global calibration_date_end td(30jun2025)

global projection_date_start td(01may2026)
global projection_date_end td(30apr2027)

* add federal holidays, as these are considered "weekend" days by the MRIP and we need to account for this when estimating fishing effort at the month and kind-of-day level

/*
Federal Holidays Included:
New Year's Day: January 1 (observed on January 2 if it falls on Sunday).
Martin Luther King Jr. Day: Third Monday in January.
Presidents' Day: Third Monday in February.
Memorial Day: Last Monday in May.
Juneteenth National Independence Day: June 19.
Independence Day: July 4 (observed on July 5 if it falls on Sunday).
Labor Day: First Monday in September.
Columbus Day: Second Monday in October.
Veterans Day: November 11 (observed on November 10 if it falls on Sunday).
Thanksgiving Day: Fourth Thursday in November.
Christmas Day: December 25 (observed on December 26 if it falls on Sunday).
*/

* fed holidays in the calibration year 
global fed_holidays "inlist(day, td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024), td(01jan2025), td(20jan2025), td(17feb2025), td(26may2025), td(19jun2025))" 

* fed holidays in the projection year 
global fed_holidays_y2 "inlist(day1, td(25may2026), td(19jun2026), td(03jul2026), td(07sep2026), td(12oct2026), td(11nov2026),  td(26nov2026),  td(25dec2026), td(01jan2027), td(18jan2027), td(15feb2027))"

* leap-year days here
global leap_yr_days "td(29feb2024)" 

* choose number of draws to create. Will ultimately select ~100 for final model
global ndraws 100

* adjust 2022 survey trip costs to account for inflation (January 2022 - January 2025)
* source =https://www.bls.gov/data/inflation_calculator.htm
global inflation_expansion=1.13

* adjust project paths based on user
global project_path "C:\Users\andrew.carr-harris\Desktop\Git\groundfishRDM" /* Lou's project path */
global input_data_cd "E:\Lou_projects\groundfishRDM\input_data" /* Lou's local data path */
global input_code_cd "C:\Users\andrew.carr-harris\Desktop\Git\groundfishRDM\Code\pre_sim"
global iterative_input_data_cd "E:\Lou_projects\groundfishRDM\process_data"
global figure_cd  "E:\Lou_projects\groundfishRDM\figures"

* set a global seed #
global seed 03211990

* years/waves of MRIP data. 
global yr_wvs 20231 20232 20233 20234 20235 20236  ///
					 20241 20242 20243 20244 20245 20246  ///
					 20251 20252 20253 20254 20255 20256
					 
global yearlist 2023 2024 2025
global wavelist 1 2 3 4 5 6


**************************************************Model calibration ************************************************** 
// 1) Pull the MRIP data
do "$input_code_cd\MRIP data wrapper.do"


// 2) Estimate directed trips at the month, mode, kind-of day level
do "$input_code_cd\directed_trips_calibration.do"
		*This file calls "set regulations.do". In it you must enter the SQ regulations in the calibration and projection year. 
		*THIS NEEDS TO BE ADJUSTED EVERY YEAR. 


// 3) Create distirbutions of costs per trip across strata
do "$input_code_cd\survey trip costs.do"


// 4) Estimate catch-per-trip at the month and mode level
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		do "$input_code_cd\calibration_catch_per_trip_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		* run copula_modeling_calibration.R
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\calibration_catch_per_trip_part2.do"


// 5) compare calibration output to MRIP, and retain total simulated harvest and discards to apply to the baseline catch-at-length distribution
		do "$input_code_cd\compare calibration data to MRIP.do" 

// 6) add additonal angler demographics based on results of utilty model
		do "$input_code_cd\additional_angler_dems.do" 


// 7) Generate baseline-year catch-at-length, using the simulated harvest/discard totals from step 5
		do "$input_code_cd\catch_at_length_calibration.do"

// At this point, the calibration routine can be run in R. 		


**************************************************Model projection ************************************************** 
		
// 8) Generate projection-year catch-at-length, incorporating the stock assessment data
		do "$input_code_cd\catch_at_length_projection.do"

		
// 9)  Estimate projected catch-per-trips at the month and mode level
		 *will use MRIP catch data from the last two full years. 
		 *For 2026 mgt. cycle: 2025 waves 1-4, 2024 waves 1-6, 2023 waves 5 & 6	 
		 
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		 do "$input_code_cd\catch_per_trip_projection_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		* run copula_model_loop_projection.R
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\catch_per_trip_projection_part2.do"
		
		//d) compare estimates of mean projected catch to MRIP data to ensure consistency and remove extraneous columns from projected catch draw data
		do "$input_code_cd\compare projection catch to MRIP.do"
		

// Steps 7-10 are not necessary to run. They compare the disaggregated simulated catch and effort data to aggreagte MRIP estimates, and compute catch weight totals in the calibration year 


// 10) Estimate total weight of harvest and release in calibration year, compare to simulation model output
	*a)  compute harvest and discard-at-length from MRIP data (by age-weight equation season: Jan-Jun and July-Dec, and for the whole year)
	
		*do "$input_code_cd\raw_props_at_length_ab1b2_calibration.do"

	*b) compute total harvest and discards from MRIP data (by age-weight equation season: Jan-Jun and July-Dec, and for the whole year)
	
		*do "$input_code_cd\catch_totals_calibration_month1.do"
	
	*c) compute the total mortality weight in the calibration year based on MRIP data and compare the simulation model output. 
		*note that the calibration model must be run in R first, and in calibration_wrapper.R, you must uncomment:
			*1) the line #source(paste0(code_cd, "calibration_catch_weights2.R"))
			*2) the last code chunk in calibration_wrapper.R  

		*datasets needed:
			*Discard mortality rates for each species by month and size-class: $input_data_cd\Discard_Mortality.csv
			*Rates are the same for cod across month and size class, but differ for haddock
			*R code output file: calibration_catch_weights_cm.xlsx
			
		*do "$input_code_cd\compute catch weights calibration.do" - this file requires the 










