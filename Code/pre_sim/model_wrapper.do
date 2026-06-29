/**** Groundfish RDM input code wrapper ****/


/* This uses the user written command here to set directories*/
/* It is not as good as R's version. Before running this code, you must change directories into project directory 
One easy way to do this is to add a line to your profile do that store that directory in the 
global groundfishRDMdir "path to this project"
and then cd "$groundfishRDMdir" right before running this code

*/

**Data availability**

* We make projections for the next fishing year in Dec/Jan. 

* MRIP catch, effort, and length data: most recent 6 waves of MRIP data. 

*Stock assessment projections data:
		*Jan 1 2024 NAA to compute historical rec. selectivity ***CHECK
		*Jan 1 2026 NAA to compute projected catch-at-length ***CHECK
		
*NEFSC trawl survey data from 2024, 2023, 2022 years used to create age-length keys
		*CHECK

*MRIP data is stored in  
	*"smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	*Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\

* Dependencies
* ssc install xsvmat 
* ssc install gammafit 
* ssc install grc1leg
* ssc install rscript

set varabbrev on

**Set globals **
* these need to be changed every year 

/*Original data years*/
*global calibration_year "(year==2025 & inlist(wave, 1, 2, 3, 4)) | (year==2024 & inlist(wave, 5, 6))"  // last six waves of data 
*global calibration_date_start td(01sep2024)
*global calibration_date_end td(31aug2025)

/*Updated data years*/
global calibration_year "(year==2025 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2024 & inlist(wave, 6))"  // last six waves of data  updated
global calibration_date_start td(01nov2024)
global calibration_date_end td(31oct2025)

global projection_date_start td(01may2026)
global projection_date_end td(30apr2027)

* add federal holidays, as these are considered "weekend" days by the MRIP and we need to account for this when estimating fishing effort at the month and kind-of-day level

* fed holidays in the calibration year 
global fed_holidays "inlist(day, td(11nov2024), td(28nov2024), td(25dec2024), td(01jan2025), td(20jan2025), td(17feb2025), td(26may2025), td(19jun2025), td(04jul2025), td(01sep2025), td(13oct2025))" 

* fed holidays in the projection year 
global fed_holidays_y2 "inlist(day1, td(25may2026), td(19jun2026), td(03jul2026), td(07sep2026), td(12oct2026), td(11nov2026),  td(26nov2026),  td(25dec2026), td(01jan2027), td(18jan2027), td(15feb2027))"

* leap-year days here
global leap_yr_days "td(29feb2024)" 

* set number of model iterations to create
global ndraws 101

* adjust 2022 survey trip costs to account for inflation (January 2022 - January 2025)
* source =https://www.bls.gov/data/inflation_calculator.htm
global inflation_expansion=1.13



/* find the root of the project 
prior to running the wrapper, you must change to $groundfishRDMdir so here picks up the project
*/
here, nogit

do "${here}/Code/helpers/user_setup_stata.do"

* adjust project paths based on user
global input_code_cd "${here}/Code/pre_sim" 
global misc_data_cd "${gfdatadir}/miscellaneous" 
global calib_catch_draws_cd "${gfdatadir}/calib_catch_draws" 
global figure_cd  "${gfdatadir}/figures" 

global log_dir "${input_code_cd}/logs" 

/* make directories if necessary */
capture mkdir $misc_data_cd
capture mkdir $calib_catch_draws_cd
capture mkdir $figure_cd
capture mkdir $log_dir

timer clear 1        // Resets timer #1
timer on 1           // Starts timing

/* start log */
cap log close
log using "${log_dir}\model_wrapper_log_$S_DATE.smcl", replace


* set a global seed #
global seed 03211990

* years/waves of MRIP data. 
global yr_wvs 20231 20232 20233 20234 20235 20236  ///
			  20241 20242 20243 20244 20245 20246  ///
			  20251 20252 20253 20254 20255 20256
					 
global yearlist 2023 2024 2025
global wavelist 1 2 3 4 5 6

* set the baseline year and projection year numbers-at-age globals 
global cod_NAA_base_year 2025  
global hadd_NAA_base_year 2025
global cod_NAA_proj_year 2026
global hadd_NAA_proj_year 2026

* set the starting year for the NEFSC trawl survey data pull (in catch_at_length_projection.do)
	* we aggregate these data across multiple years and use them to create age-length keys  
	* I usually check how many observations are available across different choices of the starting year; we want sufficient data 
	* but do not want to use historical data too far in the past. 
	
global trawl_survey_start_year 2022



**********************************************************************
************************ EXECUTION CONTROL ***************************
**********************************************************************

// Control which modules to run (set to 0 to skip)
loc pull_assessment = 1		 		// Pull Assessment data
loc processMRIP = 1		 			// deal with casing MRIP data
loc assemblemriplists = 1		 	// deal with casing MRIP data

loc estimate_dtrips = 1				// Estimate Directed Trips 
loc costs_per_trip = 1  			// Create Distributions of costs per trip (run 1x)
loc draw_angler_preferences = 1		// Create draw of angler preference parameters (run 1x)
loc catch_per_trip1 = 1				// Part 1 of catch per trip
loc copula_in_R = 1					// Copula model in R
loc catch_per_trip2 = 1				// Part 2 of catch per trip
loc compare_calibration_MRIP = 1	// compare calibration output to MRIP
loc prep_cpt_for_dashboard= 1		// prep data for dashboard
loc Rpush_to_gdrive =1 				// Push to google drive in R
loc angler_demogs	=1				// add additonal angler demographics
loc generate_baseline=1				// Generate baseline-year catch-at-length
loc catch_at_length_project=1			// Generate projection-year catch-at-length
loc run_calibration=1						// Run calibration routine in R



// Prototyping
local proto = 0

if `proto' {
	global ndraws 3
}

**************************************************Model calibration ************************************************** 

// 0) Pull Assessment data from google.

/* This code requires you to mount your google drive to D on your computer */
if `pull_assessment' {
	di "Pulling Assessment data from google"

	do "$input_code_cd\get_assessment_from_gdrive.do"
}



// 1) Pull the MRIP data


if `processMRIP' {
	di "Processing MRIP data"

	do "$input_code_cd\MRIP_column_cases.do"
	di "MRIP data processed"
}

if `assemblemriplists' {
	di "Assembling Lists of MRIP files"

	do "$input_code_cd\MRIP_lists.do"
	di "Lists of MRIP files assembled"

}


// 2) Estimate directed trips at the month, mode, kind-of day level

if `estimate_dtrips' {
	di "Estimating Directed trips"
	*This file calls "set_regulations.do". In it you must enter the SQ regulations in the calibration and projection year. 
	*THIS NEEDS TO BE ADJUSTED EVERY YEAR. 

	do "$input_code_cd\directed_trips_calibration.do"
	di "Directed trips Estimated"

}


// 3) Create distributions of costs per trip across strata - only needs to be run once
if `costs_per_trip' {
	di "Creating distributions of cost per trip"

	do "$input_code_cd\survey_trip_costs.do"
	di "distributions of cost per trip Done"

}
// 4) Create draw of angler preference parameters - only needs to be run once
if `draw_angler_preferences' {
	di "Creating draws of angler preference parameters"
	do "$input_code_cd\estimate_angler_preferences.do" 
	di "Draws of angler preference parameters Done"

}
// 5) Estimate catch-per-trip at the month and mode level
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
if `catch_per_trip1' {
	di "Estimate catch-per-trip at the month and mode level"

	do "$input_code_cd\calibration_catch_per_trip_part1.do"
	di "catch-per-trip at the month and mode level Done"

}
		//b) use copula model (in R) to simulate harvest and discards per-trip
if `copula_in_R' {
	 /* this takes a while and will look like it's hung. it's not */
    	di "Estimating copula in R. This takes a while and will look like it's hung"

		rscript using "$input_code_cd\copula_modeling_calibration.R"
    	di "Copula in R estimated"

}		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
if `catch_per_trip2' {
    	di "Generating estimates of simulated total harvest based on random draws"

		do "$input_code_cd\calibration_catch_per_trip_part2.do"
    	di "Estimates of simulated total harvest Done"

	}
// 6) compare calibration output to MRIP, and retain total simulated harvest and discards to apply to the baseline catch-at-length distribution
if `compare_calibration_MRIP' {
    	di "Comparing calibration output to MRIP"
		cd $here

		do "$input_code_cd\compare_calibration_data_to_MRIP.do" 
    	di "Comparison of calibration output to MRIP done"

	}		
// 7) Process catch-per-trip and format it for the rec dashboard
if `prep_cpt_for_dashboard'{
    	di "Processing and formatting catch-per-trip for dashboard"

		do "$input_code_cd\rdb_processing_catch_per_trip.do"
    	di "Processing and formatting catch-per-trip for dashboard done"

		}
		//run this script in R to read in the catch per trip processed for the rec dashboard, save it as an Rds, and push it to Google Drive
if `Rpush_to_gdrive'{
    	di "Pushing rec dashboard data to gdrive using R" 

		rscript using "$input_code_cd\rdb_catch_per_trip_to_drive.R"
	    di "Rec dashboard data pushed to gdrive " 

}
// 8) add additonal angler demographics based on results of utilty model
if `angler_demogs'{
    	di "Adding additional angler demographics" 

		do "$input_code_cd\additional_angler_dems.do" 
    	di "Additional angler demographics done" 

		}
// 9) Generate baseline-year catch-at-length, using the simulated harvest/discard totals from step 5
if `generate_baseline'{
    	di "Generating baseline catch-at-length" 

		do "$input_code_cd\catch_at_length_calibration.do"
    	di "Baseline catch-at-length generated " 

		}
// 10) Generate projection-year catch-at-length, incorporating the stock assessment data
if `catch_at_length_project'{
		di "Generating projection year catch-at-length" 

		do "$input_code_cd\catch_at_length_projection.do"
    	di "Projection year catch-at-length generated " 

		}
di "The calibration and projection routines can now be run in R " 
// 11) Run the calibration routine in R, export files to Google Drive
if `run_calibration'{
		di "Running calibration routine in R" 
	cd $here

		rscript using "$here\Code\sim\R code wrapper.R" 
    	di "Simulation model calibrated and files exported to Google Drive" 

		}


log close


timer off 1          // Stops timing
timer list 1         // Displays elapsed time in seconds