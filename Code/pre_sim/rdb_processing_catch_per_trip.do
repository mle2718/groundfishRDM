

/*This code cleans the raw MRIP catch per trip data compiled during Part A of rdb_calibration_catch_per_trip_part1.do to get the data in a usable format for the rec dashboard
 
(this code could be here in a separate do file or tacked on to the end of Part A in rdb_calibration_catch_per_trip_part1.do)
(will decide later where it should go)
	
*/




cd $misc_data_cd

use baseline_mrip_catch_processed.dta, clear

keep my_dom_id_string meancod_cat meanhadd_cat strat_id psu_id id_code year wp_int common_dom cod_cat hadd_cat

// next, parse out the month and the mode from my_dom_id_string
//don't actually need meancod_cat meanhadd_cat 
//make a wave variable?? would that mess things up?
//do we now need a column in dashboard dataframe for month? I think so

//put NE or new england for the state?
//fishery   is "NE Groundfish"
//common 
//species_itis
//stock_abbrev is WGOM
//state  will be NA or New England. Ask. Start an issue
//mode will be for hire or private
//data version. hm I think lou's dta's are pretty old and we need to pull in updated MRIP data and rerun the data wrapper
// ask him because the google folder says he saved them on May 12. Here: https://drive.google.com/drive/folders/1wIlpn5Q8_iBnZ0NUlKVVpzyI7x97zAdi   Ask lou if he pulled in updated MRIP data that day

//year
//wave - see comment above. Insert a month column after wave? then you need to go back to trips and catch and add a month column with NA's
//metric will be caught 1 fish?  ...
//value
//units will be trips (or number of trips)


//



