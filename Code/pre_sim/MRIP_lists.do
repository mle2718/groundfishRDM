/*catchlist -- this assembles names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "$misc_data_cd/catch_`year'`wave'.dta"
	if _rc==0{
		use "$misc_data_cd/catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "$misc_data_cd/catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year in $yearlist{
	foreach wave in  $wavelist{
	capture confirm file "$misc_data_cd/trip_`year'`wave'.dta"
	if _rc==0{
		use "$misc_data_cd/trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "$misc_data_cd/trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "$misc_data_cd/size_b2_`year'`wave'.dta"
	if _rc==0{
		use "$misc_data_cd/size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "$misc_data_cd/size_b2_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}


/*SIZE_LIST */
global sizelist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "$misc_data_cd/size_`year'`wave'.dta"
	if _rc==0{
	use "$misc_data_cd/size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "$misc_data_cd/size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}
