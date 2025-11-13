


****Set regulations for the calibration period and the projection period****
*These need to be changed every year 

*FY 2026 model
*************************
*Create the baseline regulations for the calibration period, 
*which covers (year==2025 & inlist(wave, 1, 2, 3)) | (year==2024 & inlist(wave, 4, 5, 6)). So FY 2023 regs until May 1, 2024, FY2024 regs after May 1, 2024
gen cod_bag=0 
gen cod_min=100

gen hadd_bag=0
gen hadd_min=100

* 2024 haddock regs were not implemented until july 24th, 2024, so use FY 2023 regs until then
replace hadd_bag=15 if  day>=td(01may2024) & day<=td(23jul2024)  & mode=="fh"
replace hadd_min=18*2.54 if  day>=td(01may2024) & day<=td(23jul2024)  & mode=="fh"

replace hadd_bag=10 if  day>=td(01may2024) & day<=td(23jul2024)  & mode=="pr"
replace hadd_min=17*2.54 if  day>=td(01may2024) & day<=td(23jul2024)  & mode=="pr"

replace hadd_bag=15 if  day>=td(24jul2024) & day<=td(28feb2025) 
replace hadd_min=18*2.54 if  day>=td(24jul2024) & day<=td(28feb2025) 

replace hadd_bag=15 if  day>=td(01apr2025) & day<=td(30apr2025) 
replace hadd_min=18*2.54  if  day>=td(01apr2025) & day<=td(30apr2025) 


* 2024 cod regs were not implemented untuil August 14, 2024 - not open until Septmeber, so no mid season changes. 
replace cod_bag=1 if  day>=td(01sep2024) & day<=td(31oct2024)
replace cod_min=23*2.54 if  day>=td(01sep2024) & day<=td(31oct2024)


tempfile regulations
save `regulations', replace 

*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full

gen day1=day(day_y2)
gen month1=month(day_y2)
gen year_y2=year(day_y2)
drop if day_y2==$leap_yr_days
gen dow_y2 = dow(day_y2)  

gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		
replace kod_y2="we" if $fed_holidays_y2

gen month2_y2= string(month1,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup


merge 1:m  mode day1 month1 using `regulations'
drop if day==$leap_yr_days
drop _merge 
order year mode month kod dow day  draw cod_bag cod_min hadd_bag hadd_min day_y2 dow_y2 kod_y2 month_y2
sort  mode day draw



*************************
*Create status-quo regualtions for projection period here: 01may2025  -  td(30apr2026)
gen cod_bag_y2_same=cod_bag
gen cod_min_y2_same=cod_min

gen hadd_bag_y2_same=hadd_bag
gen hadd_min_y2_same=hadd_min

gen cod_bag_y2_alt=cod_bag
replace  cod_bag_y2_alt=1 if day_y2>=td(01may2026) & day_y2<=td(31may2026)

gen cod_min_y2_alt=cod_min
replace  cod_min_y2_alt=23*2.54 if day_y2>=td(01may2026) & day_y2<=td(31may2026)

gen hadd_bag_y2_alt=hadd_bag
gen hadd_min_y2_alt=17*2.54
*************************

