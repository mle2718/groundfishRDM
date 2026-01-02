
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

global calibration_year "(year==2025 & inlist(wave, 1, 2, 3, 4)) | (year==2024 & inlist(wave, 5, 6))"  // last six waves of data 

// Pull the MRIP data for comparison with model output
do "$input_code_cd\MRIP data wrapper.do"



* This file computes predictions of harvest in weight in 2026 under status quo regulations

cd "C:\Users\andrew.carr-harris\Desktop\Git\groundfishRDM\Code\test_code"
import delimited using "cod_hadd_SQ_output_12_31.csv", clear


*Coastwide predictions
format value %12.02gc

/*
FY2026 sub-ACLs:

GOM haddock – 1,146 mt

WGOM cod – 118 mt
*/

* create a total catch statistic
preserve
keep if inlist(metric, "keep_numbers", "release_numbers")
collapse (sum) value, by(species draw mode)
gen metric="catch_numbers"
tempfile catch
save `catch', replace
restore
append using `catch'
levelsof metric

preserve
keep if  metric=="removals_weight" & mode=="all modes"
replace value=value/2205 if strmatch(metric, "*weight*")==1
 vioplot value if metric=="removals_weight" & mode=="all modes", over(species) yline(118 1146) ylab(0(100)1200) ytitle("total removals (mt)")
restore
	
gen domain=species+"_"+metric+"_"+mode

tempfile base
save `base', replace

levelsof domain, local(doms)

clear
tempfile ptiles
save `ptiles', emptyok

foreach d of local doms{
u `base', clear 

centile value if domain=="`d'", centile(2.5 5 50 95 97.5)

local p2_5    = r(c_1)
local p5   = r(c_2)
local p50   = r(c_3)
local p95   = r(c_4)
local p97_5   = r(c_5)

su value if domain=="`d'" & value>`p2_5' & value<`p97_5'
local lb95=`r(min)'
local ub95=`r(max)'

su value if domain=="`d'" & value>`p5' & value<`p95'
local lb90=`r(min)'
local ub90=`r(max)'

clear
set obs 1
gen domain="`d'"
gen lb95=`p5'
gen ub95=`ub95'
gen p50=`p50'
gen lb90=`lb90'
gen ub90 = `ub90'

append using `ptiles'
save `ptiles', replace
}

use `ptiles', clear

split domain, parse(_)
rename domain1 species
gen metric= domain2+"_"+domain3 if domain4!=""
replace metric=domain2 if domain4==""
rename domain4 mode
replace mode=domain3 if mode==""

drop domain2 domain3  domain
order species  metric mode lb95 lb90 p50 ub90 ub95

save `base', replace


* Pull in MRIP data from FY 2024 for comparison 
* coastwide estimates

* Estimates by mode
cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")


* Format MRIP data for estimation 

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

* ensure only relevant states/year
keep if inlist(st, 23, 33, 25)
keep if ((year==2024 & inlist(wave, 3, 4, 5, 6)) | (year==2025 & inlist(wave, 1, 2)))  //FY 2024

*keep if  $calibration_year
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

*drop shore trips
drop if mode1=="sh"

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 

*New MRIP site allocations
preserve 
import delimited using "$input_data_cd/MRIP_COD_ALL_SITE_LIST.csv", clear 
keep if inlist(state, "MA", "ME")
keep state intsite nmfs_stock_area nmfs_stat_area
sort intsite nmfs_stock_area  
replace nmfs_stock_area="WGOM" if inlist(nmfs_stat_area, 521, 526, 541, 514, 513, 515)
replace nmfs_stock_area="XX" if !inlist(nmfs_stat_area, 521, 526, 541, 514, 513, 515)
keep nmfs_stock_area intsite nmfs_stat_area state
duplicates drop
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite state using `mrip_sites',  keep(1 3)

/*classify into WGOM or not WGOM */
gen str3 area_s="XX"
replace area_s="WGOM" if st2=="33"
replace area_s=nmfs_stock_area if inlist(st2, "25", "23") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=area_s+"_"+mode1+"_"+common_dom

* Define the list of species to process
local species "atlanticcod haddock"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "atlanticcod" local short "cod"
    if "`s'" == "haddock"     local short "hadd"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)

    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
	
	gen `short'_wgt_harvest = wgt_ab1*2.20462 if common == "`s'" //translate kg's to pounds
    egen sum_`short'_wgt_harvest = sum(`short'_wgt_harvest), by(strat_id psu_id id_code)
}

rename sum_cod_tot_cat cod_cat
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_hadd_tot_cat hadd_cat
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel
rename sum_cod_wgt_harvest cod_keepwt
rename sum_hadd_wgt_harvest hadd_keepwt

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="ATLCO"
keep if area_s=="WGOM"

replace my_dom_id_string=mode1+"_"+common_dom

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile basefile
save `basefile', replace 


* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float total se ll95 ul95 using `results', replace

* Loop over variables
foreach var in cod_keep cod_rel cod_cat hadd_keep hadd_rel hadd_cat cod_keepwt hadd_keepwt {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname total  my_dom_id_string ll95 ul95

split varname, parse(_)
rename varname1 species
rename varname2 metric
split my, parse(_)
drop my_dom_id_string2
rename my_dom_id_string1 mode
replace metric="catch_numbers" if metric=="cat"
replace metric="keep_numbers" if metric=="keep"
replace metric="keep_weight" if metric=="keepwt"
replace metric="release_numbers" if metric=="rel"
drop my varname 
gen source="MRIP FY 2024"
rename total value
rename value p50 
rename ll95 lb95
rename ul95 ub95

*** End MRIP data pull

* combine MRIP and simulation output
append using `base'
replace source="RDM FY 2026" if source==""
order species metric mode source 
encode source, gen(source2)

gen domain=species+"_"+mode+"_"+metric
replace domain=mode+"_"+metric if inlist(metric, "CV") | strmatch(metric, "*trips")==1

/*
replace value=p50 if source=="RDM 2026"
gen ll80=p10 if source=="RDM 2026" 
gen ul80=p90 if source=="RDM 2026" 
gen ll90=p5 if source=="RDM 2026" 
gen ul90=p95 if source=="RDM 2026" 

replace ll80=ll if source=="MRIP 2024" 
replace ul80=ul if source=="MRIP 2024" 
replace ll90=ll if source=="MRIP 2024" 
replace ul90=ul if source=="MRIP 2024" 
*/
replace source2=source2+2

generate source2_90 = source2 - 0.1
generate source2_95 = source2 + 0.1

replace source2_90=source2 if source=="MRIP FY 2024"  
replace source2_95=source2 if source=="MRIP FY 2024"  


local vars p50 lb90 ub90 lb95 ub95
foreach v of local vars{
	format  `v' %14.0gc
	replace `v'=`v'/2205 if strmatch(metric, "*weight*")==1
	replace `v'=`v'/1000 if strmatch(metric, "*numbers*")==1 | strmatch(metric, "*number*")==1
	replace `v'=`v'/1000000 if strmatch(metric, "*CV*")==1

}
/*
local vars p5 p10 p50 p90 p95
foreach v of local vars{
	format  `v' %14.0gc
	replace `v'=`v'/2205 if strmatch(metric, "*weight*")==1
	replace `v'=`v'/1000 if strmatch(metric, "*numbers*")==1 | strmatch(metric, "*number*")==1
	replace `v'=`v'/1000000 if strmatch(metric, "*CV*")==1

}
*/
*cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\rdm testing data\SQ_runs_10_24"


/*
FY2026 sub-ACLs:
GOM haddock – 1,146 mt
WGOM cod – 118 mt
*/

*harvest based on age-length weights


gr drop _all 
levelsof domain if inlist(metric, "keep_weight") & mode!="all modes",  local(doms)
foreach d of local doms{

levelsof species if dom=="`d'"
if `r(levels)'=="cod"{
	local sp="Cod"
}
if `r(levels)'=="hadd"{
	local sp="Haddock"
}

levelsof mode if dom=="`d'"
if `r(levels)'=="fh"{
	local md="for-hire"
}
if `r(levels)'=="pr"{
	local md="private boat"
}

twoway  (rcap lb90 ub90 source2_90  if domain=="`d'", lcolor(navy)) ///
			 (rcap lb95 ub95 source2_95 if domain=="`d'", lcolor(blue)) ///
			(scatter p50 source2_90 if domain=="`d'", mcolor(black) msymbol(O) msize(small) ) ///
			(scatter p50 source2_95 if domain=="`d'", msymbol(O) msize(small)  mcolor(black) title("`sp', `md'", size(medium))  name(`d', replace) ///
			 xlab(1 " " 2 "MRIP 2024" 3 "RDM 2026" 4 " ", noticks labsize(small)  ) xtitle("") note("") ytitle("", size(small)) ylab(,labsize(small)) ///
			 legend(order(1 "90% CI" 2 "95% CI" ) pos(6) rows(1) size(small) region(lstyle(none))) )
			

 }
 
grc1leg cod_fh_keep_weight hadd_fh_keep_weight cod_pr_keep_weight hadd_pr_keep_weight, rows(2) title("Coastwide harvest (metric tons)",size(medium))  ///
	note("MRIP 2024: survey-weighted estimate and SE-based CIs" "RDM 2026: median and percentile-based CIs" , size(small) yoffset(-2)) 
graph export coatswide_harvest_wt.jpg, width(1024) height(768) replace

*harvest numbers 
gr drop _all 

levelsof domain if inlist(metric, "keep_numbers"),  local(doms)
foreach d of local doms{


levelsof species if dom=="`d'"
if `r(levels)'=="bsb"{
	local sp="Black sea bass"
}
if `r(levels)'=="sf"{
	local sp="Summer flounder"
}
if `r(levels)'=="scup"{
	local sp="Scup"
}

twoway  (rcap ll80 ul80 source2_80  if domain=="`d'", lcolor(navy)) ///
			 (rcap ll90 ul90 source2_90 if domain=="`d'", lcolor(blue)) ///
			(scatter value source2_80 if domain=="`d'", mcolor(black) msymbol(O) msize(small) ) ///
			(scatter value source2_90 if domain=="`d'", msymbol(O) msize(small)  mcolor(black) title("`sp'", size(medium))  name(`d', replace) ///
			 xlab(1 " " 2 "MRIP 2024" 3 "RDM 2026" 4 " ", noticks labsize(small)  ) xtitle("") note("") ytitle("", size(small)) ylab(,labsize(small)) ///
			 legend(order(1 "80% CI" 2 "90% CI" ) pos(6) rows(1) size(small) region(lstyle(none))) )
			

 }
grc1leg sf_keep_numbers bsb_keep_numbers scup_keep_numbers, rows(1) title("Coastwide harvest ('000s fish')",size(medium))  ///
	note("MRIP 2024: survey-weighted estimate and SE-based CIs" "RDM 2026: median and percentile-based CIs" , size(small) yoffset(-2)) 
graph export coatswide_harvest_num.jpg, width(1024) height(768) replace


*release numbers 
gr drop _all 

levelsof domain if inlist(metric, "release_numbers"),  local(doms)
foreach d of local doms{

levelsof species if dom=="`d'"
if `r(levels)'=="bsb"{
	local sp="Black sea bass"
}
if `r(levels)'=="sf"{
	local sp="Summer flounder"
}
if `r(levels)'=="scup"{
	local sp="Scup"
}


twoway  (rcap ll80 ul80 source2_80  if domain=="`d'", lcolor(navy)) ///
			 (rcap ll90 ul90 source2_90 if domain=="`d'", lcolor(blue)) ///
			(scatter value source2_80 if domain=="`d'", mcolor(black) msymbol(O) msize(small) ) ///
			(scatter value source2_90 if domain=="`d'", msymbol(O) msize(small)  mcolor(black) title("`sp'", size(medium))  name(`d', replace) ///
			 xlab(1 " " 2 "MRIP 2024" 3 "RDM 2026" 4 " ", noticks labsize(small)  ) xtitle("") note("") ytitle("", size(small)) ylab(,labsize(small)) ///
			 legend(order(1 "80% CI" 2 "90% CI" ) pos(6) rows(1) size(small) region(lstyle(none))) )
			

 }
grc1leg sf_release_numbers bsb_release_numbers scup_release_numbers, rows(1) title("Coastwide discards ('000s fish')",size(medium))  ///
	note("MRIP 2024: survey-weighted estimate and SE-based CIs" "RDM 2026: median and percentile-based CIs" , size(small) yoffset(-2)) 

graph export coatswide_discard_num.jpg, width(1024) height(768) replace

** Format table 
gen order=1 if metric=="catch_numbers"
replace order=2 if metric=="release_numbers"
replace order=3 if metric=="discmort_number"
replace order=4 if metric=="keep_numbers"
replace order=5 if metric=="release_weight"
replace order=6 if metric=="discmort_weight"
replace order=7 if metric=="keep_weight"
replace order=8 if metric=="change_trips"
replace order=9 if metric=="CV"
keep if order!=.
keep if source=="RDM 2026"
keep species order metric  value ll90 ll80  ul80  ul90   
order species metric   ll90 ll80 value  ul80  ul90    
sort  species order

local vars value ll90 ll80  ul80  ul90   
foreach v of local vars{
	replace `v'=`v'*2205 if strmatch(metric, "*weight*")==1

}

*/

