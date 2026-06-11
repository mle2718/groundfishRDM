
** Steps to produce projected catch at length probabilty distribution
*1. Read baseline recreational catch-at-length 
*2. Build cod and haddock age-length keys (ALKs) from recent NEFSC trawl data.
*3. Convert baseline stock assessment numbers-at-age (NAA) into baseline numbers-at-length (NAL).
*4. Merge baseline NAL to observed baseline catch-at-length and compute an empirical recreational selectivity at length by species, season, draw, and length.
*5. Convert projected NAA into projected NAL using the same ALKs.
*6. Apply baseline selectivity-at-length to projected NAL to obtain projected catch-at-length.
*7. Convert projected catch-at-length into a probability distribution over lengths, using a gamma-smoothed fitted distribution.
*8. Export projected fitted probabilities by draw/species/season/length.

 
* 1. Pull in baseline catch-at-lengths

set seed $seed

import delimited using "$misc_data_cd/baseline_catch_at_length_observed.csv", clear  
keep if draw<= $ndraws
sort draw season species length

tempfile cal
save `cal', replace 


*2. Create age-length keys from NEFSC trawl survey data
	*a. Pull in NEFSC trawl survey data from the last X of data available
	*b. Smooth counts across age classes over the range of observed catch-at-lengths for a given state-species using a LOWESS bandwidth=0.3
	*c. Compute the proportion of fish of age a that are length l

* cod ALK - age 1 through 6+ 
* there are few obs. for age 7+, combine these into 6+ category

*2a
import delimited using "$misc_data_cd/NEFSC_cruises.csv", clear 
renvarlab, lower
tempfile cruises
sort year 
save `cruises', replace 

import delimited using "$misc_data_cd/NEFSC_trawl_cod.csv", clear 
renvarlab, lower
rename countage count 
merge m:1 cruise6 using `cruises'
collapse (sum) count, by(year season svspp age length)
keep if year>=$trawl_survey_start_year
collapse (sum) count, by(year age length)

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
di `min_svy_yr'
tabstat count, stat(sum) by(age)
replace age=6 if age>=6
collapse (sum) count, by (age length)
drop if age==. | length==.

tsset age length
tsfill, full

sort age length 
mvencode count, mv(0) override 

*2b.
levelsof age, local(ages)
foreach a of local ages{
	lowess count length if age==`a' , adjust bwidth(.3) gen(s`a') nograph
	replace s`a'=0 if s`a'<=0
}

egen smoothed=rowtotal(s0-s6)
drop s0-s6

egen sum=sum(smoothed), by(age)	
gen prop_smoothed=smoothed/sum	

*2c.
egen sum_raw=sum(count), by(age)	
gen prop_raw=count/sum_raw	

/*
levelsof age, local(ages)
foreach a of local ages{
twoway(scatter prop_raw length if age==`a',   connect(direct) lcol(red)   lpat(solid) msymbol(i) ) ///
			(scatter prop_smoothed length if age==`a', connect(direct) lcol(blue) title("cod age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr'", size(small)) ///
			ytitle("proportion of fish that are age-a", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cms, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames' 
graph export "$figure_cd/cod_prop_length_at_age.png", as(png) replace
*/

drop if age==0
drop sum sum_raw
tempfile al_cod
save `al_cod', replace 


* Haddock ALK - age 1 through 9 
*2a. 
import delimited using "$misc_data_cd/NEFSC_cruises.csv", clear 
renvarlab, lower
tempfile cruises
sort year 
save `cruises', replace 

import delimited using "$misc_data_cd/NEFSC_trawl_hadd.csv", clear 
renvarlab, lower
merge m:1 cruise6 using `cruises'
collapse (sum) count, by(year season svspp age length)
keep if year>=$trawl_survey_start_year
collapse (sum) count, by(year age length)

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
di `min_svy_yr'
tabstat count, stat(sum) by(age)
replace age=9 if age>=9
collapse (sum) count, by (age length)
drop if age==. | length==.

tsset age length
tsfill, full

sort age length 
mvencode count, mv(0) override 

*2b.
levelsof age, local(ages)
foreach a of local ages{
	lowess count length if age==`a' , adjust bwidth(.3) gen(s`a') nograph
	replace s`a'=0 if s`a'<=0
}

egen smoothed=rowtotal(s0-s9)
drop s0-s9

*2c.
egen sum=sum(smoothed), by(age)	
gen prop_smoothed=smoothed/sum	

egen sum_raw=sum(count), by(age)	
gen prop_raw=count/sum_raw	

/*
levelsof age, local(ages)
foreach a of local ages{
twoway(scatter prop_raw length if age==`a',   connect(direct) lcol(red)   lpat(solid) msymbol(i) ) ///
			(scatter prop_smoothed length if age==`a', connect(direct) lcol(blue) title("haddock age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr'", size(small)) ///
			ytitle("proportion of fish that are age-a", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cms, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames' 
graph export "$figure_cd/hadd_prop_length_at_age.png", as(png) replace
*/

drop if age==0
drop sum sum_raw
tempfile al_hadd
save `al_hadd', replace 


* 3.  Convert baseline stock assessment numbers-at-age (NAA) into baseline numbers-at-length (NAL).
* cod
use "$misc_data_cd/WGOM_Cod_historical_NAA.dta", clear 
keep if year==$cod_NAA_base_year
split metric, parse(" ")
rename metric6 age
keep age value year
destring age, replace
reshape wide value, i(year) j(age)

egen value6_plus=rowtotal(value6-value9)
drop value6 value7 value8 value9
rename value6 value6
reshape long value, i(year) j(new)
replace value=value*1000
rename value nfish
rename new age 
drop year 

merge 1:m age using `al_cod', keep(3) nogen 
sort  age length

gen base_nal_raw = prop_raw*nfish
gen base_nal_smooth = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) base_nal*, by(length)

sort length 
gen species="cod"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

tempfile naa_cod
save `naa_cod', replace 

* haddock 
use "$misc_data_cd/GOM_Haddock_historical_NAA.dta", clear 
keep if year==$hadd_NAA_base_year
split metric, parse(" ")
rename metric6 age
keep age value 
replace value=value*1000
rename value nfish
destring age, replace

merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen base_nal_raw = prop_raw*nfish
gen base_nal_smooth = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) base_nal*, by(length)

sort length 
gen species="hadd"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

append using  `naa_cod'

tempfile base_naa
save `base_naa', replace 

* 4. Merge baseline NAL to observed baseline catch-at-length and selectivity at length by species, season, draw, and length.
merge 1:m species season length using `cal', keep(2 3)
drop if draw==.

rename n_fish catch
mvencode catch base_nal* , mv(0) override
sort species season  draw length

gen frac_caught_smooth = catch / base_nal_smooth if base_nal_smooth > 0
gen frac_caught_raw    = catch / base_nal_raw    if base_nal_raw > 0

sort species season  draw length
drop if catch==0
mvencode frac_caught*, mv(0) override

* catch_l > population_l adjustment 
* This block reassigns catch lengths where base_nal_smooth == 0 and the catch falls outside the population length support. 
* It does not  address cases where `catch > base_nal_smooth` at lengths where population is nonzero but small. This will produce `frac_caught_smooth > 1`.
* This is acceptable because "fraction caught" is only a scaling factor

egen min_length_pop=min(length) if base_nal_smooth!=0, by(species season draw)
egen max_length_pop=max(length) if base_nal_smooth!=0, by(species season draw)

egen min_length_catch=min(length) if catch!=0, by(species season draw)
egen max_length_catch=max(length) if catch!=0, by(species season draw)

local vars min_length_pop max_length_pop min_length_catch max_length_pop max_length_catch
foreach v of local vars{
	egen mean_`v'=mean(`v'), by(species season draw)
	replace `v'= mean_`v'
	drop mean_`v'
	
}

replace length=max_length_pop if catch>0 & base_nal_smooth==0 & length>max_length_pop
replace length=min_length_pop if catch>0 & base_nal_smooth==0 & length<min_length_pop

collapse (sum) catch base_nal*,  by(species season  draw length )
drop if catch==0

gen frac_caught_smooth = catch / base_nal_smooth if base_nal_smooth > 0
gen frac_caught_raw    = catch / base_nal_raw    if base_nal_raw > 0

sort species season draw length

mvencode frac_caught*, mv(0) override

tempfile selectivity
save `selectivity', replace


*5. Convert projected NAA into projected NAL
* cod
use "$misc_data_cd/WGOM_Cod_projected_NAA.dta", clear 
keep if year==$cod_NAA_proj_year

split metric, parse(" ")
rename metric5 age
keep age value year replicate
destring age, replace
reshape wide value, i(year replicate) j(age)
sample $ndraws, count 
gen draw=_n
egen value6_plus=rowtotal(value6-value9)
drop value6 value7 value8 value9
rename value6 value6
reshape long value, i(year replicate draw) j(new)
replace value=value*1000
rename value nfish
rename new age 

*check to validate  - increase the proportion of large fish 
*replace nfish=nfish*20 if age>=6

drop year 
sort draw age
rename replicate cod_replicate

preserve 
u `al_cod', clear 
expand $ndraws
bysort length age: gen draw=_n
tempfile al_cod_expand
save `al_cod_expand', replace
restore 

merge 1:m age draw using `al_cod_expand', keep(3) nogen 
sort  draw age length

gen proj_nal_raw = prop_raw*nfish
gen proj_nal_smooth = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) proj_nal*, by(length draw cod_replicate)

sort length 
gen species="cod"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

tempfile proj_naa_cod
save `proj_naa_cod', replace 

* haddock 
use "$misc_data_cd/GOM_Haddock_projected_NAA.dta", clear 

keep if year==$hadd_NAA_proj_year
split metric, parse(" ")
rename metric5 age
keep age value replicate
replace value=value*1000
rename value nfish
destring age, replace

*check to validate  - increase the proportion of large fish 
*replace nfish=nfish*20 if age>=6

reshape wide nfish, i( replicate) j(age)
sample $ndraws, count 
gen draw=_n
reshape long nfish, i( draw replicate) j(new)
rename new age 
rename replicate hadd_replicate

preserve 
u `al_hadd', clear 
expand $ndraws
bysort length age: gen draw=_n
tempfile al_hadd_expand
save `al_hadd_expand', replace
restore 

merge 1:m age draw using `al_hadd_expand', keep(3) nogen 
sort  draw age length

gen proj_nal_raw = prop_raw*nfish
gen proj_nal_smooth = prop_smoothed*nfish

collapse (sum) proj_nal*, by( length draw hadd_replicate)

sort length 
gen species="hadd"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

sort season draw length

append using  `proj_naa_cod' 


* 6. Apply baseline empirical fraction-caught-at-length to projected population NAL.
		* This assumes that the length-specific recreational catchability/selectivity observed
		* in the baseline year remains constant in the projection year, while projected stock
		* composition changes according to projected NAA translated to NAL using the ALK.

merge 1:1 species season length draw using `selectivity'
sort species season draw length

gen catch_proj= frac_caught_smooth*proj_nal_smooth
mvencode catch*, mv(0)

keep length species season draw  catch catch_proj cod_replicate hadd_replicate proj_* base*
tostring draw, gen(draw2)
gen domain=species+"_"+season+"_"+draw2

egen sum=sum(catch), by(species season draw domain)
gen observed_prob_base=catch/sum
egen sum_proj=sum(catch_proj), by(species season draw domain)
gen observed_prob_proj=catch_proj/sum_proj
format sum* %20.0gc
drop sum*


preserve 
rename length fitted_length
keep fitted_length observed_prob*  species season domain draw proj_* base*
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


*7. Convert projected catch-at-length into a probability distribution over lengths, using gamma-smoothed fitted distribution.

* MOM approach to avoid non-convergence 
tempfile new
save `new', replace

global fitted_sizes

levelsof domain, local(regs)

qui foreach r of local regs {
    use `new', clear
    keep if domain=="`r'"
    di "`r'"

    keep length catch_proj
    drop if missing(length) | missing(catch_proj)
    drop if catch_proj<=0
	replace catch_proj=round(catch_proj)
	su catch_proj
	local tot_n_fish=`r(sum)'
	

    * Gamma needs strictly positive support
    drop if length<=0

	* --------
    * (A) Estimate gamma parameters robustly (MOM with freq weights)
    * --------
    quietly summarize length [fw=catch_proj], meanonly
    local mu = r(mean)
    local Nw = r(sum_w)
	
	
    * Weighted variance: Var = E[x^2] - (E[x])^2 using the same freq weights
    gen double length2 = length^2
    quietly summarize length2 [fw=catch_proj], meanonly
    local ex2 = r(mean)
    local v   = `ex2' - (`mu'^2)

    * Guard: if variance is 0 or numerically tiny, make it a near-degenerate gamma
    if (`v'<=1e-10 | missing(`v') | missing(`mu') | `mu'<=0) {
        * Put essentially all mass at mu by using huge alpha
        local alpha = 1e6
        local beta  = `mu'/`alpha'
    }
    else {
        local alpha = (`mu'^2)/`v'
        local beta  = `v'/`mu'
    }

    *Simulate a truncated gamma sample via rejection sampling
    local ndraw = `tot_n_fish'   // sample size for the simulated distribution
    clear
    set obs `ndraw'

    * draw
    gen double gammafit = rgamma(`alpha', `beta')
    replace gammafit = round(gammafit)


    * If rejection killed everything, try again with more draws (once)
    if _N==0 {
        clear
        set obs `=5*`ndraw''
        gen double gammafit = rgamma(`alpha', `beta')
        replace gammafit = round(gammafit)
        if _N==0 continue
    }

    gen nfish = 1
    collapse (sum) nfish, by(gammafit)
    egen sumnfish = total(nfish)
    gen double fitted_prob = nfish/sumnfish
    gen domain = "`r'"

    tempfile fitted_sizes_`=_N'   
    save `fitted_sizes_`=_N'', replace
    global fitted_sizes "$fitted_sizes `fitted_sizes_`=_N''"
}

clear
dsconcat $fitted_sizes
rename gammafit fitted_length

merge 1:1 fitted_length domain using `observed_prob'
sort domain fitted_length 
mvencode fitted_prob observed_prob*, mv(0) override 

split domain, parse(_)
replace season=domain2
replace species=domain1
drop draw
replace domain=species+"_"+season+"_"+domain3
rename domain3 draw
destring draw, replace 
rename fitted_l length

drop _merge nfish sum
order species season domain draw length
drop domain1 domain2 
rename fitted_prob fitted_prob_proj

preserve
import delimited using "$misc_data_cd/baseline_catch_at_length.csv", clear  
keep if draw<= $ndraws
tempfile baseyr
save `baseyr', replace 
restore

merge 1:1  species season draw length using `baseyr'
sort species season draw length

keep species season domain draw length fitted* observed* proj_nal*
mvencode fitted* observed* proj_nal*, mv(0) override
drop observed_prob
rename fitted_prob fitted_prob_base

merge m:1 length species season using  `base_naa'

sort draw species season length
local vars base_nal_raw base_nal_smooth proj_nal_raw proj_nal_smooth
foreach v of local vars{
	egen sum_`v'=sum(`v'), by(draw species season)
	gen prop_`v'=`v'/sum_`v'
	drop sum_`v'
}


* truncate the fitted distribution to the observed range
levelsof domain, local(doms)
foreach d of local doms{
quietly summarize length if observed_prob_base!=0 & !missing(observed_prob_base) & domain=="`d'"
local minL = `r(min)'
local maxL = `r(max)'
drop if (length<`minL' | length>`maxL') & domain=="`d'"
}

egen sum_fitted_prob=sum(fitted_prob_proj), by(domain)
replace fitted_prob_proj=fitted_prob_proj/sum_fitted_prob

* uncomment if you want plots of the resulting distributions, evaluated at mean by length
* plots of base and projected catch-at-length
/*
collapse (mean) observed* fitted* prop* base_nal* proj_nal*, by(species season length)
gen domain=season+"_"+species

levelsof domain , local(domz)
foreach d of local domz{
	twoway (scatter observed_prob_base length if domain=="`d'" ,   cmissing(no) connect(direct) lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter observed_prob_proj length if  domain=="`d'"  , cmissing(no) connect(direct) lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "observed_catch_at_length_prob_base") lab(2 "observed_catch_at_length_prob_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)


levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter fitted_prob_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "fitted_catch_at_length_prob_base") lab(2 "fitted_catch_at_length_prob_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter observed_prob_proj length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "observed_catch_at_length_prob_proj") lab(2 "fitted_catch_at_length_prob_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter observed_prob_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_base length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "observed_catch_at_length_prob_base") lab(2 "fitted_catch_at_length_prob_base") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)



levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter prop_base_nal_raw length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter prop_proj_nal_raw length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "prop_base_nal_raw") lab(2 "prop_proj_nal_raw") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter prop_base_nal_smooth length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter prop_proj_nal_smooth length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "prop_base_nal_smooth") lab(2 "prop_proj_nal_smooth") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter prop_base_nal_smooth length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter prop_proj_nal_smooth length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "prop_base_nal_smooth") lab(2 "prop_proj_nal_smooth") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)
*/

* 8. Export projected fitted probabilities
keep draw length species season  fitted_prob_proj 
drop if missing(fitted_prob_proj) | fitted_prob_proj == 0
rename fitted_prob_proj fitted_prob
compress
export delimited using "$misc_data_cd/projected_catch_at_length.csv", replace 

