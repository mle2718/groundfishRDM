

set seed 03211990

u "$misc_data_cd\CE_survey_data.dta", clear 

*test specification - linear in preferences	
/*
gen cod_hadd_kpt=codkpt*hadkpt
mixlogit choice cost ///
				nofish_age nofish_male nofish_total_days12 nofish_likely_to_fish nofish_fish_pref_more nofish_inc_med nofish_inc_high ///
				nofish_educ_coll nofish_educ_grad nofish_own_boat if no_choice!=1, ///
				group(gid) id(qtid) rand(codkpt codrel hadkpt hadrel cod_hadd_kpt nofish) nrep(250)			
*/		
				
				
*Final specification used for 2026 mgt. cycle
/*
mixlogit choice cost ///
				nofish_age nofish_male nofish_total_days12 nofish_likely_to_fish nofish_fish_pref_more nofish_inc_med nofish_inc_high ///
				nofish_educ_coll nofish_educ_grad nofish_own_boat if no_choice!=1, ///
				group(gid) id(qtid) rand(sqrt_codkpt sqrt_codrel sqrt_hadkpt sqrt_hadrel sqrt_cod_hadd_kpt nofish) nrep(250)
*/
		
*Preferred spec using cmxtmixlogit 
global max_options ///
    difficult intmethod(halton) intpoints(500) ///
    technique(bhhh 15 nr 5) iterate(25) ///
    nrtolerance(1e-3)
	
cmset qtid scenario alt
eststo m0_gf: cmxtmixlogit choice cost /// 
					 nofish_age nofish_male ///
					 nofish_total_days12 ///
					 nofish_likely_to_fish ///
					 nofish_fish_pref_more ///
					 nofish_inc_med nofish_inc_high ///
					nofish_educ_coll ///
					nofish_educ_grad ///
					nofish_own_boat if no_choice!=1, ///
					random(sqrt_codkpt sqrt_codrel sqrt_hadkpt sqrt_hadrel sqrt_cod_hadd_kpt nofish) noconstant $max_options
					
estimates save "$misc_data_cd\m0_gf.ster", replace 			

global params
forv x=1/$ndraws{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }
    
	
        * generate draws from vector beta - sampling uncertainty
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		
		
		* generate 5000 draws based on the drawn mean and SD above for the betas specified as random - preference heterogeneity
		* enter zeroes for the parameters above the 10% level of significance
		
		clear 
		set obs 5000
		
		*original non-linear in catch spec
		gen beta_cost=beta_draw[1,1]
		gen beta_nofish_age=0
		gen beta_nofish_male=0
		gen beta_opt_out_trips12=beta_draw[4,1]
		gen beta_nofish_likely=0
		gen beta_nofish_fish_pref=beta_draw[6,1]
		gen beta_nofish_inc_med=0
		gen beta_nofish_inc_high=0
		gen beta_nofish_educ_coll=beta_draw[9,1]
		gen beta_nofish_educ_grad=beta_draw[10,1]
		gen beta_nofish_own_boat=0
		gen beta_cod_keep=rnormal(beta_draw[12,1], abs(beta_draw[18,1]))
		gen beta_cod_rel=rnormal(beta_draw[13,1], 0)
		gen beta_hadd_keep=rnormal(beta_draw[14,1], abs(beta_draw[20,1]))
		gen beta_hadd_rel=rnormal(beta_draw[15,1], abs(beta_draw[21,1]))
		gen beta_cod_hadd_keep=rnormal(beta_draw[16,1], abs(beta_draw[22,1]))
		gen beta_nofish=rnormal(0, abs(beta_draw[23,1]))
		
		/*supplementary linear in catch spec
		gen beta_cost=beta_draw[1,1]
		gen beta_nofish_age=0
		gen beta_nofish_male=0
		gen beta_opt_out_trips12=beta_draw[4,1]
		gen beta_nofish_likely=0
		gen beta_nofish_fish_pref=beta_draw[6,1]
		gen beta_nofish_inc_med=0
		gen beta_nofish_inc_high=0
		gen beta_nofish_educ_coll=beta_draw[9,1]
		gen beta_nofish_educ_grad=beta_draw[10,1]
		gen beta_nofish_own_boat=beta_draw[11,1]
		gen beta_cod_keep=rnormal(beta_draw[12,1], abs(beta_draw[18,1]))
		gen beta_cod_rel=rnormal(beta_draw[13,1], 0)
		gen beta_hadd_keep=rnormal(beta_draw[14,1], abs(beta_draw[20,1]))
		gen beta_hadd_rel=rnormal(beta_draw[15,1], abs(beta_draw[21,1]))
		gen beta_cod_hadd_keep=rnormal(beta_draw[16,1], abs(beta_draw[22,1]))
		gen beta_nofish=rnormal(0, abs(beta_draw[23,1]))*/
		
			
	gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params


* keep only non-zero parameters 
drop beta_nofish_inc_high  beta_nofish_inc_med beta_nofish_likely beta_nofish_male beta_nofish_age
compress

rename beta_nofish_fish_pref beta_opt_out_fish_pref
rename beta_nofish_educ_coll beta_opt_out_educ2
rename beta_nofish_educ_grad beta_opt_out_educ3
rename beta_nofish_own_boat beta_opt_out_ownboat
rename beta_cod_keep beta_sqrt_cod_keep
rename beta_cod_rel beta_sqrt_cod_release
rename beta_hadd_keep beta_sqrt_hadd_keep
rename beta_hadd_rel beta_sqrt_hadd_release
rename beta_cod_hadd_keep beta_sqrt_cod_hadd_keep
rename beta_nofish beta_opt_out

save  "$misc_data_cd\preference_params.dta", replace


