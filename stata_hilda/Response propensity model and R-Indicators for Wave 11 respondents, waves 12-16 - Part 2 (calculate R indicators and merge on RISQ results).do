

* Run parameters
set more off
macro drop _all

* Specify HILDA release
global release = 160

* Specify waves in which to model response propensities, and the initial wave from which respondents and their characteristics will form the basis of the response propensity models
global startwave = 12
global endwave   = 16
global initialwave = 11

* Other global and directory definitions
global alphabet "abcdefghijklmnopqrstuvwxyz"
global releasenum = substr("$release",1,2)
global releaseu = "$release"+"u"
global releasew = substr("$alphabet",$releasenum,1)
global hildalocation   "X:\HILDA\Release $release\files\STATA $releaseu"
global workinglocation "H:\E\working_arc\Adaptivedesign"
global resultslocation "H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Results"
global logslocation    "H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Logfiles"


* Fieldwork periods (for each of waves 12-16, P1 is 69 days, P2 is 48 days, and P3 is 32 days)
* Wave 16
global w16_p1start = "26/07/2016"
global w16_p1end   = "02/10/2016"
global w16_p2start = "18/10/2016"
global w16_p2end   = "04/12/2016"
global w16_p3start = "05/01/2017"
global w16_p3end   = "05/02/2017"
* Wave 15
global w15_p1start = "28/07/2015"
global w15_p1end   = "04/10/2015"
global w15_p2start = "20/10/2015"
global w15_p2end   = "06/12/2015"
global w15_p3start = "07/01/2016"
global w15_p3end   = "07/02/2016"
* Wave 14
global w14_p1start = "29/07/2014"
global w14_p1end   = "05/10/2014"
global w14_p2start = "21/10/2014"
global w14_p2end   = "07/12/2014"
global w14_p3start = "08/01/2015"
global w14_p3end   = "08/02/2015"
* Wave 13
global w13_p1start = "30/07/2013"
global w13_p1end   = "06/10/2013"
global w13_p2start = "22/10/2013"
global w13_p2end   = "08/12/2013"
global w13_p3start = "09/01/2014"
global w13_p3end   = "09/02/2014"
* Wave 12
global w12_p1start = "31/07/2012"
global w12_p1end   = "07/10/2012"
global w12_p2start = "23/10/2012"
global w12_p2end   = "09/12/2012"
global w12_p3start = "10/01/2013"
global w12_p3end   = "10/02/2013"

* Predictor variables for response propensity model (include i. prefixes for categorical variables)
global predictors           i.agerange i.employment i.owner i.sectionofstate i.relationship i.likelymove i.healthcondition i.countryofbirth i.education i.sex i.childreninhh i.adultsinhh i.benefitreliant i.ftstudent i.topupsample
global predictors_subsample i.agerange i.employment i.owner i.sectionofstate i.relationship i.likelymove i.healthcondition i.countryofbirth i.education i.sex i.childreninhh i.adultsinhh i.benefitreliant i.ftstudent
global n_predictors: 			word count $predictors
global n_predictors_subsample: 	word count $predictors_subsample
display in red "$n_predictors predictor variables to be included in response propensity model for whole sample"
display in red "$n_predictors_subsample predictor variables to be included in response propensity models for Main and TopUp samples separately"

* For each predictor, provide the minimum and maximum values 
global agerange_startvalue = 1 
global agerange_endvalue = 8 
global employment_startvalue = 0
global employment_endvalue = 3
global owner_startvalue = 0 
global owner_endvalue = 1 
global sectionofstate_startvalue = 1
global sectionofstate_endvalue = 3
global relationship_startvalue = 1
global relationship_endvalue = 4
global likelymove_startvalue = 0
global likelymove_endvalue = 1
global healthcondition_startvalue = 0
global healthcondition_endvalue = 2
global countryofbirth_startvalue = 1
global countryofbirth_endvalue = 3
global education_startvalue = 1
global education_endvalue = 6
global sex_startvalue = 1
global sex_endvalue = 2
global childreninhh_startvalue = 0
global childreninhh_endvalue = 2
global adultsinhh_startvalue = 1
global adultsinhh_endvalue = 3
global benefitreliant_startvalue = 0
global benefitreliant_endvalue = 1
global ftstudent_startvalue = 0
global ftstudent_endvalue = 1 
global topupsample_startvalue = 0
global topupsample_endvalue = 1

*********************************************************************************************************************************************************************************


********************************************************** PART 5 - CONSTRUCT PARTIAL R-INDICATORS ******************************************************************************

display in red "**********************************************************************************************************************************************************************************************************************"
display in red "Modelling response propensities, calculating R indicators, merging with RISQ output, and outputting to Results\Phase 2 - R-Indicators and CVs.xls"
display in red "**********************************************************************************************************************************************************************************************************************"

* Define programs to be called in the rindicators program
capture program drop subsamples
program define subsamples
args samplenumber wave
if `samplenumber'==1 {
local samp = "Main"
}
if `samplenumber'==2 {
local samp = "TopUp"
}
if `samplenumber'==1 {
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'&iw_hhtup==0
}
if `samplenumber'==2 {
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'&iw_hhtup==1
}

forvalues period = 1/3 {
	/* Calculate overall R indicators and coefficients of variation */
	xi:logistic response_P`period' $predictors_subsample
	lroc
	predict presponse_P`period'_allvars , pr
	sum presponse_P`period'_allvars 
	gen rind_P`period'_allvars = 1-2*r(sd)
	gen rsd_P`period' = r(sd)
	gen pbar_P`period' = r(sum)/r(N)
	gen cv_P`period'_allvars = r(sd)/pbar_P`period'
	display "R-Indicator at end of P`period' - `samp' sample - wave `wave' response propensities"
	list rind_P`period'_allvars in 1/1
	display "CV at end of P`period' - `samp' sample - wave `wave' response propensities"
	list cv_P`period'_allvars in 1/1

	/* Calculate variable-level conditional partial R indictors and CVs */
	
		/* Method 2 - Based on RISQ manual page 13 */
		foreach set of global predictors_subsample {
			local reducedlist = subinword("$predictors_subsample","`set'","",1)
			local variable = subinstr("`set'","i.","",1)
			local reducedlist_nois = subinstr("`reducedlist'","i.","",500)
			bys `reducedlist_nois': egen lvlmns_wo_`variable'_P`period' = mean(presponse_P`period'_allvars)
			gen cond_diff_sq_`variable'_P`period' = (presponse_P`period'_allvars - lvlmns_wo_`variable'_P`period')^2
			sum cond_diff_sq_`variable'_P`period'
			gen cprind_P`period'_`variable' = 1000*sqrt((1/r(N))*r(sum))
			gen ccv_P`period'_`variable' = cprind_P`period'_`variable'/pbar_P`period'
		}
		display "Conditional Partial R indicators (variable-level) at end of P`period' - `samp' sample - wave `wave' response propensities"
		list cprind_P`period'_* in 1/1
		display "Conditional Coefficients of Variation (variable-level) at end of P`period' - `samp' sample - wave `wave' response propensities"
		list ccv_P`period'_* in 1/1
		
	/* Calculate variable-level unconditional partial R indicators and CVs */
	foreach set of global predictors_subsample {
		local variable = subinstr("`set'","i.","",1)
		gen cum_sum_P`period'_`variable' = 0
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			gen `variable'`category'_P`period'=1 if `variable'==`category'			/* Same for each period, but _P added to avoid complexity in variable generation */
			replace `variable'`category'_P`period'=0 if `variable'~=`category'		/* Same for each period, but _P added to avoid complexity in variable generation */
			sum `variable'`category'_P`period'
			gen n_`variable'`category'_P`period'=r(sum)								/* Same for each period, but _P added to avoid complexity in variable generation */
			sum presponse_P`period'_allvars if `variable'`category'_P`period'==1
			gen pbar_P`period'_`variable'`category'=r(sum)/r(N)
			sum presponse_P`period'_allvars
			replace cum_sum_P`period'_`variable'=   cum_sum_P`period'_`variable' ///
														 + (n_`variable'`category'_P`period')*((pbar_P`period'_`variable'`category'-pbar_P`period')^2)
		}
		gen uprind_P`period'_`variable'=1000*sqrt((1/r(N))*cum_sum_P`period'_`variable')
		gen ucv_P`period'_`variable' = uprind_P`period'_`variable'/pbar_P`period'
	}
	display "Unconditional Partial R indicators (variable-level) at end of P`period' - `samp' sample - wave `wave' response propensities"
	list uprind_P`period'_* in 1/1
	display "Unconditional Coefficients of Variation (variable-level) at end of P`period' - `samp' sample - wave `wave' response propensities"
	list ucv_P`period'_* in 1/1
	
}



* Keep only results and transpose ready for exporting to Excel
sum rind_P1_allvars
local N=r(N)
drop in 2/`N'
keep rind* uprind* cprind* cv_* ccv* ucv*
xpose, clear varname
rename v1 Value
rename _varname TypePeriodVar
order TypePeriodVar Value

* Save results
local rows_per_fwperiod = 4*$n_predictors_subsample+2
local startP2 = `rows_per_fwperiod'+1
local endP2 = 2*`rows_per_fwperiod'
local startP3 = `endP2'+1
local endP3 = 3*`rows_per_fwperiod'
save "$workinglocation\RIndicators_MyProgram_`samp'Sample", replace
drop in `startP2'/`endP3'
rename TypePeriodVar Type_Var
rename Value P1
replace Type_Var="rind_allvars" if Type_Var=="rind_P1_allvars"
replace Type_Var="CV_allvars" if Type_Var=="cv_P1_allvars"
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	replace Type_Var="cprind_`variable'" if Type_Var=="cprind_P1_`variable'"
	replace Type_Var="uprind_`variable'" if Type_Var=="uprind_P1_`variable'"
	replace Type_Var="cpcv_`variable'" if Type_Var=="ccv_P1_`variable'"
	replace Type_Var="upcv_`variable'" if Type_Var=="ucv_P1_`variable'"
}
save "$workinglocation\RIndicators_MyProgram_P1_`samp'Sample", replace
use "$workinglocation\RIndicators_MyProgram_`samp'Sample", clear
drop in 1/`rows_per_fwperiod'
drop in `startP2'/`endP2'
rename TypePeriodVar Type_Var
rename Value P2
replace Type_Var="rind_allvars" if Type_Var=="rind_P2_allvars"
replace Type_Var="CV_allvars" if Type_Var=="cv_P2_allvars"
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	replace Type_Var="cprind_`variable'" if Type_Var=="cprind_P2_`variable'"
	replace Type_Var="uprind_`variable'" if Type_Var=="uprind_P2_`variable'"
	replace Type_Var="cpcv_`variable'" if Type_Var=="ccv_P2_`variable'"
	replace Type_Var="upcv_`variable'" if Type_Var=="ucv_P2_`variable'"
}
save "$workinglocation\RIndicators_MyProgram_P2_`samp'Sample", replace
use "$workinglocation\RIndicators_MyProgram_`samp'Sample", clear
drop in 1/`endP2'
rename TypePeriodVar Type_Var
rename Value P3
replace Type_Var="rind_allvars" if Type_Var=="rind_P3_allvars"
replace Type_Var="CV_allvars" if Type_Var=="cv_P3_allvars"
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	replace Type_Var="cprind_`variable'" if Type_Var=="cprind_P3_`variable'"
	replace Type_Var="uprind_`variable'" if Type_Var=="uprind_P3_`variable'"
	replace Type_Var="cpcv_`variable'" if Type_Var=="ccv_P3_`variable'"
	replace Type_Var="upcv_`variable'" if Type_Var=="ucv_P3_`variable'"
}
save "$workinglocation\RIndicators_MyProgram_P3_`samp'Sample", replace
use "$workinglocation\RIndicators_MyProgram_P1_`samp'Sample", clear
merge 1:1 Type_Var using "$workinglocation\RIndicators_MyProgram_P2_`samp'Sample"
drop _merge
merge 1:1 Type_Var using "$workinglocation\RIndicators_MyProgram_P3_`samp'Sample"
drop _merge
gen ordering = 1 if Type_Var=="rind_allvars"
replace ordering = 2 if Type_Var=="CV_allvars"
replace ordering = 3 if substr(Type_Var,1,6)=="cprind"
replace ordering = 4 if substr(Type_Var,1,6)=="uprind"
replace ordering = 5 if substr(Type_Var,1,4)=="cpcv"
replace ordering = 6 if substr(Type_Var,1,4)=="upcv"
sort ordering Type_Var
drop ordering 
list
save "$workinglocation\RIndicators_MyProgram_`samp'Sample",replace
end

capture program drop risqresults
program define risqresults
args period wave

	clear
	import excel "$workinglocation\RISQoutput_w11r_wave`wave'_`period'_Overall.xlsx" , firstrow case(preserve)
	* Keep overall and variable level indicators
	keep r_indicator r_withbias CV_prop_adj CV_prop_unadj namevaru sqrt_uncond_var sqrt_uncond_var_adj sqrt_cond_var sqrt_cond_var_adj cvu cvc cvu_adj cvc_adj 
	
	* Rearrange and merge onto Stata results table
	drop if namevaru=="" & r_indicator==.
	save "$workinglocation\RIndicatorsFromSAS_keptinfo", replace
	drop if r_indicator==.
	keep r_*
	gen Type_Var="rind_allvars"
	rename r_withbias `period'_Unadjusted
	rename r_indicator `period'_Adjusted
	order Type_Var `period'_Unadjusted `period'_Adjusted
	save "$workinglocation\RIndicatorsFromSAS_rinds", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if r_indicator==.
	keep CV_*
	gen Type_Var="CV_allvars"
	rename CV_prop_unadj `period'_Unadjusted
	rename CV_prop_adj `period'_Adjusted
	order Type_Var `period'_Unadjusted `period'_Adjusted
	save "$workinglocation\RIndicatorsFromSAS_cvs", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru sqrt_cond_var sqrt_cond_var_adj
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="cprind_`variable'" if namevaru=="`variable'"
	}
	replace sqrt_cond_var=1000*sqrt_cond_var
	replace sqrt_cond_var_adj=1000*sqrt_cond_var_adj
	rename sqrt_cond_var `period'_Unadjusted
	rename sqrt_cond_var_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	save "$workinglocation\RIndicatorsFromSAS_cprinds", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru sqrt_uncond_var sqrt_uncond_var_adj
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="uprind_`variable'" if namevaru=="`variable'"
	}
	replace sqrt_uncond_var=1000*sqrt_uncond_var
	replace sqrt_uncond_var_adj=1000*sqrt_uncond_var_adj
	rename sqrt_uncond_var `period'_Unadjusted
	rename sqrt_uncond_var_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	order Type_Var `period'_Unadjusted `period'_Adjusted
	save "$workinglocation\RIndicatorsFromSAS_uprinds", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru cvc cvc_adj
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="cpcv_`variable'" if namevaru=="`variable'"
	}
	replace cvc=1000*cvc
	replace cvc_adj=1000*cvc_adj
	rename cvc `period'_Unadjusted
	rename cvc_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	save "$workinglocation\RIndicatorsFromSAS_cpcvs", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru cvu cvu_adj
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="upcv_`variable'" if namevaru=="`variable'"
	}
	replace cvu=1000*cvu
	replace cvu_adj=1000*cvu_adj
	rename cvu `period'_Unadjusted
	rename cvu_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	save "$workinglocation\RIndicatorsFromSAS_upcvs", replace
	use "$workinglocation\RIndicatorsFromSAS_rinds", clear
	append using "$workinglocation\RIndicatorsFromSAS_cvs"
	append using "$workinglocation\RIndicatorsFromSAS_cprinds"
	append using "$workinglocation\RIndicatorsFromSAS_uprinds"
	append using "$workinglocation\RIndicatorsFromSAS_cpcvs"
	append using "$workinglocation\RIndicatorsFromSAS_upcvs"
	save "$workinglocation\RIndicatorsFromRISQ_`period'", replace

	* Now add the category level indicators
	clear
	import excel "$workinglocation\RISQoutput_w11r_wave`wave'_`period'_Overall.xlsx" , firstrow case(preserve)
	local predictors_nois = subinstr("$predictors","i.","",500)
	keep sqrt_uncond_cat sqrt_cond_cat cvu_category cvc_category `predictors_nois' namevaru r_indicator
	drop if namevaru~="" | r_indicator~=.
	drop namevaru r_indicator
	save "$workinglocation\RIndicatorsFromSAS_catlevels", replace
	foreach set of global predictors {
		use "$workinglocation\RIndicatorsFromSAS_catlevels", clear
		local variable = subinstr("`set'","i.","",1)
		keep `variable' sqrt_uncond_cat
		drop if `variable'==.
		replace sqrt_uncond_cat=1000*sqrt_uncond_cat
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="uprind_cat_`variable'_`category'" if `variable'=="`category'"
		}	
		rename `variable' Type_Var
		rename sqrt_uncond_cat `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_uprind_`variable'", replace
	}
	foreach set of global predictors {
		clear
		use "$workinglocation\RIndicatorsFromSAS_catlevels"
		local variable = subinstr("`set'","i.","",1)
		keep `variable' sqrt_cond_cat
		drop if `variable'==.
		replace sqrt_cond_cat=1000*sqrt_cond_cat
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="cprind_cat_`variable'_`category'" if `variable'=="`category'"
		}
		rename `variable' Type_Var
		rename sqrt_cond_cat `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_cprind_`variable'", replace
	}		
	foreach set of global predictors {
		clear
		use "$workinglocation\RIndicatorsFromSAS_catlevels"
		local variable = subinstr("`set'","i.","",1)
		keep `variable' cvu_category
		drop if `variable'==.
		replace cvu_category=1000*cvu_category
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="upcv_cat_`variable'_`category'" if `variable'=="`category'"
		}
		rename `variable' Type_Var
		rename cvu_category `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_upcv_`variable'", replace
	}	
	foreach set of global predictors {
		clear
		use "$workinglocation\RIndicatorsFromSAS_catlevels"
		local variable = subinstr("`set'","i.","",1)
		keep `variable' cvc_category
		drop if `variable'==.
		replace cvc_category=1000*cvc_category
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="cpcv_cat_`variable'_`category'" if `variable'=="`category'"
		}
		rename `variable' Type_Var
		rename cvc_category `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_cpcv_`variable'", replace
	}	
	use "$workinglocation\RIndicatorsFromRISQ_`period'", clear
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_uprind_`variable'"
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_cprind_`variable'"
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_upcv_`variable'"
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_cpcv_`variable'"
	}
	save "$workinglocation\RIndicatorsFromRISQ_`period'", replace
end

capture program drop risqresults_subsample
program define risqresults_subsample
args period sample wave

	clear
	import excel "$workinglocation\RISQoutput_w11r_wave`wave'_`period'_`sample'.xlsx" , firstrow case(preserve)

	* Keep overall and variable level indicators
	keep r_indicator r_withbias CV_prop_adj CV_prop_unadj namevaru sqrt_uncond_var sqrt_uncond_var_adj sqrt_cond_var sqrt_cond_var_adj cvu cvc cvu_adj cvc_adj 
	
	* Rearrange and merge onto Stata results table
	drop if namevaru=="" & r_indicator==.
	save "$workinglocation\RIndicatorsFromSAS_keptinfo", replace
	drop if r_indicator==.
	keep r_*
	gen Type_Var="rind_allvars"
	rename r_withbias `period'_Unadjusted
	rename r_indicator `period'_Adjusted
	order Type_Var `period'_Unadjusted `period'_Adjusted
	save "$workinglocation\RIndicatorsFromSAS_rinds", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if r_indicator==.
	keep CV_*
	gen Type_Var="CV_allvars"
	rename CV_prop_unadj `period'_Unadjusted
	rename CV_prop_adj `period'_Adjusted
	order Type_Var `period'_Unadjusted `period'_Adjusted
	save "$workinglocation\RIndicatorsFromSAS_cvs", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru sqrt_cond_var sqrt_cond_var_adj
	foreach set of global predictors_subsample {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="cprind_`variable'" if namevaru=="`variable'"
	}
	replace sqrt_cond_var=1000*sqrt_cond_var
	replace sqrt_cond_var_adj=1000*sqrt_cond_var_adj
	rename sqrt_cond_var `period'_Unadjusted
	rename sqrt_cond_var_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	save "$workinglocation\RIndicatorsFromSAS_cprinds", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru sqrt_uncond_var sqrt_uncond_var_adj
	foreach set of global predictors_subsample {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="uprind_`variable'" if namevaru=="`variable'"
	}
	replace sqrt_uncond_var=1000*sqrt_uncond_var
	replace sqrt_uncond_var_adj=1000*sqrt_uncond_var_adj
	rename sqrt_uncond_var `period'_Unadjusted
	rename sqrt_uncond_var_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	order Type_Var `period'_Unadjusted `period'_Adjusted
	save "$workinglocation\RIndicatorsFromSAS_uprinds", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru cvc cvc_adj
	foreach set of global predictors_subsample {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="cpcv_`variable'" if namevaru=="`variable'"
	}
	replace cvc=1000*cvc
	replace cvc_adj=1000*cvc_adj
	rename cvc `period'_Unadjusted
	rename cvc_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	save "$workinglocation\RIndicatorsFromSAS_cpcvs", replace
	use "$workinglocation\RIndicatorsFromSAS_keptinfo", clear
	drop if namevaru==""
	keep namevaru cvu cvu_adj
	foreach set of global predictors_subsample {
		local variable = subinstr("`set'","i.","",1)
		replace namevaru="upcv_`variable'" if namevaru=="`variable'"
	}
	replace cvu=1000*cvu
	replace cvu_adj=1000*cvu_adj
	rename cvu `period'_Unadjusted
	rename cvu_adj `period'_Adjusted
	rename namevaru Type_Var
	sort Type_Var
	save "$workinglocation\RIndicatorsFromSAS_upcvs", replace
	use "$workinglocation\RIndicatorsFromSAS_rinds", clear
	append using "$workinglocation\RIndicatorsFromSAS_cvs"
	append using "$workinglocation\RIndicatorsFromSAS_cprinds"
	append using "$workinglocation\RIndicatorsFromSAS_uprinds"
	append using "$workinglocation\RIndicatorsFromSAS_cpcvs"
	append using "$workinglocation\RIndicatorsFromSAS_upcvs"
	save "$workinglocation\RIndicatorsFromRISQ_`period'_`sample'", replace

	* Now add the category level indicators
	clear
	import excel "$workinglocation\RISQoutput_w11r_wave`wave'_`period'_`sample'.xlsx" , firstrow case(preserve)
	local predictors_nois = subinstr("$predictors_subsample","i.","",500)
	keep sqrt_uncond_cat sqrt_cond_cat cvu_category cvc_category `predictors_nois' namevaru r_indicator
	drop if namevaru~="" | r_indicator~=.
	drop namevaru r_indicator
	save "$workinglocation\RIndicatorsFromSAS_catlevels", replace
	foreach set of global predictors_subsample {
		use "$workinglocation\RIndicatorsFromSAS_catlevels", clear
		local variable = subinstr("`set'","i.","",1)
		keep `variable' sqrt_uncond_cat
		drop if `variable'==.
		replace sqrt_uncond_cat=1000*sqrt_uncond_cat
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="uprind_cat_`variable'_`category'" if `variable'=="`category'"
		}	
		rename `variable' Type_Var
		rename sqrt_uncond_cat `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_uprind_`variable'", replace
	}
	foreach set of global predictors_subsample {
		clear
		use "$workinglocation\RIndicatorsFromSAS_catlevels"
		local variable = subinstr("`set'","i.","",1)
		keep `variable' sqrt_cond_cat
		drop if `variable'==.
		replace sqrt_cond_cat=1000*sqrt_cond_cat
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="cprind_cat_`variable'_`category'" if `variable'=="`category'"
		}
		rename `variable' Type_Var
		rename sqrt_cond_cat `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_cprind_`variable'", replace
	}		
	foreach set of global predictors_subsample {
		clear
		use "$workinglocation\RIndicatorsFromSAS_catlevels"
		local variable = subinstr("`set'","i.","",1)
		keep `variable' cvu_category
		drop if `variable'==.
		replace cvu_category=1000*cvu_category
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="upcv_cat_`variable'_`category'" if `variable'=="`category'"
		}
		rename `variable' Type_Var
		rename cvu_category `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_upcv_`variable'", replace
	}	
	foreach set of global predictors_subsample {
		clear
		use "$workinglocation\RIndicatorsFromSAS_catlevels"
		local variable = subinstr("`set'","i.","",1)
		keep `variable' cvc_category
		drop if `variable'==.
		replace cvc_category=1000*cvc_category
		tostring `variable', replace
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			replace `variable'="cpcv_cat_`variable'_`category'" if `variable'=="`category'"
		}
		rename `variable' Type_Var
		rename cvc_category `period'_Adjusted
		save "$workinglocation\RIndicatorsFromSAS_catlevels_cpcv_`variable'", replace
	}	
	use "$workinglocation\RIndicatorsFromRISQ_`period'_`sample'", clear
	foreach set of global predictors_subsample {
		local variable = subinstr("`set'","i.","",1)
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_uprind_`variable'"
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_cprind_`variable'"
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_upcv_`variable'"
		append using "$workinglocation\RIndicatorsFromSAS_catlevels_cpcv_`variable'"
	}
	save "$workinglocation\RIndicatorsFromRISQ_`period'_`sample'", replace
end

capture program drop groupperiodresults
program define groupperiodresults
args sample wave

use "$workinglocation\RIndicators_MyProgram_`sample'Sample", clear
merge 1:1 Type_Var using "$workinglocation\RIndicatorsFromRISQ_P1_`sample'"
drop _merge
merge 1:1 Type_Var using "$workinglocation\RIndicatorsFromRISQ_P2_`sample'"
drop _merge
merge 1:1 Type_Var using "$workinglocation\RIndicatorsFromRISQ_P3_`sample'"
drop _merge

order Type_Var P1 P2 P3 P1_Unadjusted P1_Adjusted P2_Unadjusted P2_Adjusted P3_Unadjusted P3_Adjusted
gen ordering=1 if Type_Var=="rind_allvars"
replace ordering=2 if Type_Var=="CV_allvars"
replace ordering=3 if substr(Type_Var,1,6)=="cprind" & substr(Type_Var,1,10)~="cprind_cat"
replace ordering=4 if substr(Type_Var,1,6)=="uprind" & substr(Type_Var,1,10)~="uprind_cat"
replace ordering=5 if substr(Type_Var,1,4)=="cpcv" & substr(Type_Var,1,8)~="cpcv_cat"
replace ordering=6 if substr(Type_Var,1,4)=="upcv" & substr(Type_Var,1,8)~="upcv_cat"
replace ordering=7 if substr(Type_Var,1,10)=="cprind_cat"
replace ordering=8 if substr(Type_Var,1,10)=="uprind_cat"
replace ordering=9 if substr(Type_Var,1,8)=="cpcv_cat"
replace ordering=10 if substr(Type_Var,1,8)=="upcv_cat"
sort ordering Type_Var
drop ordering

gen PCDiff_Unadjusted_P1 = ((P1-P1_Unadjusted)/(P1_Unadjusted))*100
gen PCDiff_Adjusted_P1 = ((P1-P1_Adjusted)/(P1_Adjusted))*100
gen PCDiff_Unadjusted_P2 = ((P2-P2_Unadjusted)/(P2_Unadjusted))*100
gen PCDiff_Adjusted_P2 = ((P2-P2_Adjusted)/(P2_Adjusted))*100
gen PCDiff_Unadjusted_P3 = ((P3-P3_Unadjusted)/(P3_Unadjusted))*100
gen PCDiff_Adjusted_P3 = ((P3-P3_Adjusted)/(P3_Adjusted))*100

save "$workinglocation\RIndicatorsAndCVs_`sample'", replace
save "$workinglocation\RIndicatorsAndCVs_wave`wave'_`sample'", replace

end

* Define and then call the rindicators program
capture program drop rindicators
program define rindicators
args wave

local initialwave=$initialwave

* Retrieve dataset saved above and extract current wave
clear
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'
drop wave

* Save log
capture log close
log using "$logslocation\RIndicatorsAndCVs_Log_wave`wave'" , replace

* Create tabulations of each variable by sample type and response
display in red "Frequencies for overall sample - wave `initialwave' characteristics for modelling wave `wave' response propensities"
forvalues period = 1/3 {
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		table `variable' response_P`period', miss
	}
}
drop if iw_hhtup~=0
display in red "Frequencies for Main sample - wave `initialwave' characteristics for modelling wave `wave' response propensities"
forvalues period = 1/3 {
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		table `variable' response_P`period', miss
	}
}
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'&iw_hhtup==1
display in red "Frequencies for TopUp sample - wave `initialwave' characteristics for modelling wave `wave' response propensities"
forvalues period = 1/3 {
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		table `variable' response_P`period', miss
	}
}

capture log close

* First calculate R-indicators, partial R indicators and CVs on the whole sample of initial wave respondents
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'
forvalues period = 1/3 {

	/* Calculate overall R indicators and coefficients of variation */
	xi:probit response_P`period' $predictors
	predict presponse_P`period'_allvars , pr
	sum presponse_P`period'_allvars 
	gen rind_P`period'_allvars = 1-2*r(sd)
	gen rsd_P`period' = r(sd)
	gen pbar_P`period' = r(sum)/r(N)
	gen cv_P`period'_allvars = r(sd)/pbar_P`period'
	display "R-Indicators for wave `wave' response propensities at end of P`period'"
	list rind_P`period'_allvars in 1/1
	display "CV for wave `wave' response propensities at end of P`period'"
	list cv_P`period'_allvars in 1/1

	/* Calculate variable-level conditional partial R indictors and CVs */
	
		/* Method 2 - Based on RISQ manual page 13 */
		foreach set of global predictors {
			local reducedlist = subinword("$predictors","`set'","",1)
			local variable = subinstr("`set'","i.","",1)
			local reducedlist_nois = subinstr("`reducedlist'","i.","",500)
			bys `reducedlist_nois': egen lvlmns_wo_`variable'_P`period' = mean(presponse_P`period'_allvars)
			gen cond_diff_sq_`variable'_P`period' = (presponse_P`period'_allvars - lvlmns_wo_`variable'_P`period')^2
			sum cond_diff_sq_`variable'_P`period'
			gen cprind_P`period'_`variable' = 1000*sqrt((1/r(N))*r(sum))
			gen ccv_P`period'_`variable' = cprind_P`period'_`variable'/pbar_P`period'
		}
		display "Conditional Partial R indicators (variable-level) at end of P`period' - wave `wave' response propensities"
		list cprind_P`period'_* in 1/1
		display "Conditional Coefficients of Variation (variable-level) at end of P`period' - wave `wave' response propensities"
		list ccv_P`period'_* in 1/1
		
	/* Calculate variable-level unconditional partial R indicators and CVs */
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		gen cum_sum_P`period'_`variable' = 0
		forvalues category = $`variable'_startvalue/$`variable'_endvalue {
			gen `variable'`category'_P`period'=1 if `variable'==`category'			/* Same for each period, but _P added to avoid complexity in variable generation */
			replace `variable'`category'_P`period'=0 if `variable'~=`category'		/* Same for each period, but _P added to avoid complexity in variable generation */
			sum `variable'`category'_P`period'
			gen n_`variable'`category'_P`period'=r(sum)								/* Same for each period, but _P added to avoid complexity in variable generation */
			sum presponse_P`period'_allvars if `variable'`category'_P`period'==1
			gen pbar_P`period'_`variable'`category'=r(sum)/r(N)
			sum presponse_P`period'_allvars
			replace cum_sum_P`period'_`variable'=   cum_sum_P`period'_`variable' ///
														 + (n_`variable'`category'_P`period')*((pbar_P`period'_`variable'`category'-pbar_P`period')^2)
		}
		gen uprind_P`period'_`variable'=1000*sqrt((1/r(N))*cum_sum_P`period'_`variable') 
		gen ucv_P`period'_`variable' = uprind_P`period'_`variable'/pbar_P`period'
	}
	display "Unconditional Partial R indicators (variable-level) at end of P`period' - wave `wave' response propensities"
	list uprind_P`period'_* in 1/1
	display "Unconditional Coefficients of Variation (variable-level) at end of P`period' - wave `wave' response propensities"
	list ucv_P`period'_* in 1/1
	
}



* Keep only results and transpose ready for exporting to Excel
sum rind_P1_allvars
local N=r(N)
drop in 2/`N'
keep rind* uprind* cprind* cv_* ccv* ucv*
xpose, clear varname
rename v1 Value
rename _varname TypePeriodVar
order TypePeriodVar Value

* Save results
local rows_per_fwperiod = 4*$n_predictors+2
local startP2 = `rows_per_fwperiod'+1
local endP2 = 2*`rows_per_fwperiod'
local startP3 = `endP2'+1
local endP3 = 3*`rows_per_fwperiod'
save "$workinglocation\RIndicators_MyProgram", replace
drop in `startP2'/`endP3'
rename TypePeriodVar Type_Var
rename Value P1
replace Type_Var="rind_allvars" if Type_Var=="rind_P1_allvars"
replace Type_Var="CV_allvars" if Type_Var=="cv_P1_allvars"
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	replace Type_Var="cprind_`variable'" if Type_Var=="cprind_P1_`variable'"
	replace Type_Var="uprind_`variable'" if Type_Var=="uprind_P1_`variable'"
	replace Type_Var="cpcv_`variable'" if Type_Var=="ccv_P1_`variable'"
	replace Type_Var="upcv_`variable'" if Type_Var=="ucv_P1_`variable'"
}
save "$workinglocation\RIndicators_MyProgram_P1", replace
use "$workinglocation\RIndicators_MyProgram", clear
drop in 1/`rows_per_fwperiod'
drop in `startP2'/`endP2'
rename TypePeriodVar Type_Var
rename Value P2
replace Type_Var="rind_allvars" if Type_Var=="rind_P2_allvars"
replace Type_Var="CV_allvars" if Type_Var=="cv_P2_allvars"
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	replace Type_Var="cprind_`variable'" if Type_Var=="cprind_P2_`variable'"
	replace Type_Var="uprind_`variable'" if Type_Var=="uprind_P2_`variable'"
	replace Type_Var="cpcv_`variable'" if Type_Var=="ccv_P2_`variable'"
	replace Type_Var="upcv_`variable'" if Type_Var=="ucv_P2_`variable'"
}
save "$workinglocation\RIndicators_MyProgram_P2", replace
use "$workinglocation\RIndicators_MyProgram", clear
drop in 1/`endP2'
rename TypePeriodVar Type_Var
rename Value P3
replace Type_Var="rind_allvars" if Type_Var=="rind_P3_allvars"
replace Type_Var="CV_allvars" if Type_Var=="cv_P3_allvars"
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	replace Type_Var="cprind_`variable'" if Type_Var=="cprind_P3_`variable'"
	replace Type_Var="uprind_`variable'" if Type_Var=="uprind_P3_`variable'"
	replace Type_Var="cpcv_`variable'" if Type_Var=="ccv_P3_`variable'"
	replace Type_Var="upcv_`variable'" if Type_Var=="ucv_P3_`variable'"
}
save "$workinglocation\RIndicators_MyProgram_P3", replace
use "$workinglocation\RIndicators_MyProgram_P1", clear
merge 1:1 Type_Var using "$workinglocation\RIndicators_MyProgram_P2"
drop _merge
merge 1:1 Type_Var using "$workinglocation\RIndicators_MyProgram_P3"
drop _merge
gen ordering = 1 if Type_Var=="rind_allvars"
replace ordering = 2 if Type_Var=="CV_allvars"
replace ordering = 3 if substr(Type_Var,1,6)=="cprind"
replace ordering = 4 if substr(Type_Var,1,6)=="uprind"
replace ordering = 5 if substr(Type_Var,1,4)=="cpcv"
replace ordering = 6 if substr(Type_Var,1,4)=="upcv"
sort ordering Type_Var
drop ordering 
list
save "$workinglocation\RIndicators_MyProgram",replace




* Now calculate R-indicators, partial R indicators and CVs on the main and topup samples separately

subsamples 1 `wave'
subsamples 2 `wave'

* Now combine these results with the results from the SAS RISQ program:

* For overall sample:
risqresults "P1" `wave'
risqresults "P2" `wave'
risqresults "P3" `wave'

use "$workinglocation\RIndicators_MyProgram", clear
merge 1:1 Type_Var using "$workinglocation\RIndicatorsFromRISQ_P1"
drop _merge
merge 1:1 Type_Var using "$workinglocation\RIndicatorsFromRISQ_P2"
drop _merge
merge 1:1 Type_Var using "$workinglocation\RIndicatorsFromRISQ_P3"
drop _merge

order Type_Var P1 P2 P3 P1_Unadjusted P1_Adjusted P2_Unadjusted P2_Adjusted P3_Unadjusted P3_Adjusted
gen ordering=1 if Type_Var=="rind_allvars"
replace ordering=2 if Type_Var=="CV_allvars"
replace ordering=3 if substr(Type_Var,1,6)=="cprind" & substr(Type_Var,1,10)~="cprind_cat"
replace ordering=4 if substr(Type_Var,1,6)=="uprind" & substr(Type_Var,1,10)~="uprind_cat"
replace ordering=5 if substr(Type_Var,1,4)=="cpcv" & substr(Type_Var,1,8)~="cpcv_cat"
replace ordering=6 if substr(Type_Var,1,4)=="upcv" & substr(Type_Var,1,8)~="upcv_cat"
replace ordering=7 if substr(Type_Var,1,10)=="cprind_cat"
replace ordering=8 if substr(Type_Var,1,10)=="uprind_cat"
replace ordering=9 if substr(Type_Var,1,8)=="cpcv_cat"
replace ordering=10 if substr(Type_Var,1,8)=="upcv_cat"
sort ordering Type_Var
drop ordering

gen PCDiff_Unadjusted_P1 = ((P1-P1_Unadjusted)/(P1_Unadjusted))*100
gen PCDiff_Adjusted_P1 = ((P1-P1_Adjusted)/(P1_Adjusted))*100
gen PCDiff_Unadjusted_P2 = ((P2-P2_Unadjusted)/(P2_Unadjusted))*100
gen PCDiff_Adjusted_P2 = ((P2-P2_Adjusted)/(P2_Adjusted))*100
gen PCDiff_Unadjusted_P3 = ((P3-P3_Unadjusted)/(P3_Unadjusted))*100
gen PCDiff_Adjusted_P3 = ((P3-P3_Adjusted)/(P3_Adjusted))*100

save "$workinglocation\RIndicatorsAndCVs", replace
save "$workinglocation\RIndicatorsAndCVs_wave`wave'_overall", replace

export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Results_w`wave'_Overall") sheetmodify firstrow(variables) cell(A1)

* Also export summary tables to graphing tabs of the results spreadsheet
		
drop if Type_Var ~= "rind_allvars" & Type_Var ~= "CV_allvars"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted 
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Ov_rindcv") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs", clear
drop if substr(Type_Var,1,6)~="cprind" | substr(Type_Var,1,10)=="cprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 		Type_Var == "cprind_agerange"
replace ordering = 2 if 	Type_Var == "cprind_sex"
replace ordering = 3 if 	Type_Var == "cprind_sectionofstate"
replace ordering = 4 if 	Type_Var == "cprind_countryofbirth"
replace ordering = 5 if 	Type_Var == "cprind_education"
replace ordering = 6 if 	Type_Var == "cprind_ftstudent"
replace ordering = 7 if 	Type_Var == "cprind_employment"
replace ordering = 8 if 	Type_Var == "cprind_benefitreliant"
replace ordering = 9 if 	Type_Var == "cprind_healthcondition"
replace ordering = 10 if 	Type_Var == "cprind_relationship"
replace ordering = 11 if 	Type_Var == "cprind_adultsinhh"
replace ordering = 12 if 	Type_Var == "cprind_childreninhh"
replace ordering = 13 if 	Type_Var == "cprind_owner"
replace ordering = 14 if 	Type_Var == "cprind_likelymove"
replace ordering = 15 if 	Type_Var == "cprind_topupsample"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range" 										if Type_Var == "cprind_agerange"
replace Type_Var = "Sex" 											if Type_Var == "cprind_sex"
replace Type_Var = "Section of State"								if Type_Var == "cprind_sectionofstate"
replace Type_Var = "Country of Birth" 								if Type_Var == "cprind_countryofbirth"
replace Type_Var = "Educational Attainment"							if Type_Var == "cprind_education"
replace Type_Var = "Studying Full Time"								if Type_Var == "cprind_ftstudent"
replace Type_Var = "Employment Status & Hours"						if Type_Var == "cprind_employment"
replace Type_Var = "Reliant on Government Payments"					if Type_Var == "cprind_benefitreliant"
replace Type_Var = "Degree of Health Condition"						if Type_Var == "cprind_healthcondition"
replace Type_Var = "Relationship Status"							if Type_Var == "cprind_relationship"
replace Type_Var = "Number of Adults in Household"					if Type_Var == "cprind_adultsinhh"
replace Type_Var = "Number of Children in Household"				if Type_Var == "cprind_childreninhh"
replace Type_Var = "Household owns its dwelling"					if Type_Var == "cprind_owner"
replace Type_Var = "Household likely to move in next 12 months" 	if Type_Var == "cprind_likelymove"
replace Type_Var = "Member of Top-Up Sample"						if Type_Var == "cprind_topupsample"
rename Type_Var CondPartial_RInd_VarLevel
order CondPartial_RInd_VarLevel P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Ov_condrindsvar") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs", clear
drop if substr(Type_Var,1,6)~="uprind" | substr(Type_Var,1,10)=="uprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 		Type_Var == "uprind_agerange"
replace ordering = 2 if 	Type_Var == "uprind_sex"
replace ordering = 3 if 	Type_Var == "uprind_sectionofstate"
replace ordering = 4 if 	Type_Var == "uprind_countryofbirth"
replace ordering = 5 if 	Type_Var == "uprind_education"
replace ordering = 6 if 	Type_Var == "uprind_ftstudent"
replace ordering = 7 if 	Type_Var == "uprind_employment"
replace ordering = 8 if 	Type_Var == "uprind_benefitreliant"
replace ordering = 9 if 	Type_Var == "uprind_healthcondition"
replace ordering = 10 if 	Type_Var == "uprind_relationship"
replace ordering = 11 if 	Type_Var == "uprind_adultsinhh"
replace ordering = 12 if 	Type_Var == "uprind_childreninhh"
replace ordering = 13 if 	Type_Var == "uprind_owner"
replace ordering = 14 if 	Type_Var == "uprind_likelymove"
replace ordering = 15 if 	Type_Var == "uprind_topupsample"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range" 										if Type_Var == "uprind_agerange"
replace Type_Var = "Sex" 											if Type_Var == "uprind_sex"
replace Type_Var = "Section of State"								if Type_Var == "uprind_sectionofstate"
replace Type_Var = "Country of Birth" 								if Type_Var == "uprind_countryofbirth"
replace Type_Var = "Educational Attainment"							if Type_Var == "uprind_education"
replace Type_Var = "Studying Full Time"								if Type_Var == "uprind_ftstudent"
replace Type_Var = "Employment Status & Hours"						if Type_Var == "uprind_employment"
replace Type_Var = "Reliant on Government Payments"					if Type_Var == "uprind_benefitreliant"
replace Type_Var = "Degree of Health Condition"						if Type_Var == "uprind_healthcondition"
replace Type_Var = "Relationship Status"							if Type_Var == "uprind_relationship"
replace Type_Var = "Number of Adults in Household"					if Type_Var == "uprind_adultsinhh"
replace Type_Var = "Number of Children in Household"				if Type_Var == "uprind_childreninhh"
replace Type_Var = "Household owns its dwelling"					if Type_Var == "uprind_owner"
replace Type_Var = "Household likely to move in next 12 months" 	if Type_Var == "uprind_likelymove"
replace Type_Var = "Member of Top-Up Sample"						if Type_Var == "uprind_topupsample"
rename Type_Var UncondPartial_RInd_VarLevel
order UncondPartial_RInd_VarLevel P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Ov_uncondrindsvar") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs", clear
drop if substr(Type_Var,1,10)~="cprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 			Type_Var == "cprind_cat_agerange_1"
replace ordering = 2 if 		Type_Var == "cprind_cat_agerange_2"
replace ordering = 3 if 		Type_Var == "cprind_cat_agerange_3"
replace ordering = 4 if 		Type_Var == "cprind_cat_agerange_4"
replace ordering = 5 if 		Type_Var == "cprind_cat_agerange_5"
replace ordering = 6 if 		Type_Var == "cprind_cat_agerange_6"
replace ordering = 7 if 		Type_Var == "cprind_cat_agerange_7"
replace ordering = 8 if 		Type_Var == "cprind_cat_agerange_8"
replace ordering = 9 if 		Type_Var == "cprind_cat_sex_1"
replace ordering = 10 if 		Type_Var == "cprind_cat_sex_2"
replace ordering = 11 if 		Type_Var == "cprind_cat_sectionofstate_1"
replace ordering = 12 if 		Type_Var == "cprind_cat_sectionofstate_2"
replace ordering = 13 if 		Type_Var == "cprind_cat_sectionofstate_3"
replace ordering = 14 if 		Type_Var == "cprind_cat_countryofbirth_1"
replace ordering = 15 if 		Type_Var == "cprind_cat_countryofbirth_2"
replace ordering = 16 if 		Type_Var == "cprind_cat_countryofbirth_3"
replace ordering = 17 if		Type_Var == "cprind_cat_education_1"
replace ordering = 18 if		Type_Var == "cprind_cat_education_2"
replace ordering = 19 if		Type_Var == "cprind_cat_education_3"
replace ordering = 20 if		Type_Var == "cprind_cat_education_4"
replace ordering = 21 if		Type_Var == "cprind_cat_education_5"
replace ordering = 22 if		Type_Var == "cprind_cat_education_6"
replace ordering = 23 if	 	Type_Var == "cprind_cat_ftstudent_0"
replace ordering = 24 if	 	Type_Var == "cprind_cat_ftstudent_1"
replace ordering = 25 if 		Type_Var == "cprind_cat_employment_0"
replace ordering = 26 if 		Type_Var == "cprind_cat_employment_1"
replace ordering = 27 if 		Type_Var == "cprind_cat_employment_2"
replace ordering = 28 if 		Type_Var == "cprind_cat_employment_3"
*replace ordering = 29 if 		Type_Var == "cprind_cat_employment_4"
*replace ordering = 30 if 		Type_Var == "cprind_cat_employment_5"
*replace ordering = 31 if 		Type_Var == "cprind_cat_employment_6"
replace ordering = 32 if 		Type_Var == "cprind_cat_benefitreliant_0"
replace ordering = 33 if 		Type_Var == "cprind_cat_benefitreliant_1"
replace ordering = 34 if	 	Type_Var == "cprind_cat_healthcondition_0"
replace ordering = 35 if	 	Type_Var == "cprind_cat_healthcondition_1"
replace ordering = 36 if	 	Type_Var == "cprind_cat_healthcondition_2"
replace ordering = 37 if 		Type_Var == "cprind_cat_relationship_1"
replace ordering = 38 if 		Type_Var == "cprind_cat_relationship_2"
replace ordering = 39 if 		Type_Var == "cprind_cat_relationship_3"
replace ordering = 40 if 		Type_Var == "cprind_cat_relationship_4"
replace ordering = 41 if	 	Type_Var == "cprind_cat_adultsinhh_1"
replace ordering = 42 if	 	Type_Var == "cprind_cat_adultsinhh_2"
replace ordering = 43 if	 	Type_Var == "cprind_cat_adultsinhh_3"
replace ordering = 44 if 		Type_Var == "cprind_cat_childreninhh_0"
replace ordering = 45 if 		Type_Var == "cprind_cat_childreninhh_1"
replace ordering = 46 if 		Type_Var == "cprind_cat_childreninhh_2"
replace ordering = 47 if 		Type_Var == "cprind_cat_owner_0"
replace ordering = 48 if 		Type_Var == "cprind_cat_owner_1"
replace ordering = 49 if 		Type_Var == "cprind_cat_likelymove_0"
replace ordering = 50 if 		Type_Var == "cprind_cat_likelymove_1"
replace ordering = 51 if 		Type_Var == "cprind_cat_topupsample_0"
replace ordering = 52 if 		Type_Var == "cprind_cat_topupsample_1"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range: 15 to 19 years of age" 																				if Type_Var == "cprind_cat_agerange_1"
replace Type_Var = "Age Range: 20 to 24 years of age" 																				if Type_Var == "cprind_cat_agerange_2"
replace Type_Var = "Age Range: 25 to 34 years of age" 																				if Type_Var == "cprind_cat_agerange_3"
replace Type_Var = "Age Range: 35 to 44 years of age" 																				if Type_Var == "cprind_cat_agerange_4"
replace Type_Var = "Age Range: 45 to 54 years of age" 																				if Type_Var == "cprind_cat_agerange_5"
replace Type_Var = "Age Range: 55 to 64 years of age" 																				if Type_Var == "cprind_cat_agerange_6"
replace Type_Var = "Age Range: 65 to 74 years of age" 																				if Type_Var == "cprind_cat_agerange_7"
replace Type_Var = "Age Range: 75 years or over"	 																				if Type_Var == "cprind_cat_agerange_8"
replace Type_Var = "Sex: Male"																										if Type_Var == "cprind_cat_sex_1"
replace Type_Var = "Sex: Female"																									if Type_Var == "cprind_cat_sex_2"
replace Type_Var = "Section of State: Major Urban (Pop 100,000+)"																	if Type_Var == "cprind_cat_sectionofstate_1"
replace Type_Var = "Section of State: Other Urban (Pop 1,000-99,999)"																if Type_Var == "cprind_cat_sectionofstate_2"
replace Type_Var = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if Type_Var == "cprind_cat_sectionofstate_3"
replace Type_Var = "Country of Birth: Australia"																					if Type_Var == "cprind_cat_countryofbirth_1"
replace Type_Var = "Country of Birth: Main English speaking"																		if Type_Var == "cprind_cat_countryofbirth_2"
replace Type_Var = "Country of Birth: Other countries"																				if Type_Var == "cprind_cat_countryofbirth_3"
replace Type_Var = "Educational Attainment: Degree (incl. postgrad)"																if Type_Var == "cprind_cat_education_1"
replace Type_Var = "Educational Attainment: Other higher education"																	if Type_Var == "cprind_cat_education_2"
replace Type_Var = "Educational Attainment: Year 12 (equiv. to UK A level)"															if Type_Var == "cprind_cat_education_3"
replace Type_Var = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if Type_Var == "cprind_cat_education_4"
replace Type_Var = "Educational Attainment: Other qualification"																	if Type_Var == "cprind_cat_education_5"
replace Type_Var = "Educational Attainment: No qualification" 																		if Type_Var == "cprind_cat_education_6"
replace Type_Var = "Studying Full Time: No"																							if Type_Var == "cprind_cat_ftstudent_0"
replace Type_Var = "Studying Full Time: Yes"																						if Type_Var == "cprind_cat_ftstudent_1"
replace Type_Var = "Employment Status & Hours: Not in the labour force"																if Type_Var == "cprind_cat_employment_0"
replace Type_Var = "Employment Status & Hours: Unemployed"																			if Type_Var == "cprind_cat_employment_1"
replace Type_Var = "Employment Status & Hours: Employed full-time"																	if Type_Var == "cprind_cat_employment_2"
replace Type_Var = "Employment Status & Hours: Employed part-time"																	if Type_Var == "cprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working less than 15 hours per week"										if Type_Var == "cprind_cat_employment_2"
*replace Type_Var = "Employment Status & Hours: Employed, working 15-29 hours per week"												if Type_Var == "cprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working 30-39 hours per week"												if Type_Var == "cprind_cat_employment_4"
*replace Type_Var = "Employment Status & Hours: Employed, working 40-49 hours per week"												if Type_Var == "cprind_cat_employment_5"
*replace Type_Var = "Employment Status & Hours: Employed, working 50 or more hours per week"											if Type_Var == "cprind_cat_employment_6"
replace Type_Var = "Reliant on Government Payments: No"																				if Type_Var == "cprind_cat_benefitreliant_0"
replace Type_Var = "Reliant on Government Payments: Yes"																			if Type_Var == "cprind_cat_benefitreliant_1"
replace Type_Var = "Degree of Health Condition: Does not have a long-term health condition"											if Type_Var == "cprind_cat_healthcondition_0"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if Type_Var == "cprind_cat_healthcondition_1"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if Type_Var == "cprind_cat_healthcondition_2"
replace Type_Var = "Relationship Status: Married"																					if Type_Var == "cprind_cat_relationship_1"
replace Type_Var = "Relationship Status: De facto"																					if Type_Var == "cprind_cat_relationship_2"
replace Type_Var = "Relationship Status: Separated, divorced or widowed"															if Type_Var == "cprind_cat_relationship_3"
replace Type_Var = "Relationship Status: Never married and not de facto"															if Type_Var == "cprind_cat_relationship_4"
replace Type_Var = "Number of Adults in Household: 1 adult"																			if Type_Var == "cprind_cat_adultsinhh_1"
replace Type_Var = "Number of Adults in Household: 2 adults"																		if Type_Var == "cprind_cat_adultsinhh_2"
replace Type_Var = "Number of Adults in Household: 3 or more adults"																if Type_Var == "cprind_cat_adultsinhh_3"
replace Type_Var = "Number of Children in Household: No children"																	if Type_Var == "cprind_cat_childreninhh_0"
replace Type_Var = "Number of Children in Household: 1 child"																		if Type_Var == "cprind_cat_childreninhh_1"
replace Type_Var = "Number of Children in Household: 2 or more children"															if Type_Var == "cprind_cat_childreninhh_2"
replace Type_Var = "Household owns its dwelling: No"																				if Type_Var == "cprind_cat_owner_0"
replace Type_Var = "Household owns its dwelling: Yes"																				if Type_Var == "cprind_cat_owner_1"
replace Type_Var = "Household likely to move in next 12 months: No"																	if Type_Var == "cprind_cat_likelymove_0"
replace Type_Var = "Household likely to move in next 12 months: Yes"																if Type_Var == "cprind_cat_likelymove_1"
replace Type_Var = "Member of Top-Up Sample: No"																					if Type_Var == "cprind_cat_topupsample_0"
replace Type_Var = "Member of Top-Up Sample: Yes"																					if Type_Var == "cprind_cat_topupsample_1"
rename Type_Var CondPartial_RInd_CatLevel
order CondPartial_RInd_CatLevel P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Ov_condrindscat") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs", clear
drop if substr(Type_Var,1,10)~="uprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 			Type_Var == "uprind_cat_agerange_1"
replace ordering = 2 if 		Type_Var == "uprind_cat_agerange_2"
replace ordering = 3 if 		Type_Var == "uprind_cat_agerange_3"
replace ordering = 4 if 		Type_Var == "uprind_cat_agerange_4"
replace ordering = 5 if 		Type_Var == "uprind_cat_agerange_5"
replace ordering = 6 if 		Type_Var == "uprind_cat_agerange_6"
replace ordering = 7 if 		Type_Var == "uprind_cat_agerange_7"
replace ordering = 8 if 		Type_Var == "uprind_cat_agerange_8"
replace ordering = 9 if 		Type_Var == "uprind_cat_sex_1"
replace ordering = 10 if 		Type_Var == "uprind_cat_sex_2"
replace ordering = 11 if 		Type_Var == "uprind_cat_sectionofstate_1"
replace ordering = 12 if 		Type_Var == "uprind_cat_sectionofstate_2"
replace ordering = 13 if 		Type_Var == "uprind_cat_sectionofstate_3"
replace ordering = 14 if 		Type_Var == "uprind_cat_countryofbirth_1"
replace ordering = 15 if 		Type_Var == "uprind_cat_countryofbirth_2"
replace ordering = 16 if 		Type_Var == "uprind_cat_countryofbirth_3"
replace ordering = 17 if		Type_Var == "uprind_cat_education_1"
replace ordering = 18 if		Type_Var == "uprind_cat_education_2"
replace ordering = 19 if		Type_Var == "uprind_cat_education_3"
replace ordering = 20 if		Type_Var == "uprind_cat_education_4"
replace ordering = 21 if		Type_Var == "uprind_cat_education_5"
replace ordering = 22 if		Type_Var == "uprind_cat_education_6"
replace ordering = 23 if	 	Type_Var == "uprind_cat_ftstudent_0"
replace ordering = 24 if	 	Type_Var == "uprind_cat_ftstudent_1"
replace ordering = 25 if 		Type_Var == "uprind_cat_employment_0"
replace ordering = 26 if 		Type_Var == "uprind_cat_employment_1"
replace ordering = 27 if 		Type_Var == "uprind_cat_employment_2"
replace ordering = 28 if 		Type_Var == "uprind_cat_employment_3"
*replace ordering = 29 if 		Type_Var == "uprind_cat_employment_4"
*replace ordering = 30 if 		Type_Var == "uprind_cat_employment_5"
*replace ordering = 31 if 		Type_Var == "uprind_cat_employment_6"
replace ordering = 32 if 		Type_Var == "uprind_cat_benefitreliant_0"
replace ordering = 33 if 		Type_Var == "uprind_cat_benefitreliant_1"
replace ordering = 34 if	 	Type_Var == "uprind_cat_healthcondition_0"
replace ordering = 35 if	 	Type_Var == "uprind_cat_healthcondition_1"
replace ordering = 36 if	 	Type_Var == "uprind_cat_healthcondition_2"
replace ordering = 37 if 		Type_Var == "uprind_cat_relationship_1"
replace ordering = 38 if 		Type_Var == "uprind_cat_relationship_2"
replace ordering = 39 if 		Type_Var == "uprind_cat_relationship_3"
replace ordering = 40 if 		Type_Var == "uprind_cat_relationship_4"
replace ordering = 41 if	 	Type_Var == "uprind_cat_adultsinhh_1"
replace ordering = 42 if	 	Type_Var == "uprind_cat_adultsinhh_2"
replace ordering = 43 if	 	Type_Var == "uprind_cat_adultsinhh_3"
replace ordering = 44 if 		Type_Var == "uprind_cat_childreninhh_0"
replace ordering = 45 if 		Type_Var == "uprind_cat_childreninhh_1"
replace ordering = 46 if 		Type_Var == "uprind_cat_childreninhh_2"
replace ordering = 47 if 		Type_Var == "uprind_cat_owner_0"
replace ordering = 48 if 		Type_Var == "uprind_cat_owner_1"
replace ordering = 49 if 		Type_Var == "uprind_cat_likelymove_0"
replace ordering = 50 if 		Type_Var == "uprind_cat_likelymove_1"
replace ordering = 51 if 		Type_Var == "uprind_cat_topupsample_0"
replace ordering = 52 if 		Type_Var == "uprind_cat_topupsample_1"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range: 15 to 19 years of age" 																				if Type_Var == "uprind_cat_agerange_1"
replace Type_Var = "Age Range: 20 to 24 years of age" 																				if Type_Var == "uprind_cat_agerange_2"
replace Type_Var = "Age Range: 25 to 34 years of age" 																				if Type_Var == "uprind_cat_agerange_3"
replace Type_Var = "Age Range: 35 to 44 years of age" 																				if Type_Var == "uprind_cat_agerange_4"
replace Type_Var = "Age Range: 45 to 54 years of age" 																				if Type_Var == "uprind_cat_agerange_5"
replace Type_Var = "Age Range: 55 to 64 years of age" 																				if Type_Var == "uprind_cat_agerange_6"
replace Type_Var = "Age Range: 65 to 74 years of age" 																				if Type_Var == "uprind_cat_agerange_7"
replace Type_Var = "Age Range: 75 years or over"	 																				if Type_Var == "uprind_cat_agerange_8"
replace Type_Var = "Sex: Male"																										if Type_Var == "uprind_cat_sex_1"
replace Type_Var = "Sex: Female"																									if Type_Var == "uprind_cat_sex_2"
replace Type_Var = "Section of State: Major Urban (Pop 100,000+)"																	if Type_Var == "uprind_cat_sectionofstate_1"
replace Type_Var = "Section of State: Other Urban (Pop 1,000-99,999)"																if Type_Var == "uprind_cat_sectionofstate_2"
replace Type_Var = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if Type_Var == "uprind_cat_sectionofstate_3"
replace Type_Var = "Country of Birth: Australia"																					if Type_Var == "uprind_cat_countryofbirth_1"
replace Type_Var = "Country of Birth: Main English speaking"																		if Type_Var == "uprind_cat_countryofbirth_2"
replace Type_Var = "Country of Birth: Other countries"																				if Type_Var == "uprind_cat_countryofbirth_3"
replace Type_Var = "Educational Attainment: Degree (incl. postgrad)"																if Type_Var == "uprind_cat_education_1"
replace Type_Var = "Educational Attainment: Other higher education"																	if Type_Var == "uprind_cat_education_2"
replace Type_Var = "Educational Attainment: Year 12 (equiv. to UK A level)"															if Type_Var == "uprind_cat_education_3"
replace Type_Var = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if Type_Var == "uprind_cat_education_4"
replace Type_Var = "Educational Attainment: Other qualification"																	if Type_Var == "uprind_cat_education_5"
replace Type_Var = "Educational Attainment: No qualification" 																		if Type_Var == "uprind_cat_education_6"
replace Type_Var = "Studying Full Time: No"																							if Type_Var == "uprind_cat_ftstudent_0"
replace Type_Var = "Studying Full Time: Yes"																						if Type_Var == "uprind_cat_ftstudent_1"
replace Type_Var = "Employment Status & Hours: Not in the labour force"																if Type_Var == "uprind_cat_employment_0"
replace Type_Var = "Employment Status & Hours: Unemployed"																			if Type_Var == "uprind_cat_employment_1"
replace Type_Var = "Employment Status & Hours: Employed full-time"																	if Type_Var == "uprind_cat_employment_2"
replace Type_Var = "Employment Status & Hours: Employed part-time"																	if Type_Var == "uprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working less than 15 hours per week"										if Type_Var == "uprind_cat_employment_2"
*replace Type_Var = "Employment Status & Hours: Employed, working 15-29 hours per week"												if Type_Var == "uprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working 30-39 hours per week"												if Type_Var == "uprind_cat_employment_4"
*replace Type_Var = "Employment Status & Hours: Employed, working 40-49 hours per week"												if Type_Var == "uprind_cat_employment_5"
*replace Type_Var = "Employment Status & Hours: Employed, working 50 or more hours per week"											if Type_Var == "uprind_cat_employment_6"
replace Type_Var = "Reliant on Government Payments: No"																				if Type_Var == "uprind_cat_benefitreliant_0"
replace Type_Var = "Reliant on Government Payments: Yes"																			if Type_Var == "uprind_cat_benefitreliant_1"
replace Type_Var = "Degree of Health Condition: Does not have a long-term health condition"											if Type_Var == "uprind_cat_healthcondition_0"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if Type_Var == "uprind_cat_healthcondition_1"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if Type_Var == "uprind_cat_healthcondition_2"
replace Type_Var = "Relationship Status: Married"																					if Type_Var == "uprind_cat_relationship_1"
replace Type_Var = "Relationship Status: De facto"																					if Type_Var == "uprind_cat_relationship_2"
replace Type_Var = "Relationship Status: Separated, divorced or widowed"															if Type_Var == "uprind_cat_relationship_3"
replace Type_Var = "Relationship Status: Never married and not de facto"															if Type_Var == "uprind_cat_relationship_4"
replace Type_Var = "Number of Adults in Household: 1 adult"																			if Type_Var == "uprind_cat_adultsinhh_1"
replace Type_Var = "Number of Adults in Household: 2 adults"																		if Type_Var == "uprind_cat_adultsinhh_2"
replace Type_Var = "Number of Adults in Household: 3 or more adults"																if Type_Var == "uprind_cat_adultsinhh_3"
replace Type_Var = "Number of Children in Household: No children"																	if Type_Var == "uprind_cat_childreninhh_0"
replace Type_Var = "Number of Children in Household: 1 child"																		if Type_Var == "uprind_cat_childreninhh_1"
replace Type_Var = "Number of Children in Household: 2 or more children"															if Type_Var == "uprind_cat_childreninhh_2"
replace Type_Var = "Household owns its dwelling: No"																				if Type_Var == "uprind_cat_owner_0"
replace Type_Var = "Household owns its dwelling: Yes"																				if Type_Var == "uprind_cat_owner_1"
replace Type_Var = "Household likely to move in next 12 months: No"																	if Type_Var == "uprind_cat_likelymove_0"
replace Type_Var = "Household likely to move in next 12 months: Yes"																if Type_Var == "uprind_cat_likelymove_1"
replace Type_Var = "Member of Top-Up Sample: No"																					if Type_Var == "uprind_cat_topupsample_0"
replace Type_Var = "Member of Top-Up Sample: Yes"																					if Type_Var == "uprind_cat_topupsample_1"
rename Type_Var UncondPartial_RInd_CatLevel
order UncondPartial_RInd_CatLevel P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Ov_uncondrindscat") sheetmodify firstrow(variables) cell(A1)

* For subsamples:

risqresults_subsample "P1" "Main" `wave'
risqresults_subsample "P1" "TopUp" `wave'
risqresults_subsample "P2" "Main" `wave'
risqresults_subsample "P2" "TopUp" `wave'
risqresults_subsample "P3" "Main" `wave'
risqresults_subsample "P3" "TopUp" `wave'

groupperiodresults "Main" `wave'
groupperiodresults "TopUp" `wave'

use "$workinglocation\RIndicatorsAndCVs_Main", clear
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Results_w`wave'_Main") sheetmodify firstrow(variables) cell(A1)
		
* Also export summary tables to 'FG' (for graphing) tabs of the results spreadsheet
		
drop if Type_Var ~= "rind_allvars" & Type_Var ~= "CV_allvars"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Mn_rindcv") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_Main", clear
drop if substr(Type_Var,1,6)~="cprind" | substr(Type_Var,1,10)=="cprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 		Type_Var == "cprind_agerange"
replace ordering = 2 if 	Type_Var == "cprind_sex"
replace ordering = 3 if 	Type_Var == "cprind_sectionofstate"
replace ordering = 4 if 	Type_Var == "cprind_countryofbirth"
replace ordering = 5 if 	Type_Var == "cprind_education"
replace ordering = 6 if 	Type_Var == "cprind_ftstudent"
replace ordering = 7 if 	Type_Var == "cprind_employment"
replace ordering = 8 if 	Type_Var == "cprind_benefitreliant"
replace ordering = 9 if 	Type_Var == "cprind_healthcondition"
replace ordering = 10 if 	Type_Var == "cprind_relationship"
replace ordering = 11 if 	Type_Var == "cprind_adultsinhh"
replace ordering = 12 if 	Type_Var == "cprind_childreninhh"
replace ordering = 13 if 	Type_Var == "cprind_owner"
replace ordering = 14 if 	Type_Var == "cprind_likelymove"
replace ordering = 15 if 	Type_Var == "cprind_topupsample"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range" 										if Type_Var == "cprind_agerange"
replace Type_Var = "Sex" 											if Type_Var == "cprind_sex"
replace Type_Var = "Section of State"								if Type_Var == "cprind_sectionofstate"
replace Type_Var = "Country of Birth" 								if Type_Var == "cprind_countryofbirth"
replace Type_Var = "Educational Attainment"							if Type_Var == "cprind_education"
replace Type_Var = "Studying Full Time"								if Type_Var == "cprind_ftstudent"
replace Type_Var = "Employment Status & Hours"						if Type_Var == "cprind_employment"
replace Type_Var = "Reliant on Government Payments"					if Type_Var == "cprind_benefitreliant"
replace Type_Var = "Degree of Health Condition"						if Type_Var == "cprind_healthcondition"
replace Type_Var = "Relationship Status"							if Type_Var == "cprind_relationship"
replace Type_Var = "Number of Adults in Household"					if Type_Var == "cprind_adultsinhh"
replace Type_Var = "Number of Children in Household"				if Type_Var == "cprind_childreninhh"
replace Type_Var = "Household owns its dwelling"					if Type_Var == "cprind_owner"
replace Type_Var = "Household likely to move in next 12 months" 	if Type_Var == "cprind_likelymove"
replace Type_Var = "Member of Top-Up Sample"						if Type_Var == "cprind_topupsample"
rename Type_Var CondPartial_RInd_VarLevel_Mn
order CondPartial_RInd_VarLevel_Mn P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Mn_condrindsvar") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_Main", clear
drop if substr(Type_Var,1,6)~="uprind" | substr(Type_Var,1,10)=="uprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 		Type_Var == "uprind_agerange"
replace ordering = 2 if 	Type_Var == "uprind_sex"
replace ordering = 3 if 	Type_Var == "uprind_sectionofstate"
replace ordering = 4 if 	Type_Var == "uprind_countryofbirth"
replace ordering = 5 if 	Type_Var == "uprind_education"
replace ordering = 6 if 	Type_Var == "uprind_ftstudent"
replace ordering = 7 if 	Type_Var == "uprind_employment"
replace ordering = 8 if 	Type_Var == "uprind_benefitreliant"
replace ordering = 9 if 	Type_Var == "uprind_healthcondition"
replace ordering = 10 if 	Type_Var == "uprind_relationship"
replace ordering = 11 if 	Type_Var == "uprind_adultsinhh"
replace ordering = 12 if 	Type_Var == "uprind_childreninhh"
replace ordering = 13 if 	Type_Var == "uprind_owner"
replace ordering = 14 if 	Type_Var == "uprind_likelymove"
replace ordering = 15 if 	Type_Var == "uprind_topupsample"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range" 										if Type_Var == "uprind_agerange"
replace Type_Var = "Sex" 											if Type_Var == "uprind_sex"
replace Type_Var = "Section of State"								if Type_Var == "uprind_sectionofstate"
replace Type_Var = "Country of Birth" 								if Type_Var == "uprind_countryofbirth"
replace Type_Var = "Educational Attainment"							if Type_Var == "uprind_education"
replace Type_Var = "Studying Full Time"								if Type_Var == "uprind_ftstudent"
replace Type_Var = "Employment Status & Hours"						if Type_Var == "uprind_employment"
replace Type_Var = "Reliant on Government Payments"					if Type_Var == "uprind_benefitreliant"
replace Type_Var = "Degree of Health Condition"						if Type_Var == "uprind_healthcondition"
replace Type_Var = "Relationship Status"							if Type_Var == "uprind_relationship"
replace Type_Var = "Number of Adults in Household"					if Type_Var == "uprind_adultsinhh"
replace Type_Var = "Number of Children in Household"				if Type_Var == "uprind_childreninhh"
replace Type_Var = "Household owns its dwelling"					if Type_Var == "uprind_owner"
replace Type_Var = "Household likely to move in next 12 months" 	if Type_Var == "uprind_likelymove"
replace Type_Var = "Member of Top-Up Sample"						if Type_Var == "uprind_topupsample"
rename Type_Var UncondPartial_RInd_VarLevel_Mn
order UncondPartial_RInd_VarLevel_Mn P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Mn_uncondrindsvar") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_Main", clear
drop if substr(Type_Var,1,10)~="cprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 			Type_Var == "cprind_cat_agerange_1"
replace ordering = 2 if 		Type_Var == "cprind_cat_agerange_2"
replace ordering = 3 if 		Type_Var == "cprind_cat_agerange_3"
replace ordering = 4 if 		Type_Var == "cprind_cat_agerange_4"
replace ordering = 5 if 		Type_Var == "cprind_cat_agerange_5"
replace ordering = 6 if 		Type_Var == "cprind_cat_agerange_6"
replace ordering = 7 if 		Type_Var == "cprind_cat_agerange_7"
replace ordering = 8 if 		Type_Var == "cprind_cat_agerange_8"
replace ordering = 9 if 		Type_Var == "cprind_cat_sex_1"
replace ordering = 10 if 		Type_Var == "cprind_cat_sex_2"
replace ordering = 11 if 		Type_Var == "cprind_cat_sectionofstate_1"
replace ordering = 12 if 		Type_Var == "cprind_cat_sectionofstate_2"
replace ordering = 13 if 		Type_Var == "cprind_cat_sectionofstate_3"
replace ordering = 14 if 		Type_Var == "cprind_cat_countryofbirth_1"
replace ordering = 15 if 		Type_Var == "cprind_cat_countryofbirth_2"
replace ordering = 16 if 		Type_Var == "cprind_cat_countryofbirth_3"
replace ordering = 17 if		Type_Var == "cprind_cat_education_1"
replace ordering = 18 if		Type_Var == "cprind_cat_education_2"
replace ordering = 19 if		Type_Var == "cprind_cat_education_3"
replace ordering = 20 if		Type_Var == "cprind_cat_education_4"
replace ordering = 21 if		Type_Var == "cprind_cat_education_5"
replace ordering = 22 if		Type_Var == "cprind_cat_education_6"
replace ordering = 23 if	 	Type_Var == "cprind_cat_ftstudent_0"
replace ordering = 24 if	 	Type_Var == "cprind_cat_ftstudent_1"
replace ordering = 25 if 		Type_Var == "cprind_cat_employment_0"
replace ordering = 26 if 		Type_Var == "cprind_cat_employment_1"
replace ordering = 27 if 		Type_Var == "cprind_cat_employment_2"
replace ordering = 28 if 		Type_Var == "cprind_cat_employment_3"
*replace ordering = 29 if 		Type_Var == "cprind_cat_employment_4"
*replace ordering = 30 if 		Type_Var == "cprind_cat_employment_5"
*replace ordering = 31 if 		Type_Var == "cprind_cat_employment_6"
replace ordering = 32 if 		Type_Var == "cprind_cat_benefitreliant_0"
replace ordering = 33 if 		Type_Var == "cprind_cat_benefitreliant_1"
replace ordering = 34 if	 	Type_Var == "cprind_cat_healthcondition_0"
replace ordering = 35 if	 	Type_Var == "cprind_cat_healthcondition_1"
replace ordering = 36 if	 	Type_Var == "cprind_cat_healthcondition_2"
replace ordering = 37 if 		Type_Var == "cprind_cat_relationship_1"
replace ordering = 38 if 		Type_Var == "cprind_cat_relationship_2"
replace ordering = 39 if 		Type_Var == "cprind_cat_relationship_3"
replace ordering = 40 if 		Type_Var == "cprind_cat_relationship_4"
replace ordering = 41 if	 	Type_Var == "cprind_cat_adultsinhh_1"
replace ordering = 42 if	 	Type_Var == "cprind_cat_adultsinhh_2"
replace ordering = 43 if	 	Type_Var == "cprind_cat_adultsinhh_3"
replace ordering = 44 if 		Type_Var == "cprind_cat_childreninhh_0"
replace ordering = 45 if 		Type_Var == "cprind_cat_childreninhh_1"
replace ordering = 46 if 		Type_Var == "cprind_cat_childreninhh_2"
replace ordering = 47 if 		Type_Var == "cprind_cat_owner_0"
replace ordering = 48 if 		Type_Var == "cprind_cat_owner_1"
replace ordering = 49 if 		Type_Var == "cprind_cat_likelymove_0"
replace ordering = 50 if 		Type_Var == "cprind_cat_likelymove_1"
replace ordering = 51 if 		Type_Var == "cprind_cat_topupsample_0"
replace ordering = 52 if 		Type_Var == "cprind_cat_topupsample_1"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range: 15 to 19 years of age" 																				if Type_Var == "cprind_cat_agerange_1"
replace Type_Var = "Age Range: 20 to 24 years of age" 																				if Type_Var == "cprind_cat_agerange_2"
replace Type_Var = "Age Range: 25 to 34 years of age" 																				if Type_Var == "cprind_cat_agerange_3"
replace Type_Var = "Age Range: 35 to 44 years of age" 																				if Type_Var == "cprind_cat_agerange_4"
replace Type_Var = "Age Range: 45 to 54 years of age" 																				if Type_Var == "cprind_cat_agerange_5"
replace Type_Var = "Age Range: 55 to 64 years of age" 																				if Type_Var == "cprind_cat_agerange_6"
replace Type_Var = "Age Range: 65 to 74 years of age" 																				if Type_Var == "cprind_cat_agerange_7"
replace Type_Var = "Age Range: 75 years or over"	 																				if Type_Var == "cprind_cat_agerange_8"
replace Type_Var = "Sex: Male"																										if Type_Var == "cprind_cat_sex_1"
replace Type_Var = "Sex: Female"																									if Type_Var == "cprind_cat_sex_2"
replace Type_Var = "Section of State: Major Urban (Pop 100,000+)"																	if Type_Var == "cprind_cat_sectionofstate_1"
replace Type_Var = "Section of State: Other Urban (Pop 1,000-99,999)"																if Type_Var == "cprind_cat_sectionofstate_2"
replace Type_Var = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if Type_Var == "cprind_cat_sectionofstate_3"
replace Type_Var = "Country of Birth: Australia"																					if Type_Var == "cprind_cat_countryofbirth_1"
replace Type_Var = "Country of Birth: Main English speaking"																		if Type_Var == "cprind_cat_countryofbirth_2"
replace Type_Var = "Country of Birth: Other countries"																				if Type_Var == "cprind_cat_countryofbirth_3"
replace Type_Var = "Educational Attainment: Degree (incl. postgrad)"																if Type_Var == "cprind_cat_education_1"
replace Type_Var = "Educational Attainment: Other higher education"																	if Type_Var == "cprind_cat_education_2"
replace Type_Var = "Educational Attainment: Year 12 (equiv. to UK A level)"															if Type_Var == "cprind_cat_education_3"
replace Type_Var = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if Type_Var == "cprind_cat_education_4"
replace Type_Var = "Educational Attainment: Other qualification"																	if Type_Var == "cprind_cat_education_5"
replace Type_Var = "Educational Attainment: No qualification" 																		if Type_Var == "cprind_cat_education_6"
replace Type_Var = "Studying Full Time: No"																							if Type_Var == "cprind_cat_ftstudent_0"
replace Type_Var = "Studying Full Time: Yes"																						if Type_Var == "cprind_cat_ftstudent_1"
replace Type_Var = "Employment Status & Hours: Not in the labour force"																if Type_Var == "cprind_cat_employment_0"
replace Type_Var = "Employment Status & Hours: Unemployed"																			if Type_Var == "cprind_cat_employment_1"
replace Type_Var = "Employment Status & Hours: Employed full-time"																	if Type_Var == "cprind_cat_employment_2"
replace Type_Var = "Employment Status & Hours: Employed part-time"																	if Type_Var == "cprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working less than 15 hours per week"										if Type_Var == "cprind_cat_employment_2"
*replace Type_Var = "Employment Status & Hours: Employed, working 15-29 hours per week"												if Type_Var == "cprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working 30-39 hours per week"												if Type_Var == "cprind_cat_employment_4"
*replace Type_Var = "Employment Status & Hours: Employed, working 40-49 hours per week"												if Type_Var == "cprind_cat_employment_5"
*replace Type_Var = "Employment Status & Hours: Employed, working 50 or more hours per week"											if Type_Var == "cprind_cat_employment_6"
replace Type_Var = "Reliant on Government Payments: No"																				if Type_Var == "cprind_cat_benefitreliant_0"
replace Type_Var = "Reliant on Government Payments: Yes"																			if Type_Var == "cprind_cat_benefitreliant_1"
replace Type_Var = "Degree of Health Condition: Does not have a long-term health condition"											if Type_Var == "cprind_cat_healthcondition_0"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if Type_Var == "cprind_cat_healthcondition_1"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if Type_Var == "cprind_cat_healthcondition_2"
replace Type_Var = "Relationship Status: Married"																					if Type_Var == "cprind_cat_relationship_1"
replace Type_Var = "Relationship Status: De facto"																					if Type_Var == "cprind_cat_relationship_2"
replace Type_Var = "Relationship Status: Separated, divorced or widowed"															if Type_Var == "cprind_cat_relationship_3"
replace Type_Var = "Relationship Status: Never married and not de facto"															if Type_Var == "cprind_cat_relationship_4"
replace Type_Var = "Number of Adults in Household: 1 adult"																			if Type_Var == "cprind_cat_adultsinhh_1"
replace Type_Var = "Number of Adults in Household: 2 adults"																		if Type_Var == "cprind_cat_adultsinhh_2"
replace Type_Var = "Number of Adults in Household: 3 or more adults"																if Type_Var == "cprind_cat_adultsinhh_3"
replace Type_Var = "Number of Children in Household: No children"																	if Type_Var == "cprind_cat_childreninhh_0"
replace Type_Var = "Number of Children in Household: 1 child"																		if Type_Var == "cprind_cat_childreninhh_1"
replace Type_Var = "Number of Children in Household: 2 or more children"															if Type_Var == "cprind_cat_childreninhh_2"
replace Type_Var = "Household owns its dwelling: No"																				if Type_Var == "cprind_cat_owner_0"
replace Type_Var = "Household owns its dwelling: Yes"																				if Type_Var == "cprind_cat_owner_1"
replace Type_Var = "Household likely to move in next 12 months: No"																	if Type_Var == "cprind_cat_likelymove_0"
replace Type_Var = "Household likely to move in next 12 months: Yes"																if Type_Var == "cprind_cat_likelymove_1"
replace Type_Var = "Member of Top-Up Sample: No"																					if Type_Var == "cprind_cat_topupsample_0"
replace Type_Var = "Member of Top-Up Sample: Yes"																					if Type_Var == "cprind_cat_topupsample_1"
rename Type_Var CondPartial_RInd_CatLevel_Mn
order CondPartial_RInd_CatLevel_Mn P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Mn_condrindscat") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_Main", clear
drop if substr(Type_Var,1,10)~="uprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 			Type_Var == "uprind_cat_agerange_1"
replace ordering = 2 if 		Type_Var == "uprind_cat_agerange_2"
replace ordering = 3 if 		Type_Var == "uprind_cat_agerange_3"
replace ordering = 4 if 		Type_Var == "uprind_cat_agerange_4"
replace ordering = 5 if 		Type_Var == "uprind_cat_agerange_5"
replace ordering = 6 if 		Type_Var == "uprind_cat_agerange_6"
replace ordering = 7 if 		Type_Var == "uprind_cat_agerange_7"
replace ordering = 8 if 		Type_Var == "uprind_cat_agerange_8"
replace ordering = 9 if 		Type_Var == "uprind_cat_sex_1"
replace ordering = 10 if 		Type_Var == "uprind_cat_sex_2"
replace ordering = 11 if 		Type_Var == "uprind_cat_sectionofstate_1"
replace ordering = 12 if 		Type_Var == "uprind_cat_sectionofstate_2"
replace ordering = 13 if 		Type_Var == "uprind_cat_sectionofstate_3"
replace ordering = 14 if 		Type_Var == "uprind_cat_countryofbirth_1"
replace ordering = 15 if 		Type_Var == "uprind_cat_countryofbirth_2"
replace ordering = 16 if 		Type_Var == "uprind_cat_countryofbirth_3"
replace ordering = 17 if		Type_Var == "uprind_cat_education_1"
replace ordering = 18 if		Type_Var == "uprind_cat_education_2"
replace ordering = 19 if		Type_Var == "uprind_cat_education_3"
replace ordering = 20 if		Type_Var == "uprind_cat_education_4"
replace ordering = 21 if		Type_Var == "uprind_cat_education_5"
replace ordering = 22 if		Type_Var == "uprind_cat_education_6"
replace ordering = 23 if	 	Type_Var == "uprind_cat_ftstudent_0"
replace ordering = 24 if	 	Type_Var == "uprind_cat_ftstudent_1"
replace ordering = 25 if 		Type_Var == "uprind_cat_employment_0"
replace ordering = 26 if 		Type_Var == "uprind_cat_employment_1"
replace ordering = 27 if 		Type_Var == "uprind_cat_employment_2"
replace ordering = 28 if 		Type_Var == "uprind_cat_employment_3"
*replace ordering = 29 if 		Type_Var == "uprind_cat_employment_4"
*replace ordering = 30 if 		Type_Var == "uprind_cat_employment_5"
*replace ordering = 31 if 		Type_Var == "uprind_cat_employment_6"
replace ordering = 32 if 		Type_Var == "uprind_cat_benefitreliant_0"
replace ordering = 33 if 		Type_Var == "uprind_cat_benefitreliant_1"
replace ordering = 34 if	 	Type_Var == "uprind_cat_healthcondition_0"
replace ordering = 35 if	 	Type_Var == "uprind_cat_healthcondition_1"
replace ordering = 36 if	 	Type_Var == "uprind_cat_healthcondition_2"
replace ordering = 37 if 		Type_Var == "uprind_cat_relationship_1"
replace ordering = 38 if 		Type_Var == "uprind_cat_relationship_2"
replace ordering = 39 if 		Type_Var == "uprind_cat_relationship_3"
replace ordering = 40 if 		Type_Var == "uprind_cat_relationship_4"
replace ordering = 41 if	 	Type_Var == "uprind_cat_adultsinhh_1"
replace ordering = 42 if	 	Type_Var == "uprind_cat_adultsinhh_2"
replace ordering = 43 if	 	Type_Var == "uprind_cat_adultsinhh_3"
replace ordering = 44 if 		Type_Var == "uprind_cat_childreninhh_0"
replace ordering = 45 if 		Type_Var == "uprind_cat_childreninhh_1"
replace ordering = 46 if 		Type_Var == "uprind_cat_childreninhh_2"
replace ordering = 47 if 		Type_Var == "uprind_cat_owner_0"
replace ordering = 48 if 		Type_Var == "uprind_cat_owner_1"
replace ordering = 49 if 		Type_Var == "uprind_cat_likelymove_0"
replace ordering = 50 if 		Type_Var == "uprind_cat_likelymove_1"
replace ordering = 51 if 		Type_Var == "uprind_cat_topupsample_0"
replace ordering = 52 if 		Type_Var == "uprind_cat_topupsample_1"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range: 15 to 19 years of age" 																				if Type_Var == "uprind_cat_agerange_1"
replace Type_Var = "Age Range: 20 to 24 years of age" 																				if Type_Var == "uprind_cat_agerange_2"
replace Type_Var = "Age Range: 25 to 34 years of age" 																				if Type_Var == "uprind_cat_agerange_3"
replace Type_Var = "Age Range: 35 to 44 years of age" 																				if Type_Var == "uprind_cat_agerange_4"
replace Type_Var = "Age Range: 45 to 54 years of age" 																				if Type_Var == "uprind_cat_agerange_5"
replace Type_Var = "Age Range: 55 to 64 years of age" 																				if Type_Var == "uprind_cat_agerange_6"
replace Type_Var = "Age Range: 65 to 74 years of age" 																				if Type_Var == "uprind_cat_agerange_7"
replace Type_Var = "Age Range: 75 years or over"	 																				if Type_Var == "uprind_cat_agerange_8"
replace Type_Var = "Sex: Male"																										if Type_Var == "uprind_cat_sex_1"
replace Type_Var = "Sex: Female"																									if Type_Var == "uprind_cat_sex_2"
replace Type_Var = "Section of State: Major Urban (Pop 100,000+)"																	if Type_Var == "uprind_cat_sectionofstate_1"
replace Type_Var = "Section of State: Other Urban (Pop 1,000-99,999)"																if Type_Var == "uprind_cat_sectionofstate_2"
replace Type_Var = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if Type_Var == "uprind_cat_sectionofstate_3"
replace Type_Var = "Country of Birth: Australia"																					if Type_Var == "uprind_cat_countryofbirth_1"
replace Type_Var = "Country of Birth: Main English speaking"																		if Type_Var == "uprind_cat_countryofbirth_2"
replace Type_Var = "Country of Birth: Other countries"																				if Type_Var == "uprind_cat_countryofbirth_3"
replace Type_Var = "Educational Attainment: Degree (incl. postgrad)"																if Type_Var == "uprind_cat_education_1"
replace Type_Var = "Educational Attainment: Other higher education"																	if Type_Var == "uprind_cat_education_2"
replace Type_Var = "Educational Attainment: Year 12 (equiv. to UK A level)"															if Type_Var == "uprind_cat_education_3"
replace Type_Var = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if Type_Var == "uprind_cat_education_4"
replace Type_Var = "Educational Attainment: Other qualification"																	if Type_Var == "uprind_cat_education_5"
replace Type_Var = "Educational Attainment: No qualification" 																		if Type_Var == "uprind_cat_education_6"
replace Type_Var = "Studying Full Time: No"																							if Type_Var == "uprind_cat_ftstudent_0"
replace Type_Var = "Studying Full Time: Yes"																						if Type_Var == "uprind_cat_ftstudent_1"
replace Type_Var = "Employment Status & Hours: Not in the labour force"																if Type_Var == "uprind_cat_employment_0"
replace Type_Var = "Employment Status & Hours: Unemployed"																			if Type_Var == "uprind_cat_employment_1"
replace Type_Var = "Employment Status & Hours: Employed full-time"																	if Type_Var == "uprind_cat_employment_2"
replace Type_Var = "Employment Status & Hours: Employed part-time"																	if Type_Var == "uprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working less than 15 hours per week"										if Type_Var == "uprind_cat_employment_2"
*replace Type_Var = "Employment Status & Hours: Employed, working 15-29 hours per week"												if Type_Var == "uprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working 30-39 hours per week"												if Type_Var == "uprind_cat_employment_4"
*replace Type_Var = "Employment Status & Hours: Employed, working 40-49 hours per week"												if Type_Var == "uprind_cat_employment_5"
*replace Type_Var = "Employment Status & Hours: Employed, working 50 or more hours per week"											if Type_Var == "uprind_cat_employment_6"
replace Type_Var = "Reliant on Government Payments: No"																				if Type_Var == "uprind_cat_benefitreliant_0"
replace Type_Var = "Reliant on Government Payments: Yes"																			if Type_Var == "uprind_cat_benefitreliant_1"
replace Type_Var = "Degree of Health Condition: Does not have a long-term health condition"											if Type_Var == "uprind_cat_healthcondition_0"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if Type_Var == "uprind_cat_healthcondition_1"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if Type_Var == "uprind_cat_healthcondition_2"
replace Type_Var = "Relationship Status: Married"																					if Type_Var == "uprind_cat_relationship_1"
replace Type_Var = "Relationship Status: De facto"																					if Type_Var == "uprind_cat_relationship_2"
replace Type_Var = "Relationship Status: Separated, divorced or widowed"															if Type_Var == "uprind_cat_relationship_3"
replace Type_Var = "Relationship Status: Never married and not de facto"															if Type_Var == "uprind_cat_relationship_4"
replace Type_Var = "Number of Adults in Household: 1 adult"																			if Type_Var == "uprind_cat_adultsinhh_1"
replace Type_Var = "Number of Adults in Household: 2 adults"																		if Type_Var == "uprind_cat_adultsinhh_2"
replace Type_Var = "Number of Adults in Household: 3 or more adults"																if Type_Var == "uprind_cat_adultsinhh_3"
replace Type_Var = "Number of Children in Household: No children"																	if Type_Var == "uprind_cat_childreninhh_0"
replace Type_Var = "Number of Children in Household: 1 child"																		if Type_Var == "uprind_cat_childreninhh_1"
replace Type_Var = "Number of Children in Household: 2 or more children"															if Type_Var == "uprind_cat_childreninhh_2"
replace Type_Var = "Household owns its dwelling: No"																				if Type_Var == "uprind_cat_owner_0"
replace Type_Var = "Household owns its dwelling: Yes"																				if Type_Var == "uprind_cat_owner_1"
replace Type_Var = "Household likely to move in next 12 months: No"																	if Type_Var == "uprind_cat_likelymove_0"
replace Type_Var = "Household likely to move in next 12 months: Yes"																if Type_Var == "uprind_cat_likelymove_1"
replace Type_Var = "Member of Top-Up Sample: No"																					if Type_Var == "uprind_cat_topupsample_0"
replace Type_Var = "Member of Top-Up Sample: Yes"																					if Type_Var == "uprind_cat_topupsample_1"
rename Type_Var UncondPartial_RInd_CatLevel_Mn
order UncondPartial_RInd_CatLevel_Mn P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Mn_uncondrindscat") sheetmodify firstrow(variables) cell(A1)
		
	
		
use "$workinglocation\RIndicatorsAndCVs_TopUp", clear
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Results_w`wave'_TopUp") sheetmodify firstrow(variables) cell(A1)
		
* Also export summary tables to 'FG' (for graphing) tabs of the results spreadsheet
		
drop if Type_Var ~= "rind_allvars" & Type_Var ~= "CV_allvars"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Tu_rindcv") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_TopUp", clear
drop if substr(Type_Var,1,6)~="cprind" | substr(Type_Var,1,10)=="cprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 		Type_Var == "cprind_agerange"
replace ordering = 2 if 	Type_Var == "cprind_sex"
replace ordering = 3 if 	Type_Var == "cprind_sectionofstate"
replace ordering = 4 if 	Type_Var == "cprind_countryofbirth"
replace ordering = 5 if 	Type_Var == "cprind_education"
replace ordering = 6 if 	Type_Var == "cprind_ftstudent"
replace ordering = 7 if 	Type_Var == "cprind_employment"
replace ordering = 8 if 	Type_Var == "cprind_benefitreliant"
replace ordering = 9 if 	Type_Var == "cprind_healthcondition"
replace ordering = 10 if 	Type_Var == "cprind_relationship"
replace ordering = 11 if 	Type_Var == "cprind_adultsinhh"
replace ordering = 12 if 	Type_Var == "cprind_childreninhh"
replace ordering = 13 if 	Type_Var == "cprind_owner"
replace ordering = 14 if 	Type_Var == "cprind_likelymove"
replace ordering = 15 if 	Type_Var == "cprind_topupsample"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range" 										if Type_Var == "cprind_agerange"
replace Type_Var = "Sex" 											if Type_Var == "cprind_sex"
replace Type_Var = "Section of State"								if Type_Var == "cprind_sectionofstate"
replace Type_Var = "Country of Birth" 								if Type_Var == "cprind_countryofbirth"
replace Type_Var = "Educational Attainment"							if Type_Var == "cprind_education"
replace Type_Var = "Studying Full Time"								if Type_Var == "cprind_ftstudent"
replace Type_Var = "Employment Status & Hours"						if Type_Var == "cprind_employment"
replace Type_Var = "Reliant on Government Payments"					if Type_Var == "cprind_benefitreliant"
replace Type_Var = "Degree of Health Condition"						if Type_Var == "cprind_healthcondition"
replace Type_Var = "Relationship Status"							if Type_Var == "cprind_relationship"
replace Type_Var = "Number of Adults in Household"					if Type_Var == "cprind_adultsinhh"
replace Type_Var = "Number of Children in Household"				if Type_Var == "cprind_childreninhh"
replace Type_Var = "Household owns its dwelling"					if Type_Var == "cprind_owner"
replace Type_Var = "Household likely to move in next 12 months" 	if Type_Var == "cprind_likelymove"
replace Type_Var = "Member of Top-Up Sample"						if Type_Var == "cprind_topupsample"
rename Type_Var CondPartial_RInd_VarLevel_Tu
order CondPartial_RInd_VarLevel_Tu P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Tu_condrindsvar") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_TopUp", clear
drop if substr(Type_Var,1,6)~="uprind" | substr(Type_Var,1,10)=="uprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 		Type_Var == "uprind_agerange"
replace ordering = 2 if 	Type_Var == "uprind_sex"
replace ordering = 3 if 	Type_Var == "uprind_sectionofstate"
replace ordering = 4 if 	Type_Var == "uprind_countryofbirth"
replace ordering = 5 if 	Type_Var == "uprind_education"
replace ordering = 6 if 	Type_Var == "uprind_ftstudent"
replace ordering = 7 if 	Type_Var == "uprind_employment"
replace ordering = 8 if 	Type_Var == "uprind_benefitreliant"
replace ordering = 9 if 	Type_Var == "uprind_healthcondition"
replace ordering = 10 if 	Type_Var == "uprind_relationship"
replace ordering = 11 if 	Type_Var == "uprind_adultsinhh"
replace ordering = 12 if 	Type_Var == "uprind_childreninhh"
replace ordering = 13 if 	Type_Var == "uprind_owner"
replace ordering = 14 if 	Type_Var == "uprind_likelymove"
replace ordering = 15 if 	Type_Var == "uprind_topupsample"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range" 										if Type_Var == "uprind_agerange"
replace Type_Var = "Sex" 											if Type_Var == "uprind_sex"
replace Type_Var = "Section of State"								if Type_Var == "uprind_sectionofstate"
replace Type_Var = "Country of Birth" 								if Type_Var == "uprind_countryofbirth"
replace Type_Var = "Educational Attainment"							if Type_Var == "uprind_education"
replace Type_Var = "Studying Full Time"								if Type_Var == "uprind_ftstudent"
replace Type_Var = "Employment Status & Hours"						if Type_Var == "uprind_employment"
replace Type_Var = "Reliant on Government Payments"					if Type_Var == "uprind_benefitreliant"
replace Type_Var = "Degree of Health Condition"						if Type_Var == "uprind_healthcondition"
replace Type_Var = "Relationship Status"							if Type_Var == "uprind_relationship"
replace Type_Var = "Number of Adults in Household"					if Type_Var == "uprind_adultsinhh"
replace Type_Var = "Number of Children in Household"				if Type_Var == "uprind_childreninhh"
replace Type_Var = "Household owns its dwelling"					if Type_Var == "uprind_owner"
replace Type_Var = "Household likely to move in next 12 months" 	if Type_Var == "uprind_likelymove"
replace Type_Var = "Member of Top-Up Sample"						if Type_Var == "uprind_topupsample"
rename Type_Var UncondPartial_RInd_VarLevel_Tu
order UncondPartial_RInd_VarLevel_Tu P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Tu_uncondrindsvar") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_TopUp", clear
drop if substr(Type_Var,1,10)~="cprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 			Type_Var == "cprind_cat_agerange_1"
replace ordering = 2 if 		Type_Var == "cprind_cat_agerange_2"
replace ordering = 3 if 		Type_Var == "cprind_cat_agerange_3"
replace ordering = 4 if 		Type_Var == "cprind_cat_agerange_4"
replace ordering = 5 if 		Type_Var == "cprind_cat_agerange_5"
replace ordering = 6 if 		Type_Var == "cprind_cat_agerange_6"
replace ordering = 7 if 		Type_Var == "cprind_cat_agerange_7"
replace ordering = 8 if 		Type_Var == "cprind_cat_agerange_8"
replace ordering = 9 if 		Type_Var == "cprind_cat_sex_1"
replace ordering = 10 if 		Type_Var == "cprind_cat_sex_2"
replace ordering = 11 if 		Type_Var == "cprind_cat_sectionofstate_1"
replace ordering = 12 if 		Type_Var == "cprind_cat_sectionofstate_2"
replace ordering = 13 if 		Type_Var == "cprind_cat_sectionofstate_3"
replace ordering = 14 if 		Type_Var == "cprind_cat_countryofbirth_1"
replace ordering = 15 if 		Type_Var == "cprind_cat_countryofbirth_2"
replace ordering = 16 if 		Type_Var == "cprind_cat_countryofbirth_3"
replace ordering = 17 if		Type_Var == "cprind_cat_education_1"
replace ordering = 18 if		Type_Var == "cprind_cat_education_2"
replace ordering = 19 if		Type_Var == "cprind_cat_education_3"
replace ordering = 20 if		Type_Var == "cprind_cat_education_4"
replace ordering = 21 if		Type_Var == "cprind_cat_education_5"
replace ordering = 22 if		Type_Var == "cprind_cat_education_6"
replace ordering = 23 if	 	Type_Var == "cprind_cat_ftstudent_0"
replace ordering = 24 if	 	Type_Var == "cprind_cat_ftstudent_1"
replace ordering = 25 if 		Type_Var == "cprind_cat_employment_0"
replace ordering = 26 if 		Type_Var == "cprind_cat_employment_1"
replace ordering = 27 if 		Type_Var == "cprind_cat_employment_2"
replace ordering = 28 if 		Type_Var == "cprind_cat_employment_3"
*replace ordering = 29 if 		Type_Var == "cprind_cat_employment_4"
*replace ordering = 30 if 		Type_Var == "cprind_cat_employment_5"
*replace ordering = 31 if 		Type_Var == "cprind_cat_employment_6"
replace ordering = 32 if 		Type_Var == "cprind_cat_benefitreliant_0"
replace ordering = 33 if 		Type_Var == "cprind_cat_benefitreliant_1"
replace ordering = 34 if	 	Type_Var == "cprind_cat_healthcondition_0"
replace ordering = 35 if	 	Type_Var == "cprind_cat_healthcondition_1"
replace ordering = 36 if	 	Type_Var == "cprind_cat_healthcondition_2"
replace ordering = 37 if 		Type_Var == "cprind_cat_relationship_1"
replace ordering = 38 if 		Type_Var == "cprind_cat_relationship_2"
replace ordering = 39 if 		Type_Var == "cprind_cat_relationship_3"
replace ordering = 40 if 		Type_Var == "cprind_cat_relationship_4"
replace ordering = 41 if	 	Type_Var == "cprind_cat_adultsinhh_1"
replace ordering = 42 if	 	Type_Var == "cprind_cat_adultsinhh_2"
replace ordering = 43 if	 	Type_Var == "cprind_cat_adultsinhh_3"
replace ordering = 44 if 		Type_Var == "cprind_cat_childreninhh_0"
replace ordering = 45 if 		Type_Var == "cprind_cat_childreninhh_1"
replace ordering = 46 if 		Type_Var == "cprind_cat_childreninhh_2"
replace ordering = 47 if 		Type_Var == "cprind_cat_owner_0"
replace ordering = 48 if 		Type_Var == "cprind_cat_owner_1"
replace ordering = 49 if 		Type_Var == "cprind_cat_likelymove_0"
replace ordering = 50 if 		Type_Var == "cprind_cat_likelymove_1"
replace ordering = 51 if 		Type_Var == "cprind_cat_topupsample_0"
replace ordering = 52 if 		Type_Var == "cprind_cat_topupsample_1"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range: 15 to 19 years of age" 																				if Type_Var == "cprind_cat_agerange_1"
replace Type_Var = "Age Range: 20 to 24 years of age" 																				if Type_Var == "cprind_cat_agerange_2"
replace Type_Var = "Age Range: 25 to 34 years of age" 																				if Type_Var == "cprind_cat_agerange_3"
replace Type_Var = "Age Range: 35 to 44 years of age" 																				if Type_Var == "cprind_cat_agerange_4"
replace Type_Var = "Age Range: 45 to 54 years of age" 																				if Type_Var == "cprind_cat_agerange_5"
replace Type_Var = "Age Range: 55 to 64 years of age" 																				if Type_Var == "cprind_cat_agerange_6"
replace Type_Var = "Age Range: 65 to 74 years of age" 																				if Type_Var == "cprind_cat_agerange_7"
replace Type_Var = "Age Range: 75 years or over"	 																				if Type_Var == "cprind_cat_agerange_8"
replace Type_Var = "Sex: Male"																										if Type_Var == "cprind_cat_sex_1"
replace Type_Var = "Sex: Female"																									if Type_Var == "cprind_cat_sex_2"
replace Type_Var = "Section of State: Major Urban (Pop 100,000+)"																	if Type_Var == "cprind_cat_sectionofstate_1"
replace Type_Var = "Section of State: Other Urban (Pop 1,000-99,999)"																if Type_Var == "cprind_cat_sectionofstate_2"
replace Type_Var = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if Type_Var == "cprind_cat_sectionofstate_3"
replace Type_Var = "Country of Birth: Australia"																					if Type_Var == "cprind_cat_countryofbirth_1"
replace Type_Var = "Country of Birth: Main English speaking"																		if Type_Var == "cprind_cat_countryofbirth_2"
replace Type_Var = "Country of Birth: Other countries"																				if Type_Var == "cprind_cat_countryofbirth_3"
replace Type_Var = "Educational Attainment: Degree (incl. postgrad)"																if Type_Var == "cprind_cat_education_1"
replace Type_Var = "Educational Attainment: Other higher education"																	if Type_Var == "cprind_cat_education_2"
replace Type_Var = "Educational Attainment: Year 12 (equiv. to UK A level)"															if Type_Var == "cprind_cat_education_3"
replace Type_Var = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if Type_Var == "cprind_cat_education_4"
replace Type_Var = "Educational Attainment: Other qualification"																	if Type_Var == "cprind_cat_education_5"
replace Type_Var = "Educational Attainment: No qualification" 																		if Type_Var == "cprind_cat_education_6"
replace Type_Var = "Studying Full Time: No"																							if Type_Var == "cprind_cat_ftstudent_0"
replace Type_Var = "Studying Full Time: Yes"																						if Type_Var == "cprind_cat_ftstudent_1"
replace Type_Var = "Employment Status & Hours: Not in the labour force"																if Type_Var == "cprind_cat_employment_0"
replace Type_Var = "Employment Status & Hours: Unemployed"																			if Type_Var == "cprind_cat_employment_1"
replace Type_Var = "Employment Status & Hours: Employed full-time"																	if Type_Var == "cprind_cat_employment_2"
replace Type_Var = "Employment Status & Hours: Employed part-time"																	if Type_Var == "cprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working less than 15 hours per week"										if Type_Var == "cprind_cat_employment_2"
*replace Type_Var = "Employment Status & Hours: Employed, working 15-29 hours per week"												if Type_Var == "cprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working 30-39 hours per week"												if Type_Var == "cprind_cat_employment_4"
*replace Type_Var = "Employment Status & Hours: Employed, working 40-49 hours per week"												if Type_Var == "cprind_cat_employment_5"
*replace Type_Var = "Employment Status & Hours: Employed, working 50 or more hours per week"											if Type_Var == "cprind_cat_employment_6"
replace Type_Var = "Reliant on Government Payments: No"																				if Type_Var == "cprind_cat_benefitreliant_0"
replace Type_Var = "Reliant on Government Payments: Yes"																			if Type_Var == "cprind_cat_benefitreliant_1"
replace Type_Var = "Degree of Health Condition: Does not have a long-term health condition"											if Type_Var == "cprind_cat_healthcondition_0"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if Type_Var == "cprind_cat_healthcondition_1"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if Type_Var == "cprind_cat_healthcondition_2"
replace Type_Var = "Relationship Status: Married"																					if Type_Var == "cprind_cat_relationship_1"
replace Type_Var = "Relationship Status: De facto"																					if Type_Var == "cprind_cat_relationship_2"
replace Type_Var = "Relationship Status: Separated, divorced or widowed"															if Type_Var == "cprind_cat_relationship_3"
replace Type_Var = "Relationship Status: Never married and not de facto"															if Type_Var == "cprind_cat_relationship_4"
replace Type_Var = "Number of Adults in Household: 1 adult"																			if Type_Var == "cprind_cat_adultsinhh_1"
replace Type_Var = "Number of Adults in Household: 2 adults"																		if Type_Var == "cprind_cat_adultsinhh_2"
replace Type_Var = "Number of Adults in Household: 3 or more adults"																if Type_Var == "cprind_cat_adultsinhh_3"
replace Type_Var = "Number of Children in Household: No children"																	if Type_Var == "cprind_cat_childreninhh_0"
replace Type_Var = "Number of Children in Household: 1 child"																		if Type_Var == "cprind_cat_childreninhh_1"
replace Type_Var = "Number of Children in Household: 2 or more children"															if Type_Var == "cprind_cat_childreninhh_2"
replace Type_Var = "Household owns its dwelling: No"																				if Type_Var == "cprind_cat_owner_0"
replace Type_Var = "Household owns its dwelling: Yes"																				if Type_Var == "cprind_cat_owner_1"
replace Type_Var = "Household likely to move in next 12 months: No"																	if Type_Var == "cprind_cat_likelymove_0"
replace Type_Var = "Household likely to move in next 12 months: Yes"																if Type_Var == "cprind_cat_likelymove_1"
replace Type_Var = "Member of Top-Up Sample: No"																					if Type_Var == "cprind_cat_topupsample_0"
replace Type_Var = "Member of Top-Up Sample: Yes"																					if Type_Var == "cprind_cat_topupsample_1"
rename Type_Var CondPartial_RInd_CatLevel_Tu
order CondPartial_RInd_CatLevel_Tu P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Tu_condrindscat") sheetmodify firstrow(variables) cell(A1)

use "$workinglocation\RIndicatorsAndCVs_TopUp", clear
drop if substr(Type_Var,1,10)~="uprind_cat"
keep Type_Var P1_Adjusted P2_Adjusted P3_Adjusted
rename P1_Adjusted P1
rename P2_Adjusted P2
rename P3_Adjusted P3
gen ordering = 1 if 			Type_Var == "uprind_cat_agerange_1"
replace ordering = 2 if 		Type_Var == "uprind_cat_agerange_2"
replace ordering = 3 if 		Type_Var == "uprind_cat_agerange_3"
replace ordering = 4 if 		Type_Var == "uprind_cat_agerange_4"
replace ordering = 5 if 		Type_Var == "uprind_cat_agerange_5"
replace ordering = 6 if 		Type_Var == "uprind_cat_agerange_6"
replace ordering = 7 if 		Type_Var == "uprind_cat_agerange_7"
replace ordering = 8 if 		Type_Var == "uprind_cat_agerange_8"
replace ordering = 9 if 		Type_Var == "uprind_cat_sex_1"
replace ordering = 10 if 		Type_Var == "uprind_cat_sex_2"
replace ordering = 11 if 		Type_Var == "uprind_cat_sectionofstate_1"
replace ordering = 12 if 		Type_Var == "uprind_cat_sectionofstate_2"
replace ordering = 13 if 		Type_Var == "uprind_cat_sectionofstate_3"
replace ordering = 14 if 		Type_Var == "uprind_cat_countryofbirth_1"
replace ordering = 15 if 		Type_Var == "uprind_cat_countryofbirth_2"
replace ordering = 16 if 		Type_Var == "uprind_cat_countryofbirth_3"
replace ordering = 17 if		Type_Var == "uprind_cat_education_1"
replace ordering = 18 if		Type_Var == "uprind_cat_education_2"
replace ordering = 19 if		Type_Var == "uprind_cat_education_3"
replace ordering = 20 if		Type_Var == "uprind_cat_education_4"
replace ordering = 21 if		Type_Var == "uprind_cat_education_5"
replace ordering = 22 if		Type_Var == "uprind_cat_education_6"
replace ordering = 23 if	 	Type_Var == "uprind_cat_ftstudent_0"
replace ordering = 24 if	 	Type_Var == "uprind_cat_ftstudent_1"
replace ordering = 25 if 		Type_Var == "uprind_cat_employment_0"
replace ordering = 26 if 		Type_Var == "uprind_cat_employment_1"
replace ordering = 27 if 		Type_Var == "uprind_cat_employment_2"
replace ordering = 28 if 		Type_Var == "uprind_cat_employment_3"
*replace ordering = 29 if 		Type_Var == "uprind_cat_employment_4"
*replace ordering = 30 if 		Type_Var == "uprind_cat_employment_5"
*replace ordering = 31 if 		Type_Var == "uprind_cat_employment_6"
replace ordering = 32 if 		Type_Var == "uprind_cat_benefitreliant_0"
replace ordering = 33 if 		Type_Var == "uprind_cat_benefitreliant_1"
replace ordering = 34 if	 	Type_Var == "uprind_cat_healthcondition_0"
replace ordering = 35 if	 	Type_Var == "uprind_cat_healthcondition_1"
replace ordering = 36 if	 	Type_Var == "uprind_cat_healthcondition_2"
replace ordering = 37 if 		Type_Var == "uprind_cat_relationship_1"
replace ordering = 38 if 		Type_Var == "uprind_cat_relationship_2"
replace ordering = 39 if 		Type_Var == "uprind_cat_relationship_3"
replace ordering = 40 if 		Type_Var == "uprind_cat_relationship_4"
replace ordering = 41 if	 	Type_Var == "uprind_cat_adultsinhh_1"
replace ordering = 42 if	 	Type_Var == "uprind_cat_adultsinhh_2"
replace ordering = 43 if	 	Type_Var == "uprind_cat_adultsinhh_3"
replace ordering = 44 if 		Type_Var == "uprind_cat_childreninhh_0"
replace ordering = 45 if 		Type_Var == "uprind_cat_childreninhh_1"
replace ordering = 46 if 		Type_Var == "uprind_cat_childreninhh_2"
replace ordering = 47 if 		Type_Var == "uprind_cat_owner_0"
replace ordering = 48 if 		Type_Var == "uprind_cat_owner_1"
replace ordering = 49 if 		Type_Var == "uprind_cat_likelymove_0"
replace ordering = 50 if 		Type_Var == "uprind_cat_likelymove_1"
replace ordering = 51 if 		Type_Var == "uprind_cat_topupsample_0"
replace ordering = 52 if 		Type_Var == "uprind_cat_topupsample_1"
gsort -ordering Type_Var
drop ordering
replace Type_Var = "Age Range: 15 to 19 years of age" 																				if Type_Var == "uprind_cat_agerange_1"
replace Type_Var = "Age Range: 20 to 24 years of age" 																				if Type_Var == "uprind_cat_agerange_2"
replace Type_Var = "Age Range: 25 to 34 years of age" 																				if Type_Var == "uprind_cat_agerange_3"
replace Type_Var = "Age Range: 35 to 44 years of age" 																				if Type_Var == "uprind_cat_agerange_4"
replace Type_Var = "Age Range: 45 to 54 years of age" 																				if Type_Var == "uprind_cat_agerange_5"
replace Type_Var = "Age Range: 55 to 64 years of age" 																				if Type_Var == "uprind_cat_agerange_6"
replace Type_Var = "Age Range: 65 to 74 years of age" 																				if Type_Var == "uprind_cat_agerange_7"
replace Type_Var = "Age Range: 75 years or over"	 																				if Type_Var == "uprind_cat_agerange_8"
replace Type_Var = "Sex: Male"																										if Type_Var == "uprind_cat_sex_1"
replace Type_Var = "Sex: Female"																									if Type_Var == "uprind_cat_sex_2"
replace Type_Var = "Section of State: Major Urban (Pop 100,000+)"																	if Type_Var == "uprind_cat_sectionofstate_1"
replace Type_Var = "Section of State: Other Urban (Pop 1,000-99,999)"																if Type_Var == "uprind_cat_sectionofstate_2"
replace Type_Var = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if Type_Var == "uprind_cat_sectionofstate_3"
replace Type_Var = "Country of Birth: Australia"																					if Type_Var == "uprind_cat_countryofbirth_1"
replace Type_Var = "Country of Birth: Main English speaking"																		if Type_Var == "uprind_cat_countryofbirth_2"
replace Type_Var = "Country of Birth: Other countries"																				if Type_Var == "uprind_cat_countryofbirth_3"
replace Type_Var = "Educational Attainment: Degree (incl. postgrad)"																if Type_Var == "uprind_cat_education_1"
replace Type_Var = "Educational Attainment: Other higher education"																	if Type_Var == "uprind_cat_education_2"
replace Type_Var = "Educational Attainment: Year 12 (equiv. to UK A level)"															if Type_Var == "uprind_cat_education_3"
replace Type_Var = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if Type_Var == "uprind_cat_education_4"
replace Type_Var = "Educational Attainment: Other qualification"																	if Type_Var == "uprind_cat_education_5"
replace Type_Var = "Educational Attainment: No qualification" 																		if Type_Var == "uprind_cat_education_6"
replace Type_Var = "Studying Full Time: No"																							if Type_Var == "uprind_cat_ftstudent_0"
replace Type_Var = "Studying Full Time: Yes"																						if Type_Var == "uprind_cat_ftstudent_1"
replace Type_Var = "Employment Status & Hours: Not in the labour force"																if Type_Var == "uprind_cat_employment_0"
replace Type_Var = "Employment Status & Hours: Unemployed"																			if Type_Var == "uprind_cat_employment_1"
replace Type_Var = "Employment Status & Hours: Employed full-time"																	if Type_Var == "uprind_cat_employment_2"
replace Type_Var = "Employment Status & Hours: Employed part-time" 																	if Type_Var == "uprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working less than 15 hours per week"										if Type_Var == "uprind_cat_employment_2"
*replace Type_Var = "Employment Status & Hours: Employed, working 15-29 hours per week"												if Type_Var == "uprind_cat_employment_3"
*replace Type_Var = "Employment Status & Hours: Employed, working 30-39 hours per week"												if Type_Var == "uprind_cat_employment_4"
*replace Type_Var = "Employment Status & Hours: Employed, working 40-49 hours per week"												if Type_Var == "uprind_cat_employment_5"
*replace Type_Var = "Employment Status & Hours: Employed, working 50 or more hours per week"											if Type_Var == "uprind_cat_employment_6"
replace Type_Var = "Reliant on Government Payments: No"																				if Type_Var == "uprind_cat_benefitreliant_0"
replace Type_Var = "Reliant on Government Payments: Yes"																			if Type_Var == "uprind_cat_benefitreliant_1"
replace Type_Var = "Degree of Health Condition: Does not have a long-term health condition"											if Type_Var == "uprind_cat_healthcondition_0"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if Type_Var == "uprind_cat_healthcondition_1"
replace Type_Var = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if Type_Var == "uprind_cat_healthcondition_2"
replace Type_Var = "Relationship Status: Married"																					if Type_Var == "uprind_cat_relationship_1"
replace Type_Var = "Relationship Status: De facto"																					if Type_Var == "uprind_cat_relationship_2"
replace Type_Var = "Relationship Status: Separated, divorced or widowed"															if Type_Var == "uprind_cat_relationship_3"
replace Type_Var = "Relationship Status: Never married and not de facto"															if Type_Var == "uprind_cat_relationship_4"
replace Type_Var = "Number of Adults in Household: 1 adult"																			if Type_Var == "uprind_cat_adultsinhh_1"
replace Type_Var = "Number of Adults in Household: 2 adults"																		if Type_Var == "uprind_cat_adultsinhh_2"
replace Type_Var = "Number of Adults in Household: 3 or more adults"																if Type_Var == "uprind_cat_adultsinhh_3"
replace Type_Var = "Number of Children in Household: No children"																	if Type_Var == "uprind_cat_childreninhh_0"
replace Type_Var = "Number of Children in Household: 1 child"																		if Type_Var == "uprind_cat_childreninhh_1"
replace Type_Var = "Number of Children in Household: 2 or more children"															if Type_Var == "uprind_cat_childreninhh_2"
replace Type_Var = "Household owns its dwelling: No"																				if Type_Var == "uprind_cat_owner_0"
replace Type_Var = "Household owns its dwelling: Yes"																				if Type_Var == "uprind_cat_owner_1"
replace Type_Var = "Household likely to move in next 12 months: No"																	if Type_Var == "uprind_cat_likelymove_0"
replace Type_Var = "Household likely to move in next 12 months: Yes"																if Type_Var == "uprind_cat_likelymove_1"
replace Type_Var = "Member of Top-Up Sample: No"																					if Type_Var == "uprind_cat_topupsample_0"
replace Type_Var = "Member of Top-Up Sample: Yes"																					if Type_Var == "uprind_cat_topupsample_1"
rename Type_Var UncondPartial_RInd_CatLevel_Tu
order UncondPartial_RInd_CatLevel_Tu P3 P2 P1
export  excel "$resultslocation\Phase 2 - R-Indicators and CVs - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Graph_w`wave'_Tu_uncondrindscat") sheetmodify firstrow(variables) cell(A1)
		
*********************************************************************************************************************************************************************************
end
forvalues wave=$startwave/$endwave {
	rindicators `wave'
}






capture program drop groupwavegraphs
program define groupwavegraphs
args period
*** Additional graphs showing P1 for all waves together
* Conditional partials - variable level - Main
forvalues wave=$startwave/$endwave {
	use "$workinglocation\RIndicatorsAndCVs_wave`wave'_Main", clear
	keep if substr(Type_Var,1,6)=="cprind" & substr(Type_Var,1,10)~="cprind_cat"
	keep Type_Var P`period'_Adjusted
	rename Type_Var cprind_var
	rename P`period'_Adjusted Wave_`wave'
	sort cprind_var
	save "$workinglocation\interim_wave`wave'", replace
}
use "$workinglocation\interim_wave12", clear
merge 1:1 cprind_var using "$workinglocation\interim_wave13"
drop _merge
merge 1:1 cprind_var using "$workinglocation\interim_wave14"
drop _merge
merge 1:1 cprind_var using "$workinglocation\interim_wave15"
drop _merge
merge 1:1 cprind_var using "$workinglocation\interim_wave16"
drop _merge
gen ordering = 1 if 		cprind_var == "cprind_agerange"
replace ordering = 2 if 	cprind_var == "cprind_sex"
replace ordering = 3 if 	cprind_var == "cprind_sectionofstate"
replace ordering = 4 if 	cprind_var == "cprind_countryofbirth"
replace ordering = 5 if 	cprind_var == "cprind_education"
replace ordering = 6 if 	cprind_var == "cprind_ftstudent"
replace ordering = 7 if 	cprind_var == "cprind_employment"
replace ordering = 8 if 	cprind_var == "cprind_benefitreliant"
replace ordering = 9 if 	cprind_var == "cprind_healthcondition"
replace ordering = 10 if 	cprind_var == "cprind_relationship"
replace ordering = 11 if 	cprind_var == "cprind_adultsinhh"
replace ordering = 12 if 	cprind_var == "cprind_childreninhh"
replace ordering = 13 if 	cprind_var == "cprind_owner"
replace ordering = 14 if 	cprind_var == "cprind_likelymove"
replace ordering = 15 if 	cprind_var == "cprind_topupsample"
gsort -ordering 
drop ordering
replace cprind_var = "Age Range" 										if cprind_var == "cprind_agerange"
replace cprind_var = "Sex" 											if cprind_var == "cprind_sex"
replace cprind_var = "Section of State"								if cprind_var == "cprind_sectionofstate"
replace cprind_var = "Country of Birth" 								if cprind_var == "cprind_countryofbirth"
replace cprind_var = "Educational Attainment"							if cprind_var == "cprind_education"
replace cprind_var = "Studying Full Time"								if cprind_var == "cprind_ftstudent"
replace cprind_var = "Employment Status & Hours"						if cprind_var == "cprind_employment"
replace cprind_var = "Reliant on Government Payments"					if cprind_var == "cprind_benefitreliant"
replace cprind_var = "Degree of Health Condition"						if cprind_var == "cprind_healthcondition"
replace cprind_var = "Relationship Status"							if cprind_var == "cprind_relationship"
replace cprind_var = "Number of Adults in Household"					if cprind_var == "cprind_adultsinhh"
replace cprind_var = "Number of Children in Household"				if cprind_var == "cprind_childreninhh"
replace cprind_var = "Household owns its dwelling"					if cprind_var == "cprind_owner"
replace cprind_var = "Household likely to move in next 12 months" 	if cprind_var == "cprind_likelymove"
replace cprind_var = "Member of Top-Up Sample"						if cprind_var == "cprind_topupsample"
order cprind_var Wave_16 Wave_15 Wave_14 Wave_13 Wave_12
save "$workinglocation\conditionals_varlevel_Main_period`period'", replace
list
export  excel "$resultslocation\Phase 3 - Results grouped across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Conditional, Var, Main, P`period'") sheetmodify firstrow(variables) cell(A1)
		
* Conditional partials - variable level - TopUp
forvalues wave=$startwave/$endwave {
	use "$workinglocation\RIndicatorsAndCVs_wave`wave'_TopUp", clear
	keep if substr(Type_Var,1,6)=="cprind" & substr(Type_Var,1,10)~="cprind_cat"
	keep Type_Var P`period'_Adjusted
	rename Type_Var cprind_var
	rename P`period'_Adjusted Wave_`wave'
	sort cprind_var
	save "$workinglocation\interim_wave`wave'", replace
}
use "$workinglocation\interim_wave12", clear
merge 1:1 cprind_var using "$workinglocation\interim_wave13"
drop _merge
merge 1:1 cprind_var using "$workinglocation\interim_wave14"
drop _merge
merge 1:1 cprind_var using "$workinglocation\interim_wave15"
drop _merge
merge 1:1 cprind_var using "$workinglocation\interim_wave16"
drop _merge
gen ordering = 1 if 		cprind_var == "cprind_agerange"
replace ordering = 2 if 	cprind_var == "cprind_sex"
replace ordering = 3 if 	cprind_var == "cprind_sectionofstate"
replace ordering = 4 if 	cprind_var == "cprind_countryofbirth"
replace ordering = 5 if 	cprind_var == "cprind_education"
replace ordering = 6 if 	cprind_var == "cprind_ftstudent"
replace ordering = 7 if 	cprind_var == "cprind_employment"
replace ordering = 8 if 	cprind_var == "cprind_benefitreliant"
replace ordering = 9 if 	cprind_var == "cprind_healthcondition"
replace ordering = 10 if 	cprind_var == "cprind_relationship"
replace ordering = 11 if 	cprind_var == "cprind_adultsinhh"
replace ordering = 12 if 	cprind_var == "cprind_childreninhh"
replace ordering = 13 if 	cprind_var == "cprind_owner"
replace ordering = 14 if 	cprind_var == "cprind_likelymove"
replace ordering = 15 if 	cprind_var == "cprind_topupsample"
gsort -ordering 
drop ordering
replace cprind_var = "Age Range" 										if cprind_var == "cprind_agerange"
replace cprind_var = "Sex" 											if cprind_var == "cprind_sex"
replace cprind_var = "Section of State"								if cprind_var == "cprind_sectionofstate"
replace cprind_var = "Country of Birth" 								if cprind_var == "cprind_countryofbirth"
replace cprind_var = "Educational Attainment"							if cprind_var == "cprind_education"
replace cprind_var = "Studying Full Time"								if cprind_var == "cprind_ftstudent"
replace cprind_var = "Employment Status & Hours"						if cprind_var == "cprind_employment"
replace cprind_var = "Reliant on Government Payments"					if cprind_var == "cprind_benefitreliant"
replace cprind_var = "Degree of Health Condition"						if cprind_var == "cprind_healthcondition"
replace cprind_var = "Relationship Status"							if cprind_var == "cprind_relationship"
replace cprind_var = "Number of Adults in Household"					if cprind_var == "cprind_adultsinhh"
replace cprind_var = "Number of Children in Household"				if cprind_var == "cprind_childreninhh"
replace cprind_var = "Household owns its dwelling"					if cprind_var == "cprind_owner"
replace cprind_var = "Household likely to move in next 12 months" 	if cprind_var == "cprind_likelymove"
replace cprind_var = "Member of Top-Up Sample"						if cprind_var == "cprind_topupsample"
order cprind_var Wave_16 Wave_15 Wave_14 Wave_13 Wave_12
save "$workinglocation\conditionals_varlevel_TopUp_period`period'", replace
list
export  excel "$resultslocation\Phase 3 - Results grouped across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Conditional, Var, TopUp, P`period'") sheetmodify firstrow(variables) cell(A1)

* Conditional partials - category level - Main
forvalues wave=$startwave/$endwave {
	use "$workinglocation\RIndicatorsAndCVs_wave`wave'_Main", clear
	keep if substr(Type_Var,1,10)=="cprind_cat"
	keep Type_Var P`period'_Adjusted
	rename Type_Var cprind_cat
	rename P`period'_Adjusted Wave_`wave'
	sort cprind_cat
	save "$workinglocation\interim_wave`wave'", replace
}
use "$workinglocation\interim_wave12", clear
merge 1:1 cprind_cat using "$workinglocation\interim_wave13"
drop _merge
merge 1:1 cprind_cat using "$workinglocation\interim_wave14"
drop _merge
merge 1:1 cprind_cat using "$workinglocation\interim_wave15"
drop _merge
merge 1:1 cprind_cat using "$workinglocation\interim_wave16"
drop _merge
gen ordering = 1 if 			cprind_cat == "cprind_cat_agerange_1"
replace ordering = 2 if 		cprind_cat == "cprind_cat_agerange_2"
replace ordering = 3 if 		cprind_cat == "cprind_cat_agerange_3"
replace ordering = 4 if 		cprind_cat == "cprind_cat_agerange_4"
replace ordering = 5 if 		cprind_cat == "cprind_cat_agerange_5"
replace ordering = 6 if 		cprind_cat == "cprind_cat_agerange_6"
replace ordering = 7 if 		cprind_cat == "cprind_cat_agerange_7"
replace ordering = 8 if 		cprind_cat == "cprind_cat_agerange_8"
replace ordering = 9 if 		cprind_cat == "cprind_cat_sex_1"
replace ordering = 10 if 		cprind_cat == "cprind_cat_sex_2"
replace ordering = 11 if 		cprind_cat == "cprind_cat_sectionofstate_1"
replace ordering = 12 if 		cprind_cat == "cprind_cat_sectionofstate_2"
replace ordering = 13 if 		cprind_cat == "cprind_cat_sectionofstate_3"
replace ordering = 14 if 		cprind_cat == "cprind_cat_countryofbirth_1"
replace ordering = 15 if 		cprind_cat == "cprind_cat_countryofbirth_2"
replace ordering = 16 if 		cprind_cat == "cprind_cat_countryofbirth_3"
replace ordering = 17 if		cprind_cat == "cprind_cat_education_1"
replace ordering = 18 if		cprind_cat == "cprind_cat_education_2"
replace ordering = 19 if		cprind_cat == "cprind_cat_education_3"
replace ordering = 20 if		cprind_cat == "cprind_cat_education_4"
replace ordering = 21 if		cprind_cat == "cprind_cat_education_5"
replace ordering = 22 if		cprind_cat == "cprind_cat_education_6"
replace ordering = 23 if	 	cprind_cat == "cprind_cat_ftstudent_0"
replace ordering = 24 if	 	cprind_cat == "cprind_cat_ftstudent_1"
replace ordering = 25 if 		cprind_cat == "cprind_cat_employment_0"
replace ordering = 26 if 		cprind_cat == "cprind_cat_employment_1"
replace ordering = 27 if 		cprind_cat == "cprind_cat_employment_2"
replace ordering = 28 if 		cprind_cat == "cprind_cat_employment_3"
*replace ordering = 29 if 		cprind_cat == "cprind_cat_employment_4"
*replace ordering = 30 if 		cprind_cat == "cprind_cat_employment_5"
*replace ordering = 31 if 		cprind_cat == "cprind_cat_employment_6"
replace ordering = 32 if 		cprind_cat == "cprind_cat_benefitreliant_0"
replace ordering = 33 if 		cprind_cat == "cprind_cat_benefitreliant_1"
replace ordering = 34 if	 	cprind_cat == "cprind_cat_healthcondition_0"
replace ordering = 35 if	 	cprind_cat == "cprind_cat_healthcondition_1"
replace ordering = 36 if	 	cprind_cat == "cprind_cat_healthcondition_2"
replace ordering = 37 if 		cprind_cat == "cprind_cat_relationship_1"
replace ordering = 38 if 		cprind_cat == "cprind_cat_relationship_2"
replace ordering = 39 if 		cprind_cat == "cprind_cat_relationship_3"
replace ordering = 40 if 		cprind_cat == "cprind_cat_relationship_4"
replace ordering = 41 if	 	cprind_cat == "cprind_cat_adultsinhh_1"
replace ordering = 42 if	 	cprind_cat == "cprind_cat_adultsinhh_2"
replace ordering = 43 if	 	cprind_cat == "cprind_cat_adultsinhh_3"
replace ordering = 44 if 		cprind_cat == "cprind_cat_childreninhh_0"
replace ordering = 45 if 		cprind_cat == "cprind_cat_childreninhh_1"
replace ordering = 46 if 		cprind_cat == "cprind_cat_childreninhh_2"
replace ordering = 47 if 		cprind_cat == "cprind_cat_owner_0"
replace ordering = 48 if 		cprind_cat == "cprind_cat_owner_1"
replace ordering = 49 if 		cprind_cat == "cprind_cat_likelymove_0"
replace ordering = 50 if 		cprind_cat == "cprind_cat_likelymove_1"
replace ordering = 51 if 		cprind_cat == "cprind_cat_topupsample_0"
replace ordering = 52 if 		cprind_cat == "cprind_cat_topupsample_1"
gsort -ordering cprind_cat
drop ordering
replace cprind_cat = "Age Range: 15 to 19 years of age" 																				if cprind_cat == "cprind_cat_agerange_1"
replace cprind_cat = "Age Range: 20 to 24 years of age" 																				if cprind_cat == "cprind_cat_agerange_2"
replace cprind_cat = "Age Range: 25 to 34 years of age" 																				if cprind_cat == "cprind_cat_agerange_3"
replace cprind_cat = "Age Range: 35 to 44 years of age" 																				if cprind_cat == "cprind_cat_agerange_4"
replace cprind_cat = "Age Range: 45 to 54 years of age" 																				if cprind_cat == "cprind_cat_agerange_5"
replace cprind_cat = "Age Range: 55 to 64 years of age" 																				if cprind_cat == "cprind_cat_agerange_6"
replace cprind_cat = "Age Range: 65 to 74 years of age" 																				if cprind_cat == "cprind_cat_agerange_7"
replace cprind_cat = "Age Range: 75 years or over"	 																				if cprind_cat == "cprind_cat_agerange_8"
replace cprind_cat = "Sex: Male"																										if cprind_cat == "cprind_cat_sex_1"
replace cprind_cat = "Sex: Female"																									if cprind_cat == "cprind_cat_sex_2"
replace cprind_cat = "Section of State: Major Urban (Pop 100,000+)"																	if cprind_cat == "cprind_cat_sectionofstate_1"
replace cprind_cat = "Section of State: Other Urban (Pop 1,000-99,999)"																if cprind_cat == "cprind_cat_sectionofstate_2"
replace cprind_cat = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if cprind_cat == "cprind_cat_sectionofstate_3"
replace cprind_cat = "Country of Birth: Australia"																					if cprind_cat == "cprind_cat_countryofbirth_1"
replace cprind_cat = "Country of Birth: Main English speaking"																		if cprind_cat == "cprind_cat_countryofbirth_2"
replace cprind_cat = "Country of Birth: Other countries"																				if cprind_cat == "cprind_cat_countryofbirth_3"
replace cprind_cat = "Educational Attainment: Degree (incl. postgrad)"																if cprind_cat == "cprind_cat_education_1"
replace cprind_cat = "Educational Attainment: Other higher education"																	if cprind_cat == "cprind_cat_education_2"
replace cprind_cat = "Educational Attainment: Year 12 (equiv. to UK A level)"															if cprind_cat == "cprind_cat_education_3"
replace cprind_cat = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if cprind_cat == "cprind_cat_education_4"
replace cprind_cat = "Educational Attainment: Other qualification"																	if cprind_cat == "cprind_cat_education_5"
replace cprind_cat = "Educational Attainment: No qualification" 																		if cprind_cat == "cprind_cat_education_6"
replace cprind_cat = "Studying Full Time: No"																							if cprind_cat == "cprind_cat_ftstudent_0"
replace cprind_cat = "Studying Full Time: Yes"																						if cprind_cat == "cprind_cat_ftstudent_1"
replace cprind_cat = "Employment Status & Hours: Not in the labour force"																if cprind_cat == "cprind_cat_employment_0"
replace cprind_cat = "Employment Status & Hours: Unemployed"																			if cprind_cat == "cprind_cat_employment_1"
replace cprind_cat = "Employment Status & Hours: Employed full-time"																if cprind_cat == "cprind_cat_employment_2"
replace cprind_cat = "Employment Status & Hours: Employed part-time"																if cprind_cat == "cprind_cat_employment_3"
*replace cprind_cat = "Employment Status & Hours: Employed, working less than 15 hours per week"										if cprind_cat == "cprind_cat_employment_2"
*replace cprind_cat = "Employment Status & Hours: Employed, working 15-29 hours per week"												if cprind_cat == "cprind_cat_employment_3"
*replace cprind_cat = "Employment Status & Hours: Employed, working 30-39 hours per week"												if cprind_cat == "cprind_cat_employment_4"
*replace cprind_cat = "Employment Status & Hours: Employed, working 40-49 hours per week"												if cprind_cat == "cprind_cat_employment_5"
*replace cprind_cat = "Employment Status & Hours: Employed, working 50 or more hours per week"											if cprind_cat == "cprind_cat_employment_6"
replace cprind_cat = "Reliant on Government Payments: No"																				if cprind_cat == "cprind_cat_benefitreliant_0"
replace cprind_cat = "Reliant on Government Payments: Yes"																			if cprind_cat == "cprind_cat_benefitreliant_1"
replace cprind_cat = "Degree of Health Condition: Does not have a long-term health condition"											if cprind_cat == "cprind_cat_healthcondition_0"
replace cprind_cat = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if cprind_cat == "cprind_cat_healthcondition_1"
replace cprind_cat = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if cprind_cat == "cprind_cat_healthcondition_2"
replace cprind_cat = "Relationship Status: Married"																					if cprind_cat == "cprind_cat_relationship_1"
replace cprind_cat = "Relationship Status: De facto"																					if cprind_cat == "cprind_cat_relationship_2"
replace cprind_cat = "Relationship Status: Separated, divorced or widowed"															if cprind_cat == "cprind_cat_relationship_3"
replace cprind_cat = "Relationship Status: Never married and not de facto"															if cprind_cat == "cprind_cat_relationship_4"
replace cprind_cat = "Number of Adults in Household: 1 adult"																			if cprind_cat == "cprind_cat_adultsinhh_1"
replace cprind_cat = "Number of Adults in Household: 2 adults"																		if cprind_cat == "cprind_cat_adultsinhh_2"
replace cprind_cat = "Number of Adults in Household: 3 or more adults"																if cprind_cat == "cprind_cat_adultsinhh_3"
replace cprind_cat = "Number of Children in Household: No children"																	if cprind_cat == "cprind_cat_childreninhh_0"
replace cprind_cat = "Number of Children in Household: 1 child"																		if cprind_cat == "cprind_cat_childreninhh_1"
replace cprind_cat = "Number of Children in Household: 2 or more children"															if cprind_cat == "cprind_cat_childreninhh_2"
replace cprind_cat = "Household owns its dwelling: No"																				if cprind_cat == "cprind_cat_owner_0"
replace cprind_cat = "Household owns its dwelling: Yes"																				if cprind_cat == "cprind_cat_owner_1"
replace cprind_cat = "Household likely to move in next 12 months: No"																	if cprind_cat == "cprind_cat_likelymove_0"
replace cprind_cat = "Household likely to move in next 12 months: Yes"																if cprind_cat == "cprind_cat_likelymove_1"
replace cprind_cat = "Member of Top-Up Sample: No"																					if cprind_cat == "cprind_cat_topupsample_0"
replace cprind_cat = "Member of Top-Up Sample: Yes"																					if cprind_cat == "cprind_cat_topupsample_1"
order cprind_cat Wave_16 Wave_15 Wave_14 Wave_13 Wave_12
save "$workinglocation\conditionals_catlevel_Main_period`period'", replace
list
export  excel "$resultslocation\Phase 3 - Results grouped across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Conditional, Cat, Main, P`period'") sheetmodify firstrow(variables) cell(A1)


* Conditional partials - category level - TopUp
forvalues wave=$startwave/$endwave {
	use "$workinglocation\RIndicatorsAndCVs_wave`wave'_TopUp", clear
	keep if substr(Type_Var,1,10)=="cprind_cat"
	keep Type_Var P`period'_Adjusted
	rename Type_Var cprind_cat
	rename P`period'_Adjusted Wave_`wave'
	sort cprind_cat
	save "$workinglocation\interim_wave`wave'", replace
}
use "$workinglocation\interim_wave12", clear
merge 1:1 cprind_cat using "$workinglocation\interim_wave13"
drop _merge
merge 1:1 cprind_cat using "$workinglocation\interim_wave14"
drop _merge
merge 1:1 cprind_cat using "$workinglocation\interim_wave15"
drop _merge
merge 1:1 cprind_cat using "$workinglocation\interim_wave16"
drop _merge
gen ordering = 1 if 			cprind_cat == "cprind_cat_agerange_1"
replace ordering = 2 if 		cprind_cat == "cprind_cat_agerange_2"
replace ordering = 3 if 		cprind_cat == "cprind_cat_agerange_3"
replace ordering = 4 if 		cprind_cat == "cprind_cat_agerange_4"
replace ordering = 5 if 		cprind_cat == "cprind_cat_agerange_5"
replace ordering = 6 if 		cprind_cat == "cprind_cat_agerange_6"
replace ordering = 7 if 		cprind_cat == "cprind_cat_agerange_7"
replace ordering = 8 if 		cprind_cat == "cprind_cat_agerange_8"
replace ordering = 9 if 		cprind_cat == "cprind_cat_sex_1"
replace ordering = 10 if 		cprind_cat == "cprind_cat_sex_2"
replace ordering = 11 if 		cprind_cat == "cprind_cat_sectionofstate_1"
replace ordering = 12 if 		cprind_cat == "cprind_cat_sectionofstate_2"
replace ordering = 13 if 		cprind_cat == "cprind_cat_sectionofstate_3"
replace ordering = 14 if 		cprind_cat == "cprind_cat_countryofbirth_1"
replace ordering = 15 if 		cprind_cat == "cprind_cat_countryofbirth_2"
replace ordering = 16 if 		cprind_cat == "cprind_cat_countryofbirth_3"
replace ordering = 17 if		cprind_cat == "cprind_cat_education_1"
replace ordering = 18 if		cprind_cat == "cprind_cat_education_2"
replace ordering = 19 if		cprind_cat == "cprind_cat_education_3"
replace ordering = 20 if		cprind_cat == "cprind_cat_education_4"
replace ordering = 21 if		cprind_cat == "cprind_cat_education_5"
replace ordering = 22 if		cprind_cat == "cprind_cat_education_6"
replace ordering = 23 if	 	cprind_cat == "cprind_cat_ftstudent_0"
replace ordering = 24 if	 	cprind_cat == "cprind_cat_ftstudent_1"
replace ordering = 25 if 		cprind_cat == "cprind_cat_employment_0"
replace ordering = 26 if 		cprind_cat == "cprind_cat_employment_1"
replace ordering = 27 if 		cprind_cat == "cprind_cat_employment_2"
replace ordering = 28 if 		cprind_cat == "cprind_cat_employment_3"
*replace ordering = 29 if 		cprind_cat == "cprind_cat_employment_4"
*replace ordering = 30 if 		cprind_cat == "cprind_cat_employment_5"
*replace ordering = 31 if 		cprind_cat == "cprind_cat_employment_6"
replace ordering = 32 if 		cprind_cat == "cprind_cat_benefitreliant_0"
replace ordering = 33 if 		cprind_cat == "cprind_cat_benefitreliant_1"
replace ordering = 34 if	 	cprind_cat == "cprind_cat_healthcondition_0"
replace ordering = 35 if	 	cprind_cat == "cprind_cat_healthcondition_1"
replace ordering = 36 if	 	cprind_cat == "cprind_cat_healthcondition_2"
replace ordering = 37 if 		cprind_cat == "cprind_cat_relationship_1"
replace ordering = 38 if 		cprind_cat == "cprind_cat_relationship_2"
replace ordering = 39 if 		cprind_cat == "cprind_cat_relationship_3"
replace ordering = 40 if 		cprind_cat == "cprind_cat_relationship_4"
replace ordering = 41 if	 	cprind_cat == "cprind_cat_adultsinhh_1"
replace ordering = 42 if	 	cprind_cat == "cprind_cat_adultsinhh_2"
replace ordering = 43 if	 	cprind_cat == "cprind_cat_adultsinhh_3"
replace ordering = 44 if 		cprind_cat == "cprind_cat_childreninhh_0"
replace ordering = 45 if 		cprind_cat == "cprind_cat_childreninhh_1"
replace ordering = 46 if 		cprind_cat == "cprind_cat_childreninhh_2"
replace ordering = 47 if 		cprind_cat == "cprind_cat_owner_0"
replace ordering = 48 if 		cprind_cat == "cprind_cat_owner_1"
replace ordering = 49 if 		cprind_cat == "cprind_cat_likelymove_0"
replace ordering = 50 if 		cprind_cat == "cprind_cat_likelymove_1"
replace ordering = 51 if 		cprind_cat == "cprind_cat_topupsample_0"
replace ordering = 52 if 		cprind_cat == "cprind_cat_topupsample_1"
gsort -ordering cprind_cat
drop ordering
replace cprind_cat = "Age Range: 15 to 19 years of age" 																				if cprind_cat == "cprind_cat_agerange_1"
replace cprind_cat = "Age Range: 20 to 24 years of age" 																				if cprind_cat == "cprind_cat_agerange_2"
replace cprind_cat = "Age Range: 25 to 34 years of age" 																				if cprind_cat == "cprind_cat_agerange_3"
replace cprind_cat = "Age Range: 35 to 44 years of age" 																				if cprind_cat == "cprind_cat_agerange_4"
replace cprind_cat = "Age Range: 45 to 54 years of age" 																				if cprind_cat == "cprind_cat_agerange_5"
replace cprind_cat = "Age Range: 55 to 64 years of age" 																				if cprind_cat == "cprind_cat_agerange_6"
replace cprind_cat = "Age Range: 65 to 74 years of age" 																				if cprind_cat == "cprind_cat_agerange_7"
replace cprind_cat = "Age Range: 75 years or over"	 																				if cprind_cat == "cprind_cat_agerange_8"
replace cprind_cat = "Sex: Male"																										if cprind_cat == "cprind_cat_sex_1"
replace cprind_cat = "Sex: Female"																									if cprind_cat == "cprind_cat_sex_2"
replace cprind_cat = "Section of State: Major Urban (Pop 100,000+)"																	if cprind_cat == "cprind_cat_sectionofstate_1"
replace cprind_cat = "Section of State: Other Urban (Pop 1,000-99,999)"																if cprind_cat == "cprind_cat_sectionofstate_2"
replace cprind_cat = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if cprind_cat == "cprind_cat_sectionofstate_3"
replace cprind_cat = "Country of Birth: Australia"																					if cprind_cat == "cprind_cat_countryofbirth_1"
replace cprind_cat = "Country of Birth: Main English speaking"																		if cprind_cat == "cprind_cat_countryofbirth_2"
replace cprind_cat = "Country of Birth: Other countries"																				if cprind_cat == "cprind_cat_countryofbirth_3"
replace cprind_cat = "Educational Attainment: Degree (incl. postgrad)"																if cprind_cat == "cprind_cat_education_1"
replace cprind_cat = "Educational Attainment: Other higher education"																	if cprind_cat == "cprind_cat_education_2"
replace cprind_cat = "Educational Attainment: Year 12 (equiv. to UK A level)"															if cprind_cat == "cprind_cat_education_3"
replace cprind_cat = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if cprind_cat == "cprind_cat_education_4"
replace cprind_cat = "Educational Attainment: Other qualification"																	if cprind_cat == "cprind_cat_education_5"
replace cprind_cat = "Educational Attainment: No qualification" 																		if cprind_cat == "cprind_cat_education_6"
replace cprind_cat = "Studying Full Time: No"																							if cprind_cat == "cprind_cat_ftstudent_0"
replace cprind_cat = "Studying Full Time: Yes"																						if cprind_cat == "cprind_cat_ftstudent_1"
replace cprind_cat = "Employment Status & Hours: Not in the labour force"																if cprind_cat == "cprind_cat_employment_0"
replace cprind_cat = "Employment Status & Hours: Unemployed"																			if cprind_cat == "cprind_cat_employment_1"
replace cprind_cat = "Employment Status & Hours: Employed full-time"																if cprind_cat == "cprind_cat_employment_2"
replace cprind_cat = "Employment Status & Hours: Employed part-time"																if cprind_cat == "cprind_cat_employment_3"
*replace cprind_cat = "Employment Status & Hours: Employed, working less than 15 hours per week"										if cprind_cat == "cprind_cat_employment_2"
*replace cprind_cat = "Employment Status & Hours: Employed, working 15-29 hours per week"												if cprind_cat == "cprind_cat_employment_3"
*replace cprind_cat = "Employment Status & Hours: Employed, working 30-39 hours per week"												if cprind_cat == "cprind_cat_employment_4"
*replace cprind_cat = "Employment Status & Hours: Employed, working 40-49 hours per week"												if cprind_cat == "cprind_cat_employment_5"
*replace cprind_cat = "Employment Status & Hours: Employed, working 50 or more hours per week"											if cprind_cat == "cprind_cat_employment_6"
replace cprind_cat = "Reliant on Government Payments: No"																				if cprind_cat == "cprind_cat_benefitreliant_0"
replace cprind_cat = "Reliant on Government Payments: Yes"																			if cprind_cat == "cprind_cat_benefitreliant_1"
replace cprind_cat = "Degree of Health Condition: Does not have a long-term health condition"											if cprind_cat == "cprind_cat_healthcondition_0"
replace cprind_cat = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if cprind_cat == "cprind_cat_healthcondition_1"
replace cprind_cat = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if cprind_cat == "cprind_cat_healthcondition_2"
replace cprind_cat = "Relationship Status: Married"																					if cprind_cat == "cprind_cat_relationship_1"
replace cprind_cat = "Relationship Status: De facto"																					if cprind_cat == "cprind_cat_relationship_2"
replace cprind_cat = "Relationship Status: Separated, divorced or widowed"															if cprind_cat == "cprind_cat_relationship_3"
replace cprind_cat = "Relationship Status: Never married and not de facto"															if cprind_cat == "cprind_cat_relationship_4"
replace cprind_cat = "Number of Adults in Household: 1 adult"																			if cprind_cat == "cprind_cat_adultsinhh_1"
replace cprind_cat = "Number of Adults in Household: 2 adults"																		if cprind_cat == "cprind_cat_adultsinhh_2"
replace cprind_cat = "Number of Adults in Household: 3 or more adults"																if cprind_cat == "cprind_cat_adultsinhh_3"
replace cprind_cat = "Number of Children in Household: No children"																	if cprind_cat == "cprind_cat_childreninhh_0"
replace cprind_cat = "Number of Children in Household: 1 child"																		if cprind_cat == "cprind_cat_childreninhh_1"
replace cprind_cat = "Number of Children in Household: 2 or more children"															if cprind_cat == "cprind_cat_childreninhh_2"
replace cprind_cat = "Household owns its dwelling: No"																				if cprind_cat == "cprind_cat_owner_0"
replace cprind_cat = "Household owns its dwelling: Yes"																				if cprind_cat == "cprind_cat_owner_1"
replace cprind_cat = "Household likely to move in next 12 months: No"																	if cprind_cat == "cprind_cat_likelymove_0"
replace cprind_cat = "Household likely to move in next 12 months: Yes"																if cprind_cat == "cprind_cat_likelymove_1"
replace cprind_cat = "Member of Top-Up Sample: No"																					if cprind_cat == "cprind_cat_topupsample_0"
replace cprind_cat = "Member of Top-Up Sample: Yes"																					if cprind_cat == "cprind_cat_topupsample_1"
order cprind_cat Wave_16 Wave_15 Wave_14 Wave_13 Wave_12
save "$workinglocation\conditionals_catlevel_TopUp_period`period'", replace
list
export  excel "$resultslocation\Phase 3 - Results grouped across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Conditional, Cat, TopUp, P`period'") sheetmodify firstrow(variables) cell(A1)

* Unconditional partials - category level - Main
forvalues wave=$startwave/$endwave {
	use "$workinglocation\RIndicatorsAndCVs_wave`wave'_Main", clear
	keep if substr(Type_Var,1,10)=="uprind_cat"
	keep Type_Var P`period'_Adjusted
	rename Type_Var uprind_cat
	rename P`period'_Adjusted Wave_`wave'
	sort uprind_cat
	save "$workinglocation\interim_wave`wave'", replace
}
use "$workinglocation\interim_wave12", clear
merge 1:1 uprind_cat using "$workinglocation\interim_wave13"
drop _merge
merge 1:1 uprind_cat using "$workinglocation\interim_wave14"
drop _merge
merge 1:1 uprind_cat using "$workinglocation\interim_wave15"
drop _merge
merge 1:1 uprind_cat using "$workinglocation\interim_wave16"
drop _merge
gen ordering = 1 if 			uprind_cat == "uprind_cat_agerange_1"
replace ordering = 2 if 		uprind_cat == "uprind_cat_agerange_2"
replace ordering = 3 if 		uprind_cat == "uprind_cat_agerange_3"
replace ordering = 4 if 		uprind_cat == "uprind_cat_agerange_4"
replace ordering = 5 if 		uprind_cat == "uprind_cat_agerange_5"
replace ordering = 6 if 		uprind_cat == "uprind_cat_agerange_6"
replace ordering = 7 if 		uprind_cat == "uprind_cat_agerange_7"
replace ordering = 8 if 		uprind_cat == "uprind_cat_agerange_8"
replace ordering = 9 if 		uprind_cat == "uprind_cat_sex_1"
replace ordering = 10 if 		uprind_cat == "uprind_cat_sex_2"
replace ordering = 11 if 		uprind_cat == "uprind_cat_sectionofstate_1"
replace ordering = 12 if 		uprind_cat == "uprind_cat_sectionofstate_2"
replace ordering = 13 if 		uprind_cat == "uprind_cat_sectionofstate_3"
replace ordering = 14 if 		uprind_cat == "uprind_cat_countryofbirth_1"
replace ordering = 15 if 		uprind_cat == "uprind_cat_countryofbirth_2"
replace ordering = 16 if 		uprind_cat == "uprind_cat_countryofbirth_3"
replace ordering = 17 if		uprind_cat == "uprind_cat_education_1"
replace ordering = 18 if		uprind_cat == "uprind_cat_education_2"
replace ordering = 19 if		uprind_cat == "uprind_cat_education_3"
replace ordering = 20 if		uprind_cat == "uprind_cat_education_4"
replace ordering = 21 if		uprind_cat == "uprind_cat_education_5"
replace ordering = 22 if		uprind_cat == "uprind_cat_education_6"
replace ordering = 23 if	 	uprind_cat == "uprind_cat_ftstudent_0"
replace ordering = 24 if	 	uprind_cat == "uprind_cat_ftstudent_1"
replace ordering = 25 if 		uprind_cat == "uprind_cat_employment_0"
replace ordering = 26 if 		uprind_cat == "uprind_cat_employment_1"
replace ordering = 27 if 		uprind_cat == "uprind_cat_employment_2"
replace ordering = 28 if 		uprind_cat == "uprind_cat_employment_3"
*replace ordering = 29 if 		uprind_cat == "uprind_cat_employment_4"
*replace ordering = 30 if 		uprind_cat == "uprind_cat_employment_5"
*replace ordering = 31 if 		uprind_cat == "uprind_cat_employment_6"
replace ordering = 32 if 		uprind_cat == "uprind_cat_benefitreliant_0"
replace ordering = 33 if 		uprind_cat == "uprind_cat_benefitreliant_1"
replace ordering = 34 if	 	uprind_cat == "uprind_cat_healthcondition_0"
replace ordering = 35 if	 	uprind_cat == "uprind_cat_healthcondition_1"
replace ordering = 36 if	 	uprind_cat == "uprind_cat_healthcondition_2"
replace ordering = 37 if 		uprind_cat == "uprind_cat_relationship_1"
replace ordering = 38 if 		uprind_cat == "uprind_cat_relationship_2"
replace ordering = 39 if 		uprind_cat == "uprind_cat_relationship_3"
replace ordering = 40 if 		uprind_cat == "uprind_cat_relationship_4"
replace ordering = 41 if	 	uprind_cat == "uprind_cat_adultsinhh_1"
replace ordering = 42 if	 	uprind_cat == "uprind_cat_adultsinhh_2"
replace ordering = 43 if	 	uprind_cat == "uprind_cat_adultsinhh_3"
replace ordering = 44 if 		uprind_cat == "uprind_cat_childreninhh_0"
replace ordering = 45 if 		uprind_cat == "uprind_cat_childreninhh_1"
replace ordering = 46 if 		uprind_cat == "uprind_cat_childreninhh_2"
replace ordering = 47 if 		uprind_cat == "uprind_cat_owner_0"
replace ordering = 48 if 		uprind_cat == "uprind_cat_owner_1"
replace ordering = 49 if 		uprind_cat == "uprind_cat_likelymove_0"
replace ordering = 50 if 		uprind_cat == "uprind_cat_likelymove_1"
replace ordering = 51 if 		uprind_cat == "uprind_cat_topupsample_0"
replace ordering = 52 if 		uprind_cat == "uprind_cat_topupsample_1"
gsort -ordering uprind_cat
drop ordering
replace uprind_cat = "Age Range: 15 to 19 years of age" 																				if uprind_cat == "uprind_cat_agerange_1"
replace uprind_cat = "Age Range: 20 to 24 years of age" 																				if uprind_cat == "uprind_cat_agerange_2"
replace uprind_cat = "Age Range: 25 to 34 years of age" 																				if uprind_cat == "uprind_cat_agerange_3"
replace uprind_cat = "Age Range: 35 to 44 years of age" 																				if uprind_cat == "uprind_cat_agerange_4"
replace uprind_cat = "Age Range: 45 to 54 years of age" 																				if uprind_cat == "uprind_cat_agerange_5"
replace uprind_cat = "Age Range: 55 to 64 years of age" 																				if uprind_cat == "uprind_cat_agerange_6"
replace uprind_cat = "Age Range: 65 to 74 years of age" 																				if uprind_cat == "uprind_cat_agerange_7"
replace uprind_cat = "Age Range: 75 years or over"	 																				if uprind_cat == "uprind_cat_agerange_8"
replace uprind_cat = "Sex: Male"																										if uprind_cat == "uprind_cat_sex_1"
replace uprind_cat = "Sex: Female"																									if uprind_cat == "uprind_cat_sex_2"
replace uprind_cat = "Section of State: Major Urban (Pop 100,000+)"																	if uprind_cat == "uprind_cat_sectionofstate_1"
replace uprind_cat = "Section of State: Other Urban (Pop 1,000-99,999)"																if uprind_cat == "uprind_cat_sectionofstate_2"
replace uprind_cat = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if uprind_cat == "uprind_cat_sectionofstate_3"
replace uprind_cat = "Country of Birth: Australia"																					if uprind_cat == "uprind_cat_countryofbirth_1"
replace uprind_cat = "Country of Birth: Main English speaking"																		if uprind_cat == "uprind_cat_countryofbirth_2"
replace uprind_cat = "Country of Birth: Other countries"																				if uprind_cat == "uprind_cat_countryofbirth_3"
replace uprind_cat = "Educational Attainment: Degree (incl. postgrad)"																if uprind_cat == "uprind_cat_education_1"
replace uprind_cat = "Educational Attainment: Other higher education"																	if uprind_cat == "uprind_cat_education_2"
replace uprind_cat = "Educational Attainment: Year 12 (equiv. to UK A level)"															if uprind_cat == "uprind_cat_education_3"
replace uprind_cat = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if uprind_cat == "uprind_cat_education_4"
replace uprind_cat = "Educational Attainment: Other qualification"																	if uprind_cat == "uprind_cat_education_5"
replace uprind_cat = "Educational Attainment: No qualification" 																		if uprind_cat == "uprind_cat_education_6"
replace uprind_cat = "Studying Full Time: No"																							if uprind_cat == "uprind_cat_ftstudent_0"
replace uprind_cat = "Studying Full Time: Yes"																						if uprind_cat == "uprind_cat_ftstudent_1"
replace uprind_cat = "Employment Status & Hours: Not in the labour force"																if uprind_cat == "uprind_cat_employment_0"
replace uprind_cat = "Employment Status & Hours: Unemployed"																			if uprind_cat == "uprind_cat_employment_1"
replace uprind_cat = "Employment Status & Hours: Employed full-time"																if uprind_cat == "uprind_cat_employment_2"
replace uprind_cat = "Employment Status & Hours: Employed part-time"																if uprind_cat == "uprind_cat_employment_3"
*replace uprind_cat = "Employment Status & Hours: Employed, working less than 15 hours per week"										if uprind_cat == "uprind_cat_employment_2"
*replace uprind_cat = "Employment Status & Hours: Employed, working 15-29 hours per week"												if uprind_cat == "uprind_cat_employment_3"
*replace uprind_cat = "Employment Status & Hours: Employed, working 30-39 hours per week"												if uprind_cat == "uprind_cat_employment_4"
*replace uprind_cat = "Employment Status & Hours: Employed, working 40-49 hours per week"												if uprind_cat == "uprind_cat_employment_5"
*replace uprind_cat = "Employment Status & Hours: Employed, working 50 or more hours per week"											if uprind_cat == "uprind_cat_employment_6"
replace uprind_cat = "Reliant on Government Payments: No"																				if uprind_cat == "uprind_cat_benefitreliant_0"
replace uprind_cat = "Reliant on Government Payments: Yes"																			if uprind_cat == "uprind_cat_benefitreliant_1"
replace uprind_cat = "Degree of Health Condition: Does not have a long-term health condition"											if uprind_cat == "uprind_cat_healthcondition_0"
replace uprind_cat = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if uprind_cat == "uprind_cat_healthcondition_1"
replace uprind_cat = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if uprind_cat == "uprind_cat_healthcondition_2"
replace uprind_cat = "Relationship Status: Married"																					if uprind_cat == "uprind_cat_relationship_1"
replace uprind_cat = "Relationship Status: De facto"																					if uprind_cat == "uprind_cat_relationship_2"
replace uprind_cat = "Relationship Status: Separated, divorced or widowed"															if uprind_cat == "uprind_cat_relationship_3"
replace uprind_cat = "Relationship Status: Never married and not de facto"															if uprind_cat == "uprind_cat_relationship_4"
replace uprind_cat = "Number of Adults in Household: 1 adult"																			if uprind_cat == "uprind_cat_adultsinhh_1"
replace uprind_cat = "Number of Adults in Household: 2 adults"																		if uprind_cat == "uprind_cat_adultsinhh_2"
replace uprind_cat = "Number of Adults in Household: 3 or more adults"																if uprind_cat == "uprind_cat_adultsinhh_3"
replace uprind_cat = "Number of Children in Household: No children"																	if uprind_cat == "uprind_cat_childreninhh_0"
replace uprind_cat = "Number of Children in Household: 1 child"																		if uprind_cat == "uprind_cat_childreninhh_1"
replace uprind_cat = "Number of Children in Household: 2 or more children"															if uprind_cat == "uprind_cat_childreninhh_2"
replace uprind_cat = "Household owns its dwelling: No"																				if uprind_cat == "uprind_cat_owner_0"
replace uprind_cat = "Household owns its dwelling: Yes"																				if uprind_cat == "uprind_cat_owner_1"
replace uprind_cat = "Household likely to move in next 12 months: No"																	if uprind_cat == "uprind_cat_likelymove_0"
replace uprind_cat = "Household likely to move in next 12 months: Yes"																if uprind_cat == "uprind_cat_likelymove_1"
replace uprind_cat = "Member of Top-Up Sample: No"																					if uprind_cat == "uprind_cat_topupsample_0"
replace uprind_cat = "Member of Top-Up Sample: Yes"																					if uprind_cat == "uprind_cat_topupsample_1"
order uprind_cat Wave_16 Wave_15 Wave_14 Wave_13 Wave_12
save "$workinglocation\unconditionals_catlevel_Main_period`period'", replace
list
export  excel "$resultslocation\Phase 3 - Results grouped across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Unconditional, Cat, Main, P`period'") sheetmodify firstrow(variables) cell(A1)

* Unconditional partials - category level - TopUp
forvalues wave=$startwave/$endwave {
	use "$workinglocation\RIndicatorsAndCVs_wave`wave'_TopUp", clear
	keep if substr(Type_Var,1,10)=="uprind_cat"
	keep Type_Var P`period'_Adjusted
	rename Type_Var uprind_cat
	rename P`period'_Adjusted Wave_`wave'
	sort uprind_cat
	save "$workinglocation\interim_wave`wave'", replace
}
use "$workinglocation\interim_wave12", clear
merge 1:1 uprind_cat using "$workinglocation\interim_wave13"
drop _merge
merge 1:1 uprind_cat using "$workinglocation\interim_wave14"
drop _merge
merge 1:1 uprind_cat using "$workinglocation\interim_wave15"
drop _merge
merge 1:1 uprind_cat using "$workinglocation\interim_wave16"
drop _merge
gen ordering = 1 if 			uprind_cat == "uprind_cat_agerange_1"
replace ordering = 2 if 		uprind_cat == "uprind_cat_agerange_2"
replace ordering = 3 if 		uprind_cat == "uprind_cat_agerange_3"
replace ordering = 4 if 		uprind_cat == "uprind_cat_agerange_4"
replace ordering = 5 if 		uprind_cat == "uprind_cat_agerange_5"
replace ordering = 6 if 		uprind_cat == "uprind_cat_agerange_6"
replace ordering = 7 if 		uprind_cat == "uprind_cat_agerange_7"
replace ordering = 8 if 		uprind_cat == "uprind_cat_agerange_8"
replace ordering = 9 if 		uprind_cat == "uprind_cat_sex_1"
replace ordering = 10 if 		uprind_cat == "uprind_cat_sex_2"
replace ordering = 11 if 		uprind_cat == "uprind_cat_sectionofstate_1"
replace ordering = 12 if 		uprind_cat == "uprind_cat_sectionofstate_2"
replace ordering = 13 if 		uprind_cat == "uprind_cat_sectionofstate_3"
replace ordering = 14 if 		uprind_cat == "uprind_cat_countryofbirth_1"
replace ordering = 15 if 		uprind_cat == "uprind_cat_countryofbirth_2"
replace ordering = 16 if 		uprind_cat == "uprind_cat_countryofbirth_3"
replace ordering = 17 if		uprind_cat == "uprind_cat_education_1"
replace ordering = 18 if		uprind_cat == "uprind_cat_education_2"
replace ordering = 19 if		uprind_cat == "uprind_cat_education_3"
replace ordering = 20 if		uprind_cat == "uprind_cat_education_4"
replace ordering = 21 if		uprind_cat == "uprind_cat_education_5"
replace ordering = 22 if		uprind_cat == "uprind_cat_education_6"
replace ordering = 23 if	 	uprind_cat == "uprind_cat_ftstudent_0"
replace ordering = 24 if	 	uprind_cat == "uprind_cat_ftstudent_1"
replace ordering = 25 if 		uprind_cat == "uprind_cat_employment_0"
replace ordering = 26 if 		uprind_cat == "uprind_cat_employment_1"
replace ordering = 27 if 		uprind_cat == "uprind_cat_employment_2"
replace ordering = 28 if 		uprind_cat == "uprind_cat_employment_3"
*replace ordering = 29 if 		uprind_cat == "uprind_cat_employment_4"
*replace ordering = 30 if 		uprind_cat == "uprind_cat_employment_5"
*replace ordering = 31 if 		uprind_cat == "uprind_cat_employment_6"
replace ordering = 32 if 		uprind_cat == "uprind_cat_benefitreliant_0"
replace ordering = 33 if 		uprind_cat == "uprind_cat_benefitreliant_1"
replace ordering = 34 if	 	uprind_cat == "uprind_cat_healthcondition_0"
replace ordering = 35 if	 	uprind_cat == "uprind_cat_healthcondition_1"
replace ordering = 36 if	 	uprind_cat == "uprind_cat_healthcondition_2"
replace ordering = 37 if 		uprind_cat == "uprind_cat_relationship_1"
replace ordering = 38 if 		uprind_cat == "uprind_cat_relationship_2"
replace ordering = 39 if 		uprind_cat == "uprind_cat_relationship_3"
replace ordering = 40 if 		uprind_cat == "uprind_cat_relationship_4"
replace ordering = 41 if	 	uprind_cat == "uprind_cat_adultsinhh_1"
replace ordering = 42 if	 	uprind_cat == "uprind_cat_adultsinhh_2"
replace ordering = 43 if	 	uprind_cat == "uprind_cat_adultsinhh_3"
replace ordering = 44 if 		uprind_cat == "uprind_cat_childreninhh_0"
replace ordering = 45 if 		uprind_cat == "uprind_cat_childreninhh_1"
replace ordering = 46 if 		uprind_cat == "uprind_cat_childreninhh_2"
replace ordering = 47 if 		uprind_cat == "uprind_cat_owner_0"
replace ordering = 48 if 		uprind_cat == "uprind_cat_owner_1"
replace ordering = 49 if 		uprind_cat == "uprind_cat_likelymove_0"
replace ordering = 50 if 		uprind_cat == "uprind_cat_likelymove_1"
replace ordering = 51 if 		uprind_cat == "uprind_cat_topupsample_0"
replace ordering = 52 if 		uprind_cat == "uprind_cat_topupsample_1"
gsort -ordering uprind_cat
drop ordering
replace uprind_cat = "Age Range: 15 to 19 years of age" 																				if uprind_cat == "uprind_cat_agerange_1"
replace uprind_cat = "Age Range: 20 to 24 years of age" 																				if uprind_cat == "uprind_cat_agerange_2"
replace uprind_cat = "Age Range: 25 to 34 years of age" 																				if uprind_cat == "uprind_cat_agerange_3"
replace uprind_cat = "Age Range: 35 to 44 years of age" 																				if uprind_cat == "uprind_cat_agerange_4"
replace uprind_cat = "Age Range: 45 to 54 years of age" 																				if uprind_cat == "uprind_cat_agerange_5"
replace uprind_cat = "Age Range: 55 to 64 years of age" 																				if uprind_cat == "uprind_cat_agerange_6"
replace uprind_cat = "Age Range: 65 to 74 years of age" 																				if uprind_cat == "uprind_cat_agerange_7"
replace uprind_cat = "Age Range: 75 years or over"	 																				if uprind_cat == "uprind_cat_agerange_8"
replace uprind_cat = "Sex: Male"																										if uprind_cat == "uprind_cat_sex_1"
replace uprind_cat = "Sex: Female"																									if uprind_cat == "uprind_cat_sex_2"
replace uprind_cat = "Section of State: Major Urban (Pop 100,000+)"																	if uprind_cat == "uprind_cat_sectionofstate_1"
replace uprind_cat = "Section of State: Other Urban (Pop 1,000-99,999)"																if uprind_cat == "uprind_cat_sectionofstate_2"
replace uprind_cat = "Section of State: Bounded Locality, Rural Balance, Migratory, or no usual address"								if uprind_cat == "uprind_cat_sectionofstate_3"
replace uprind_cat = "Country of Birth: Australia"																					if uprind_cat == "uprind_cat_countryofbirth_1"
replace uprind_cat = "Country of Birth: Main English speaking"																		if uprind_cat == "uprind_cat_countryofbirth_2"
replace uprind_cat = "Country of Birth: Other countries"																				if uprind_cat == "uprind_cat_countryofbirth_3"
replace uprind_cat = "Educational Attainment: Degree (incl. postgrad)"																if uprind_cat == "uprind_cat_education_1"
replace uprind_cat = "Educational Attainment: Other higher education"																	if uprind_cat == "uprind_cat_education_2"
replace uprind_cat = "Educational Attainment: Year 12 (equiv. to UK A level)"															if uprind_cat == "uprind_cat_education_3"
replace uprind_cat = "Educational Attainment: Year 10 (equiv. to UK GCSE)"															if uprind_cat == "uprind_cat_education_4"
replace uprind_cat = "Educational Attainment: Other qualification"																	if uprind_cat == "uprind_cat_education_5"
replace uprind_cat = "Educational Attainment: No qualification" 																		if uprind_cat == "uprind_cat_education_6"
replace uprind_cat = "Studying Full Time: No"																							if uprind_cat == "uprind_cat_ftstudent_0"
replace uprind_cat = "Studying Full Time: Yes"																						if uprind_cat == "uprind_cat_ftstudent_1"
replace uprind_cat = "Employment Status & Hours: Not in the labour force"																if uprind_cat == "uprind_cat_employment_0"
replace uprind_cat = "Employment Status & Hours: Unemployed"																			if uprind_cat == "uprind_cat_employment_1"
replace uprind_cat = "Employment Status & Hours: Employed full-time"																if uprind_cat == "uprind_cat_employment_2"
replace uprind_cat = "Employment Status & Hours: Employed part-time"																if uprind_cat == "uprind_cat_employment_3"
*replace uprind_cat = "Employment Status & Hours: Employed, working less than 15 hours per week"										if uprind_cat == "uprind_cat_employment_2"
*replace uprind_cat = "Employment Status & Hours: Employed, working 15-29 hours per week"												if uprind_cat == "uprind_cat_employment_3"
*replace uprind_cat = "Employment Status & Hours: Employed, working 30-39 hours per week"												if uprind_cat == "uprind_cat_employment_4"
*replace uprind_cat = "Employment Status & Hours: Employed, working 40-49 hours per week"												if uprind_cat == "uprind_cat_employment_5"
*replace uprind_cat = "Employment Status & Hours: Employed, working 50 or more hours per week"											if uprind_cat == "uprind_cat_employment_6"
replace uprind_cat = "Reliant on Government Payments: No"																				if uprind_cat == "uprind_cat_benefitreliant_0"
replace uprind_cat = "Reliant on Government Payments: Yes"																			if uprind_cat == "uprind_cat_benefitreliant_1"
replace uprind_cat = "Degree of Health Condition: Does not have a long-term health condition"											if uprind_cat == "uprind_cat_healthcondition_0"
replace uprind_cat = "Degree of Health Condition: Has a long-term health condition with either no or low-medium impact on work"		if uprind_cat == "uprind_cat_healthcondition_1"
replace uprind_cat = "Degree of Health Condition: Has a long-term health condition with a high impact on work"						if uprind_cat == "uprind_cat_healthcondition_2"
replace uprind_cat = "Relationship Status: Married"																					if uprind_cat == "uprind_cat_relationship_1"
replace uprind_cat = "Relationship Status: De facto"																					if uprind_cat == "uprind_cat_relationship_2"
replace uprind_cat = "Relationship Status: Separated, divorced or widowed"															if uprind_cat == "uprind_cat_relationship_3"
replace uprind_cat = "Relationship Status: Never married and not de facto"															if uprind_cat == "uprind_cat_relationship_4"
replace uprind_cat = "Number of Adults in Household: 1 adult"																			if uprind_cat == "uprind_cat_adultsinhh_1"
replace uprind_cat = "Number of Adults in Household: 2 adults"																		if uprind_cat == "uprind_cat_adultsinhh_2"
replace uprind_cat = "Number of Adults in Household: 3 or more adults"																if uprind_cat == "uprind_cat_adultsinhh_3"
replace uprind_cat = "Number of Children in Household: No children"																	if uprind_cat == "uprind_cat_childreninhh_0"
replace uprind_cat = "Number of Children in Household: 1 child"																		if uprind_cat == "uprind_cat_childreninhh_1"
replace uprind_cat = "Number of Children in Household: 2 or more children"															if uprind_cat == "uprind_cat_childreninhh_2"
replace uprind_cat = "Household owns its dwelling: No"																				if uprind_cat == "uprind_cat_owner_0"
replace uprind_cat = "Household owns its dwelling: Yes"																				if uprind_cat == "uprind_cat_owner_1"
replace uprind_cat = "Household likely to move in next 12 months: No"																	if uprind_cat == "uprind_cat_likelymove_0"
replace uprind_cat = "Household likely to move in next 12 months: Yes"																if uprind_cat == "uprind_cat_likelymove_1"
replace uprind_cat = "Member of Top-Up Sample: No"																					if uprind_cat == "uprind_cat_topupsample_0"
replace uprind_cat = "Member of Top-Up Sample: Yes"																					if uprind_cat == "uprind_cat_topupsample_1"
order uprind_cat Wave_16 Wave_15 Wave_14 Wave_13 Wave_12
save "$workinglocation\unconditionals_catlevel_TopUp_period`period'", replace
list
export  excel "$resultslocation\Phase 3 - Results grouped across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("Unconditional, Cat, TopUp, P`period'") sheetmodify firstrow(variables) cell(A1)
end

groupwavegraphs 1
groupwavegraphs 3


capture log close

* Just frequencies
capture program drop frequencies
program define frequencies
args wave

local initialwave=$initialwave

* Retrieve dataset saved above and extract current wave
clear
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'
drop wave

* Save log
capture log close
log using "$logslocation\RIndicatorsAndCVs_Log_wave`wave'" , replace

* Create tabulations of each variable by sample type and response
display in red "Frequencies for overall sample - wave `initialwave' characteristics for modelling wave `wave' response propensities"
forvalues period = 1/3 {
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		tabulate `variable' response_P`period', miss co
	}
}
drop if iw_hhtup~=0
display in red "Frequencies for Main sample - wave `initialwave' characteristics for modelling wave `wave' response propensities"
forvalues period = 1/3 {
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		tabulate `variable' response_P`period', miss co
	}
}
use "$workinglocation\propensities_wave11respondents.dta", clear
keep if wave==`wave'&iw_hhtup==1
display in red "Frequencies for TopUp sample - wave `initialwave' characteristics for modelling wave `wave' response propensities"
forvalues period = 1/3 {
	foreach set of global predictors {
		local variable = subinstr("`set'","i.","",1)
		tabulate `variable' response_P`period', miss co
	}
}
capture log close
end
frequencies 12
frequencies 13
frequencies 14
frequencies 15
frequencies 16


* R-Indicator, SE and CV graphs for comparing simulations

capture program drop rindgraphs
program define rindgraphs
args sample
forvalues wave=$startwave/$endwave {
	forvalues period=1/3 {
		clear
		import excel "$workinglocation\RISQoutput_w11r_wave`wave'_P`period'_`sample'.xlsx" , firstrow case(preserve)
		keep r_indicator 
		drop if r_indicator==.
		gen Wave=`wave'
		rename r_indicator P`period'_R
		order Wave P`period'_R 
		save "$workinglocation\rindCVSEs_`sample'_wave`wave'_P`period'", replace
	}
	use "$workinglocation\rindCVSEs_`sample'_wave`wave'_P1", clear
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P2"
	drop _merge
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P3"
	drop _merge
	save "$workinglocation\rindCVSEs_`sample'_wave`wave'", replace
}
use "$workinglocation\rindCVSEs_`sample'_wave12", clear
append using "$workinglocation\rindCVSEs_`sample'_wave13"
append using "$workinglocation\rindCVSEs_`sample'_wave14"
append using "$workinglocation\rindCVSEs_`sample'_wave15"
append using "$workinglocation\rindCVSEs_`sample'_wave16"
export  excel "$resultslocation\Phase 4 - RIndicators across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("`sample' Sample") sheetmodify firstrow(variables) cell(A1)
end

rindgraphs Main
rindgraphs TopUp

capture program drop rindgraphsse
program define rindgraphsse
args sample
forvalues wave=$startwave/$endwave {
	forvalues period=1/3 {
		clear
		import excel "$workinglocation\RISQoutput_w11r_wave`wave'_P`period'_`sample'.xlsx" , firstrow case(preserve)
		keep StdErr_r 
		drop if StdErr_r==.
		gen Wave=`wave'
		rename StdErr_r P`period'_R_SD
		order Wave P`period'_R_SD 
		save "$workinglocation\rindCVSEs_`sample'_wave`wave'_P`period'", replace
	}
	use "$workinglocation\rindCVSEs_`sample'_wave`wave'_P1", clear
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P2"
	drop _merge
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P3"
	drop _merge
	save "$workinglocation\rindCVSEs_`sample'_wave`wave'", replace
}
use "$workinglocation\rindCVSEs_`sample'_wave12", clear
append using "$workinglocation\rindCVSEs_`sample'_wave13"
append using "$workinglocation\rindCVSEs_`sample'_wave14"
append using "$workinglocation\rindCVSEs_`sample'_wave15"
append using "$workinglocation\rindCVSEs_`sample'_wave16"
export  excel "$resultslocation\Phase 4 - RIndicators across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("`sample' Sample") sheetmodify firstrow(variables) cell(A8)
end

rindgraphsse Main
rindgraphsse TopUp

capture program drop cvgraphs
program define cvgraphs
args sample
forvalues wave=$startwave/$endwave {
	forvalues period=1/3 {
		clear
		import excel "$workinglocation\RISQoutput_w11r_wave`wave'_P`period'_`sample'.xlsx" , firstrow case(preserve)
		keep CV_prop_adj 
		drop if CV_prop_adj==.
		gen Wave=`wave'
		rename CV_prop_adj P`period'_CV
		order Wave P`period'_CV 
		save "$workinglocation\rindCVSEs_`sample'_wave`wave'_P`period'", replace
	}
	use "$workinglocation\rindCVSEs_`sample'_wave`wave'_P1", clear
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P2"
	drop _merge
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P3"
	drop _merge
	save "$workinglocation\rindCVSEs_`sample'_wave`wave'", replace
}
use "$workinglocation\rindCVSEs_`sample'_wave12", clear
append using "$workinglocation\rindCVSEs_`sample'_wave13"
append using "$workinglocation\rindCVSEs_`sample'_wave14"
append using "$workinglocation\rindCVSEs_`sample'_wave15"
append using "$workinglocation\rindCVSEs_`sample'_wave16"
export  excel "$resultslocation\Phase 4 - RIndicators across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("`sample' Sample") sheetmodify firstrow(variables) cell(A15)
end

cvgraphs Main
cvgraphs TopUp

capture program drop cvgraphsse
program define cvgraphsse
args sample
forvalues wave=$startwave/$endwave {
	forvalues period=1/3 {
		clear
		import excel "$workinglocation\RISQoutput_w11r_wave`wave'_P`period'_`sample'.xlsx" , firstrow case(preserve)
		keep StdErr_CV_adj
		drop if StdErr_CV_adj==.
		gen Wave=`wave'
		rename StdErr_CV_adj P`period'_CV_SD
		order Wave P`period'_CV_SD
		save "$workinglocation\rindCVSEs_`sample'_wave`wave'_P`period'", replace
	}
	use "$workinglocation\rindCVSEs_`sample'_wave`wave'_P1", clear
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P2"
	drop _merge
	merge 1:1 Wave using "$workinglocation\rindCVSEs_`sample'_wave`wave'_P3"
	drop _merge
	save "$workinglocation\rindCVSEs_`sample'_wave`wave'", replace
}
use "$workinglocation\rindCVSEs_`sample'_wave12", clear
append using "$workinglocation\rindCVSEs_`sample'_wave13"
append using "$workinglocation\rindCVSEs_`sample'_wave14"
append using "$workinglocation\rindCVSEs_`sample'_wave15"
append using "$workinglocation\rindCVSEs_`sample'_wave16"
export  excel "$resultslocation\Phase 4 - RIndicators across waves - Wave 11 respondents and characteristics - base.xlsx" , ///
		sheet("`sample' Sample") sheetmodify firstrow(variables) cell(A22)
end

cvgraphsse Main
cvgraphsse TopUp




