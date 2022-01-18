

* Run parameters
set more off
*pause off
pause on
macro drop _all

* Specify HILDA release
global release = 160

* Specify waves in which to model response propensities on the previous wave's respondents
global startwave = 12
global startwavep1 = 13
global endwave   = 16

* Other global and directory definitions
global alphabet "abcdefghijklmnopqrstuvwxyz"
global releasenum = substr("$release",1,2)
global releaseu = "$release"+"u"
global releasew = substr("$alphabet",$releasenum,1)
global hildalocation   "X:\HILDA\Release $release\files\STATA $releaseu"
global workinglocation "H:\E\working_arc\Adaptivedesign"
global resultslocation "H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Results"
global logslocation    "H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Logfiles"

set seed 54168564

capture log close
log using "$logslocation\Models_w11vars_withcallrecordinfo_sim" , replace

global predictors_subsample i.agerange i.employment i.owner i.sectionofstate i.relationship i.likelymove i.healthcondition i.countryofbirth i.education i.sex i.childreninhh i.adultsinhh i.benefitreliant i.ftstudent

*********************************************************************************************************************************************************************************

********************************************************** PROPENSITIES TO RESPOND IN PERIOD 2 or 3 ******************************************************************************

display in red "**********************************************************************************************************************************************************************************************************************"
display in red "Modelling response propensities for period 2 and 3 given no response in period 1"
display in red "**********************************************************************************************************************************************************************************************************************"

* create long file of call record info
forvalues wave=$startwave/$endwave {
  local w=substr("abcdefghijklmnopqrstuvwxyz",`wave',1)
  if "`wave'"=="$startwave" {
    use "H:\E\working_arc\Adaptivedesign\callsum`w'.dta" , clear
    gen wave=$startwave
  }
  else {
    append using "H:\E\working_arc\Adaptivedesign\callsum`w'.dta" 
    replace wave=`wave' if wave==.
  }
}
gen p1callcount2=p1callcount*p1callcount
gen p1propnc2=p1propnc*p1propnc
gen p1duration2=p1duration*p1duration

*get callcount for P2-P3
gen p23callcount=callcount-p1callcount
replace p23callcount=0 if p23callcount==.

label variable p1callcount   "Number of calls made in P1"
label variable p1propnc      "Prop of non-contact calls in P1"
label variable p1duration    "Span of days for calls made in P1"
label variable p1lastoutcome "Outcome of last call in P1"
label variable p1type        "Type of last call in P1"
label variable p1lasttime    "Time of last call in P1"
label variable p1lastday     "Day of last call in P1"
label variable p1ivw         "Interviewed in HH in P1"

label define p1lastoutcome_label	1	"Interview" ///
									2	"Contact, appointment made" ///
									3	"Contact, no appointment made" ///
									4	"Non-contact" ///
									5	"HH outcome coded" ///
									6	"HH moved" ///
									10  "Missing"
									
label define p1type_label	1	"Face-to-face" ///
							2	"Phone" ///
							10  "Missing"
									
label define p1lasttime_label	1	"Morning" ///
								2	"Afternoon" ///
								3	"Evening" ///
								4	"Night" ///
								10  "Missing"

label define p1lastday_label	1	"Sunday" ///
								2	"Monday" ///
								3	"Tuesday" ///
								4	"Wednesday" ///
								5	"Thursday" ///
								6	"Friday" ///
								7	"Saturday" ///
								10  "Missing"

label define p1ivw_label	0	"No HH member interviewed during initial fieldwork" ///
							1	"At least one HH member interviewed during initial fieldwork" ///
							10  "Missing"

* apply labels
foreach var in p1lastoutcome p1type p1lasttime p1lastday p1ivw {
  label values `var' `var'_label
}	       

* merge call record with dataset of previous wave respondents
sort wave hhid
merge 1:m wave hhid using "$workinglocation\propensities_wave11respondents.dta" 
keep if _merge==2 | _merge==3
gen rP3_given_nrP1=response_P3 if response_P1==0
* set call counts to zero if issued but does not have a call record (eg untraceable, overseas, etc) - this could occur if a HF record was not created for the HH but the outcome is assigned on master file
gen nocallrecord=(issued==1 & callcount==.)  // issued but no calls recorded
replace callcount=0 if issued==1 & callcount==.
replace p1callcount=0 if issued==1 & p1callcount==.
replace p23callcount=0 if issued==1 & p23callcount==.

capture drop havecallobs misscallobs
gen havecallobs=1 if p1callcount!=. & p1lastoutcome!=. & p1type!=. & p1lasttime!=. & p1lastday!=. & p1propnc!=. & p1ivw!=. & p1duration!=. 
replace havecallobs=0 if havecallobs==.
gen misscallobs=1-havecallobs

* Code reason for no call record info
tab p1lastoutcome if issued==1, miss
gen misscallobs_reason=0 if issued==1
replace misscallobs_reason=1 if misscallobs_reason==0 & nocallrecord==1 & issued==1 & (fstatus_master==4 | fstatus_master==12 | fstatus_master==6)  // nocallrecord - out of scope (overseas, dead, tsm moved out - could be prefield calls)
replace misscallobs_reason=2 if misscallobs_reason==0 & nocallrecord==1 & issued==1 & (fstatus_master==99)  // nocallrecord - lost to tracking
replace misscallobs_reason=3 if misscallobs_reason==0 & nocallrecord==1 & issued==1  // nocallrecord - other reasons for no call record
count if misscallobs_reason==0 & hhresp==. & hhresp_master<100 // check other nocallrecord - HF not issued to interviewer - mostly untraceable HH or work completed before field - there are none of these
replace misscallobs_reason=4 if p1lastoutcome==10 & p1type==10 & p1lasttime==10 & p1lastday==10 & p1propnc==0 & p1ivw==0 & p1duration==0 & p1callcount==0 & p23callcount>0 & p23callcount!=. & issued==1  // issued only to follow up (eg splits or not found until follow up)
replace misscallobs_reason=5 if misscallobs_reason==0 & (p1lastoutcome==. | p1type==. | p1lasttime==. | p1lastday==. | p1propnc==. | p1ivw==. | p1duration==.) & issued==1  // miscelaneous missing on call info

gen nocallp1=(misscallobs_reason==4) if issued==1

tab misscallobs_reason misscallobs if issued==1, miss
list xwaveid wave hhid hgivw hhresp hhresp_master issued misscallobs_reason nocallrecord callcount p23callcount p1callcount p1lastoutcome p1type p1lasttime p1lastday p1propnc p1ivw p1duration misscallobs_reason if issued==1 & misscallobs_reason==0 & misscallobs==1, nolabel

* code remaining missingness (cases with missing call observations n=231 plus cases missing some but not all call info vars n=10 overall)
foreach var in p1lastoutcome p1type p1lasttime p1lastday {							
label values `var' `var'_label		
replace `var'=10 if issued==1 & `var'==.
}
foreach var in p1callcount p1f2fcount p1propnc p1duration p1ivw {
replace `var'=0 if issued==1 & `var'==.	
}

* Define programs to be called in the rindicators program
capture program drop predp3
program define predp3
args samplenumber wave

if `samplenumber'==1 {
local samp = "Main"
}
if `samplenumber'==2 {
local samp = "TopUp"
}

* define regression to apply based on wave and hhtup=0 for main and 1 for topup
display in red "Model of response P1 with prev wave char, subsample=`samp', wave=`wave'"
display in red "xi:logistic response_P1 $predictors_subsample if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef"
xi:logistic response_P1 $predictors_subsample if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef
estimates store rp1_w`wave'_s`samplenumber'_orig
predict pred_rp1_w`wave'_s`samplenumber'_orig if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, pr
lroc
display in red "Model of response P3 | no response P1 with prev wave char, subsample=`samp', wave=`wave'"
display in red "xi:logistic rP3_given_nrP1 $predictors_subsample if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef"
xi:logistic rP3_given_nrP1 $predictors_subsample if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef
estimates store rp3_w`wave'_s`samplenumber'_orig
* use model to predict response in next wave
predict pred_rP3_given_nrP1_w`wave'_s`samplenumber'_orig if wave==`wave'+1&iw_hhtup==`samplenumber'-1&issued==1, pr
lroc
display in red "Model of response P3 | no response P1 with prev wave char and call record info, subsample=`samp', wave=`wave'"
display in red "xi:logistic rP3_given_nrP1 $predictors_subsample /*misscallobs nocallp1*/ p1callcount  p1callcount2 i.p1lastoutcome i.p1type i.p1lasttime i.p1lastday p1propnc p1propnc2 i.p1ivw p1duration p1duration2 if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef"
xi:logistic rP3_given_nrP1 $predictors_subsample /*misscallobs nocallp1*/ p1callcount  p1callcount2 i.p1lastoutcome i.p1type i.p1lasttime i.p1lastday p1propnc p1propnc2 i.p1ivw p1duration p1duration2 if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef
estimates store rp3_w`wave'_s`samplenumber'_call
* use model to predict response in next wave
predict pred_rP3_given_nrP1_w`wave'_s`samplenumber'_call if wave==`wave'+1&iw_hhtup==`samplenumber'-1&issued==1, pr
lroc
display in red "Model of response P3 | no response P1 with prev wave char and key call record info, subsample=`samp', wave=`wave'"
display in red "xi:logistic rP3_given_nrP1 $predictors_subsample /*misscallobs nocallp1*/ i.p1lastoutcome p1duration p1duration2 if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef"
xi:logistic rP3_given_nrP1 $predictors_subsample /*misscallobs nocallp1*/ i.p1lastoutcome p1duration p1duration2 if wave==`wave'&iw_hhtup==`samplenumber'-1&issued==1, coef
estimates store rp3_w`wave'_s`samplenumber'_call2
* use model to predict response in next wave
predict pred_rP3_given_nrP1_w`wave'_s`samplenumber'_call2 if wave==`wave'+1&iw_hhtup==`samplenumber'-1&issued==1, pr
lroc

end

capture log close
log using "$logslocation\Models_w11vars_withcallrecordinfo" , replace

forvalues wave=$startwave/$endwave {
	predp3 1 `wave'
	predp3 2 `wave'
}

*ssc install estout, replace
estout rp1_w12_s1_orig rp1_w13_s1_orig rp1_w14_s1_orig rp1_w15_s1_orig rp1_w16_s1_orig ///
       rp1_w12_s2_orig rp1_w13_s2_orig rp1_w14_s2_orig rp1_w15_s2_orig rp1_w16_s2_orig ///
	   rp3_w12_s1_orig rp3_w13_s1_orig rp3_w14_s1_orig rp3_w15_s1_orig rp3_w16_s1_orig ///
	   rp3_w12_s1_call rp3_w13_s1_call rp3_w14_s1_call rp3_w15_s1_call rp3_w16_s1_call ///
       rp3_w12_s2_orig rp3_w13_s2_orig rp3_w14_s2_orig rp3_w15_s2_orig rp3_w16_s2_orig /// 
	   rp3_w12_s2_call rp3_w13_s2_call rp3_w14_s2_call rp3_w15_s2_call rp3_w16_s2_call /// 
	   rp3_w12_s1_call2 rp3_w13_s1_call2 rp3_w14_s1_call2 rp3_w15_s1_call2 rp3_w16_s1_call2 ///
	   rp3_w12_s2_call2 rp3_w13_s2_call2 rp3_w14_s2_call2 rp3_w15_s2_call2 rp3_w16_s2_call2 /// 
	   using  "$resultslocation\logit_rp3_nrp1_w11resp.txt", ///
	   cells(b(star fmt(3)) /* se(par fmt(3)) */) style(tab) replace starlevels("*" 0.1 "**" 0.05 "***" 0.01) stats(ll N r2_p) 

estout rp1_w12_s1_orig rp1_w13_s1_orig rp1_w14_s1_orig rp1_w15_s1_orig rp1_w16_s1_orig ///
       rp1_w12_s2_orig rp1_w13_s2_orig rp1_w14_s2_orig rp1_w15_s2_orig rp1_w16_s2_orig ///
	   rp3_w12_s1_orig rp3_w13_s1_orig rp3_w14_s1_orig rp3_w15_s1_orig rp3_w16_s1_orig ///
	   rp3_w12_s1_call rp3_w13_s1_call rp3_w14_s1_call rp3_w15_s1_call rp3_w16_s1_call ///
       rp3_w12_s2_orig rp3_w13_s2_orig rp3_w14_s2_orig rp3_w15_s2_orig rp3_w16_s2_orig /// 
	   rp3_w12_s2_call rp3_w13_s2_call rp3_w14_s2_call rp3_w15_s2_call rp3_w16_s2_call /// 
	   rp3_w12_s1_call2 rp3_w13_s1_call2 rp3_w14_s1_call2 rp3_w15_s1_call2 rp3_w16_s1_call2 ///
	   rp3_w12_s2_call2 rp3_w13_s2_call2 rp3_w14_s2_call2 rp3_w15_s2_call2 rp3_w16_s2_call2 /// 
	   using  "$resultslocation\logit_rp3_nrp1_w11resp_withse.txt", ///
	   cells(b(star fmt(3)) se(par fmt(3))) style(tab) replace stats(N ll r2_p df aic bic) 
	   
* check call info is similar	   
graph twoway scatter pred_rP3_given_nrP1_w12_s1_call pred_rP3_given_nrP1_w12_s1_call2 if response_P1==0 & wave==13 & iw_hhtup==0
corr pred_rP3_given_nrP1_w12_s1_call pred_rP3_given_nrP1_w12_s1_call2 if response_P1==0 & wave==13 & iw_hhtup==0
corr pred_rP3_given_nrP1_w13_s1_call pred_rP3_given_nrP1_w13_s1_call2 if response_P1==0 & wave==14 & iw_hhtup==0
corr pred_rP3_given_nrP1_w14_s1_call pred_rP3_given_nrP1_w14_s1_call2 if response_P1==0 & wave==15 & iw_hhtup==0
corr pred_rP3_given_nrP1_w15_s1_call pred_rP3_given_nrP1_w15_s1_call2 if response_P1==0 & wave==16 & iw_hhtup==0

graph twoway scatter pred_rP3_given_nrP1_w12_s2_call pred_rP3_given_nrP1_w12_s2_call2 if response_P1==0 & wave==13 & iw_hhtup==1
corr pred_rP3_given_nrP1_w12_s2_call pred_rP3_given_nrP1_w12_s2_call2 if response_P1==0 & wave==13 & iw_hhtup==1
corr pred_rP3_given_nrP1_w13_s2_call pred_rP3_given_nrP1_w13_s2_call2 if response_P1==0 & wave==14 & iw_hhtup==1
corr pred_rP3_given_nrP1_w14_s2_call pred_rP3_given_nrP1_w14_s2_call2 if response_P1==0 & wave==15 & iw_hhtup==1
corr pred_rP3_given_nrP1_w15_s2_call pred_rP3_given_nrP1_w15_s2_call2 if response_P1==0 & wave==16 & iw_hhtup==1
	   
sum pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call2 if response_P1==0 & wave==13 & iw_hhtup==0, detail
corr pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call2 if response_P1==0 & wave==13 & iw_hhtup==0
sum pred_rp1_w13_s2_orig pred_rP3_given_nrP1_w12_s2_call2 if response_P1==0 & wave==13 & iw_hhtup==1, detail	   
corr pred_rp1_w13_s2_orig pred_rP3_given_nrP1_w12_s2_call2 if response_P1==0 & wave==13 & iw_hhtup==1

graph twoway scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & wave==13 & iw_hhtup==0, xline(0.6746746) yline(0.842396) saving("$resultslocation\rp3_nrp1_w13_main_w11resp.gph", replace)
graph twoway scatter pred_rp1_w13_s2_orig pred_rP3_given_nrP1_w12_s2_call if response_P1==0 & wave==13 & iw_hhtup==1, xline(0.4917435) yline(0.7625944) saving("$resultslocation\rp3_nrp1_w13_topup_w11resp.gph", replace)

sum pred_rp1_w16_s1_orig pred_rP3_given_nrP1_w15_s1_call2 if response_P1==0 & wave==16 & iw_hhtup==0, detail
corr pred_rp1_w16_s1_orig pred_rP3_given_nrP1_w15_s1_call2 if response_P1==0 & wave==16 & iw_hhtup==0
sum pred_rp1_w16_s2_orig pred_rP3_given_nrP1_w15_s2_call2 if response_P1==0 & wave==16 & iw_hhtup==1, detail	   
corr pred_rp1_w16_s2_orig pred_rP3_given_nrP1_w15_s2_call2 if response_P1==0 & wave==16 & iw_hhtup==1

graph twoway scatter pred_rp1_w16_s1_orig pred_rP3_given_nrP1_w15_s1_call if response_P1==0 & wave==16 & iw_hhtup==0, xline(0.3869235) yline(0.8395734) saving("$resultslocation\rp3_nrp1_w16_main_w11resp.gph", replace)
graph twoway scatter pred_rp1_w16_s2_orig pred_rP3_given_nrP1_w15_s2_call if response_P1==0 & wave==16 & iw_hhtup==1, xline(0.2521165) yline(0.7513291) saving("$resultslocation\rp3_nrp1_w16_topup_w11resp.gph", replace)
	   
save "$workinglocation\propensities_w11r_for_sim.dta", replace 
log close

capture log close
log using "$logslocation\simulated_response_outcomes" , replace
  
* Create simulated response outcomes
use "$workinglocation\propensities_w11r_for_sim.dta", clear 

sum pred_rP3_given_nrP1_w*_s*_call pred_rp1_w*_s*_orig

gen response_P2_opt1=response_P2  // OPTION 1: Focus on cases most likely to respond in P2 and P3
gen response_P2_opt2=response_P2  // OPTION 2: Focus on cases most likely to improve sample balance (R-indicator) by end of P3
gen response_P2_opt3=response_P2  // OPTION 3: Combination 1 of the first two options - same % increase in cutoff rate
gen response_P2_opt4=response_P2  // OPTION 4: Combination 2 of the first two options - top 75% of the sum of the probabilities

gen response_P2_opt5=response_P2  // OPTION 1 AT HH LEVEL: Focus on cases most likely to respond in P2 and P3 (HH takes value of max prob)
gen response_P2_opt6=response_P2  // OPTION 2 AT HH LEVEL: Focus on cases most likely to improve sample balance (R-indicator) by end of P3 (HH takes value of max prob)
gen response_P2_opt7=response_P2  // OPTION 3 AT HH LEVEL: Combination 1 of the first two options - same % increase in cutoff rate (HH takes value of max prob)
gen response_P2_opt8=response_P2  // OPTION 4 AT HH LEVEL: Combination 2 of the first two options - top 75% of the sum of the probabilities (HH takes value of max prob)
gen response_P2_opt9=response_P2  // OPTION 5 AT HH LEVEL: Random 25%

gen response_P3_opt1=response_P3  // OPTION 1: Focus on cases most likely to respond in P2 and P3
gen response_P3_opt2=response_P3  // OPTION 2: Focus on cases most likely to improve sample balance (R-indicator) by end of P3
gen response_P3_opt3=response_P3  // OPTION 3: Combination 1 of the first two options - same % increase in cutoff rate
gen response_P3_opt4=response_P3  // OPTION 4: Combination 2 of the first two options - top 75% of the sum of the probabilities

gen response_P3_opt5=response_P3  // OPTION 1 AT HH LEVEL: Focus on cases most likely to respond in P2 and P3
gen response_P3_opt6=response_P3  // OPTION 2 AT HH LEVEL: Focus on cases most likely to improve sample balance (R-indicator) by end of P3
gen response_P3_opt7=response_P3  // OPTION 3 AT HH LEVEL: Combination 1 of the first two options - same % increase in cutoff rate
gen response_P3_opt8=response_P3  // OPTION 4 AT HH LEVEL: Combination 2 of the first two options - top 75% of the sum of the probabilities
gen response_P3_opt9=response_P3  // OPTION 5 AT HH LEVEL: Random 75%

gen cut_nrp1=.
gen cut_rp3=.
gen cut_sum=.

gen cut_nrp1_hh=.
gen cut_rp3_hh=.
gen cut_sum_hh=.

* add random number to each household
sort wave hhid pers
by wave hhid: gen obs=_n
gen rannum_head=runiform() if obs==1
bys wave hhid: egen rannum=mean(rannum_head)

global cutoff=25  // Drop this percent of cases after P1 fieldwork
display "cutoff=$cutoff"

* get percentiles and create four alternative fieldwork options with revised response outcomes at end of P3
* intialise flag for whether issue to field in P2-P3
capture drop field_opt*
gen field_opt1=0
gen field_opt2=0
gen field_opt3=0
gen field_opt4=0
gen field_opt5=0
gen field_opt6=0
gen field_opt7=0
gen field_opt8=0
gen field_opt9=0

forvalues s=1/2 {
  forvalues wave=$startwavep1/$endwave {
  local pw=`wave'-1
    
    * percentiles of response propensities
    local topup=`s'-1
    display "wave=`wave', prevwave=`pw', sample=`s', topup=`topup', cutoff=$cutoff"
	
	capture drop pred_nrp1_w`wave'_s`s'_orig
	gen pred_nrp1_w`wave'_s`s'_orig=1-pred_rp1_w`wave'_s`s'_orig
	gen temp=pred_nrp1_w`wave'_s`s'_orig
	replace temp=0 if pred_nrp1_w`wave'_s`s'_orig==. & response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' // ensure cases with missing predicted values are issued
    sum temp if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', detail	
	replace cut_nrp1=r(p$cutoff)
	drop temp
	
	gen temp=pred_rP3_given_nrP1_w`pw'_s`s'_call
	replace temp=1 if pred_rP3_given_nrP1_w`pw'_s`s'_call==. & response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' // ensure cases with missing predicted values are issued
    sum temp if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', detail	
	replace cut_rp3=r(p$cutoff)
	drop temp
	
	egen rank_pred_rP3_nrP1_w`pw'_s`s'_call=rank(pred_rP3_given_nrP1_w`pw'_s`s'_call) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', track
	egen rank_pred_nrp1_w`wave'_s`s'_orig=rank(pred_nrp1_w`wave'_s`s'_orig) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', track
	capture drop sum_pred
	gen sum_pred=pred_nrp1_w`wave'_s`s'_orig+pred_rP3_given_nrP1_w`pw'_s`s'_call
    sum sum_pred if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', detail	
	replace cut_sum=r(p$cutoff)
	list cut_nrp1 cut_rp3 cut_sum if _n==1

	* calculate the same at HH level - by taking average of response rates (ie selects HH if non-respondents more likely to respond on average)
	bys wave hhid: egen pred_nrp1_w`wave'_s`s'_orig_hh=mean(pred_nrp1_w`wave'_s`s'_orig) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
	gen temp=pred_nrp1_w`wave'_s`s'_orig_hh
	replace temp=0 if pred_nrp1_w`wave'_s`s'_orig_hh==. & response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' // ensure hh with missing predicted values are issued
    sum temp if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', detail	
	replace cut_nrp1_hh=r(p$cutoff)
	drop temp
	
	bys wave hhid: egen pred_rP3_given_nrP1_w`pw'_s`s'_chh=mean(pred_rP3_given_nrP1_w`pw'_s`s'_call) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
	gen temp=pred_rP3_given_nrP1_w`pw'_s`s'_chh
	replace temp=1 if pred_rP3_given_nrP1_w`pw'_s`s'_chh==. & response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
    sum temp if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', detail	
	replace cut_rp3_hh=r(p$cutoff)
	drop temp
	
	egen rank_pred_rP3_nrP1_w`pw'_s`s'_chh=rank(pred_rP3_given_nrP1_w`pw'_s`s'_chh) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', track
	egen rank_pred_nrp1_w`wave'_s`s'_orig_hh=rank(pred_nrp1_w`wave'_s`s'_orig_hh) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', track
	capture drop sum_pred_hh
	gen sum_pred_hh=pred_nrp1_w`wave'_s`s'_orig_hh+pred_rP3_given_nrP1_w`pw'_s`s'_chh
    sum sum_pred_hh if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup', detail	
	replace cut_sum_hh=r(p$cutoff)
	list cut_nrp1_hh cut_rp3_hh cut_sum_hh if _n==1
	
	*alternative fieldwork options
	* Option 1 - cases most likely to respond in P2 and P3
	display "Option 1"
    replace field_opt1=(pred_rP3_given_nrP1_w`pw'_s`s'_call>=cut_rp3) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& pred_rP3_given_nrP1_w`pw'_s`s'_call!=.*/
    tab field_opt1 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt1 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt1=response_P1 if field_opt1==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt1=response_P1 if field_opt1==0 & wave==`wave' & iw_hhtup==`topup'
	
	* Option 2 - cases most likely to improve sample balance
	display "Option 2"
    replace field_opt2=(pred_nrp1_w`wave'_s`s'_orig>=cut_nrp1) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& pred_nrp1_w`wave'_s`s'_orig!=.*/
    tab field_opt2 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt2 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt2=response_P1 if field_opt2==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt2=response_P1 if field_opt2==0 & wave==`wave' & iw_hhtup==`topup'
	
	* Option 3 - using combination of improving response rate and sample balance - rank cases and take people as ordered by rank
	* rank 1 for worst case, so find the 25% that we dont want to issue.
	display "Option 3"
	quietly replace field_opt3=1 if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' 
	forvalues i=1(1)4000 { 
	  capture drop prop
      quietly replace field_opt3=0 if ((rank_pred_rP3_nrP1_w`pw'_s`s'_call==`i') | (rank_pred_nrp1_w`wave'_s`s'_orig==`i')) & response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' & pred_rP3_given_nrP1_w`pw'_s`s'_call!=. & pred_nrp1_w`wave'_s`s'_orig!=.
      quietly sum field_opt3 if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
	  quietly scalar prop=r(mean)
	  if (prop<0.75) {
	    display "Final rank=`i', wave=`wave' topup=`topup'"
		continue, break
	  }
	}
    tab field_opt3 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt3 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt3=response_P1 if field_opt3==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt3=response_P1 if field_opt3==0 & wave==`wave' & iw_hhtup==`topup'
	
	* Option 4 - cases most likely to improve sample balance
	display "Option 4"
    replace field_opt4=(sum_pred>=cut_sum) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& sum_pred!=.*/
    tab field_opt4 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt4 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt4=response_P1 if field_opt4==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt4=response_P1 if field_opt4==0 & wave==`wave' & iw_hhtup==`topup'

	* Option 5 - cases most likely to respond in P2 and P3 AT HH LEVEL
	display "Option 5"
    replace field_opt5=(pred_rP3_given_nrP1_w`pw'_s`s'_chh>=cut_rp3_hh) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& pred_rP3_given_nrP1_w`pw'_s`s'_chh!=.*/
    tab field_opt5 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt5 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt5=response_P1 if field_opt5==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt5=response_P1 if field_opt5==0 & wave==`wave' & iw_hhtup==`topup'
	
	* Option 6 - cases most likely to improve sample balance AT HH LEVEL
	display "Option 6"
    replace field_opt6=(pred_nrp1_w`wave'_s`s'_orig_hh>=cut_nrp1_hh) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& pred_nrp1_w`wave'_s`s'_orig_hh!=.*/
    tab field_opt6 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt6 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt6=response_P1 if field_opt6==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt6=response_P1 if field_opt6==0 & wave==`wave' & iw_hhtup==`topup'
	
	* Option 7 - using combination of improving response rate and sample balance AT HH LEVEL
	display "Option 7"
	replace field_opt7=1 if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
	forvalues i=1(1)8000 { 
	  capture drop prop
      quietly replace field_opt7=0 if ((rank_pred_rP3_nrP1_w`pw'_s`s'_chh==`i') | (rank_pred_nrp1_w`wave'_s`s'_orig_hh==`i')) & response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& pred_rP3_given_nrP1_w`pw'_s`s'_chh!=. & pred_nrp1_w`wave'_s`s'_orig_hh!=.*/
      quietly sum field_opt7 if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
	  quietly scalar prop=r(mean)
	  if (prop<0.75) {
	    display "Final rank=`i', wave=`wave' topup=`topup'"
		continue, break
	  }
	}
    tab field_opt7 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt7 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt7=response_P1 if field_opt7==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt7=response_P1 if field_opt7==0 & wave==`wave' & iw_hhtup==`topup'
	
	* Option 8 - cases most likely to improve sample balance AT HH LEVEL
	display "Option 8"
    replace field_opt8=(sum_pred_hh>=cut_sum_hh) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' /*& sum_pred_hh!=.*/
    tab field_opt8 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt8 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt8=response_P1 if field_opt8==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt8=response_P1 if field_opt8==0 & wave==`wave' & iw_hhtup==`topup'

	* Option 9 - random 75% cases AT HH LEVEL
	display "Option 9"
    replace field_opt9=(rannum>0.25) if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup' 
    tab field_opt9 if wave==`wave' & iw_hhtup==`topup'
    tab field_opt9 response_P3 if wave==`wave' & iw_hhtup==`topup', row
    replace response_P2_opt9=response_P1 if field_opt9==0 & wave==`wave' & iw_hhtup==`topup'
    replace response_P3_opt9=response_P1 if field_opt9==0 & wave==`wave' & iw_hhtup==`topup'

	* summary of various options
    sum field_opt* if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==`wave' & iw_hhtup==`topup'
	pause
    save "$workinglocation\temp_w11r_w`wave'_s`s'.dta", replace 
	
  }
}

bys wave iw_hhtup: sum response_P2* response_P3*

save "$workinglocation\propensities_w11r_sim.dta", replace 

use "$workinglocation\temp_w11r_w13_s1.dta", clear

sum                  pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call field_opt4 if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0, detail
*graph twoway scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0,  xline(0.639294) yline(0.8367257) msymbol(smcircle) 
graph twoway scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0 & field_opt4==1, xline(0.639294) yline(0.8367257) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(1 "Opt4 issue")) msymbol(smcircle) ///
          || scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0 & field_opt4==0, xline(0.639294) yline(0.8367257) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(2 "Opt4 not issue")) msymbol(smx) saving("$resultslocation\simgraph_w11r_main_w13.gph", replace) 
graph export "$resultslocation\simgraph_w11r_main_w13.png", replace
pause

use "$workinglocation\temp_w11r_w13_s1.dta", clear

sum                  pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call field_opt4 if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0, detail
*graph twoway scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0,  xline(0.639294) yline(0.8367257) msymbol(smcircle) 
graph twoway scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0 & field_opt3==1, xline(0.639294) yline(0.8367257) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(1 "Opt3 issue")) msymbol(smcircle) ///
          || scatter pred_rp1_w13_s1_orig pred_rP3_given_nrP1_w12_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==0 & field_opt3==0, xline(0.639294) yline(0.8367257) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(2 "Opt3 not issue")) msymbol(smx) saving("$resultslocation\simgraph_w11r_main_w13.gph", replace) 
graph export "$resultslocation\simgraph_w11r_main_opt3_w13.png", replace
pause


use "$workinglocation\temp_w11r_w13_s2.dta", clear

*graph twoway scatter pred_rp1_w13_s2_orig pred_rP3_given_nrP1_w12_s2_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==1,  xline(0.4917435) yline(0.7625944) msymbol(smcircle) 
graph twoway scatter pred_rp1_w13_s2_orig pred_rP3_given_nrP1_w12_s2_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==1 & field_opt4==1,  xline(0.4917435) yline(0.7625944) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(1 "Opt4 issue")) msymbol(smcircle) ///
          || scatter pred_rp1_w13_s2_orig pred_rP3_given_nrP1_w12_s2_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==13 & iw_hhtup==1 & field_opt4==0,  xline(0.4917435) yline(0.7625944) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(2 "Opt4 not issue")) msymbol(smx) saving("$resultslocation\simgraph_w11r_topup_w13.gph", replace) 
pause


use "$workinglocation\temp_w11r_w16_s1.dta", clear 

*graph twoway scatter pred_rp1_w16_s1_orig pred_rP3_given_nrP1_w15_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==16 & iw_hhtup==0, xline(0.3869235) yline(0.8395734) msymbol(smcircle) 
graph twoway scatter pred_rp1_w16_s1_orig pred_rP3_given_nrP1_w15_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==16 & iw_hhtup==0 & field_opt4==1, xline(0.3869235) yline(0.8395734) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(1 "Opt4 issue")) msymbol(smcircle) ///
          || scatter pred_rp1_w16_s1_orig pred_rP3_given_nrP1_w15_s1_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==16 & iw_hhtup==0 & field_opt4==0, xline(0.3869235) yline(0.8395734) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(2 "Opt4 not issue")) msymbol(smx) saving("$resultslocation\simgraph_w11r_main_w16.gph", replace) 
pause

use "$workinglocation\temp_w11r_w16_s2.dta", clear 

graph twoway scatter pred_rp1_w16_s2_orig pred_rP3_given_nrP1_w15_s2_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==16 & iw_hhtup==1,  xline(0.2521165) yline(0.7513291) msymbol(smcircle) 
graph twoway scatter pred_rp1_w16_s2_orig pred_rP3_given_nrP1_w15_s2_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==16 & iw_hhtup==1 & field_opt4==1,  xline(0.2521165) yline(0.7513291) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(1 "Opt4 issue")) msymbol(smcircle) ///
          || scatter pred_rp1_w16_s2_orig pred_rP3_given_nrP1_w15_s2_call if response_P1==0 & p23callcount>0 & p23callcount!=. & wave==16 & iw_hhtup==1 & field_opt4==0,  xline(0.2521165) yline(0.7513291) xtitle("P(R followup | NR initial)") ytitle("P(R initial)") legend(label(2 "Opt4 not issue")) msymbol(smx) saving("$resultslocation\simgraph_w11r_topup_w16.gph", replace) 
pause


log close


