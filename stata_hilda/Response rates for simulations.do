
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

capture log close
log using "$logslocation\resprates for sim.log", replace

* Propensities dataset (wave 11 characteristics, waves 12-16)
use "$workinglocation\propensities_w11r_sim.dta", clear

* Tabulate response rates for each wave by simoption, wave, and wave 11 sample type
bys wave iw_hhtup: sum response_P3*

* Balanced panel response rates - base scenario
destring xwaveid, replace
tsset xwaveid wave

/* Wave 12 */
gen response_bp_w12=1 if wave==12 & response_P3==1
replace response_bp_w12=0 if wave==12 & response_P3==0
/* Wave 13 */
gen response_bp_w13=1 if wave==12 & response_P3==1 & F.response_P3==1
replace response_bp_w13=0 if wave==12 & (response_P3==0|F.response_P3==0)
replace response_bp_w13=. if wave==12 & (response_P3==.|F.response_P3==.)
/* Wave 14 */
gen response_bp_w14=1 if wave==12 & response_P3==1 & F.response_P3==1 & F2.response_P3==1
replace response_bp_w14=0 if wave==12 & (response_P3==0|F.response_P3==0|F2.response_P3==0)
replace response_bp_w14=. if wave==12 & (response_P3==.|F.response_P3==.|F2.response_P3==.)
/* Wave 15 */
gen response_bp_w15=1 if wave==12 & response_P3==1 & F.response_P3==1 & F2.response_P3==1 & F3.response_P3==1
replace response_bp_w15=0 if wave==12 & (response_P3==0|F.response_P3==0|F2.response_P3==0|F3.response_P3==0)
replace response_bp_w15=. if wave==12 & (response_P3==.|F.response_P3==.|F2.response_P3==.|F3.response_P3==.)
/* Wave 16 */
gen response_bp_w16=1 if wave==12 & response_P3==1 & F.response_P3==1 & F2.response_P3==1 & F3.response_P3==1 & F4.response_P3==1
replace response_bp_w16=0 if wave==12 & (response_P3==0|F.response_P3==0|F2.response_P3==0|F3.response_P3==0|F4.response_P3==0)
replace response_bp_w16=. if wave==12 & (response_P3==.|F.response_P3==.|F2.response_P3==.|F3.response_P3==.|F4.response_P3==.)

* Balanced panel response rates - simulation options

capture program drop bprr
program define bprr
args option
/* Wave 12 */
gen response_bp_`option'_w12=1 if wave==12 & response_P3_`option'==1
replace response_bp_`option'_w12=0 if wave==12 & response_P3_`option'==0
/* Wave 13 */
gen response_bp_`option'_w13=1 if wave==12 & response_P3_`option'==1 & F.response_P3_`option'==1
replace response_bp_`option'_w13=0 if wave==12 & (response_P3_`option'==0|F.response_P3_`option'==0)
replace response_bp_`option'_w13=. if wave==12 & (response_P3_`option'==.|F.response_P3_`option'==.)
/* Wave 14 */
gen response_bp_`option'_w14=1 if wave==12 & response_P3_`option'==1 & F.response_P3_`option'==1 & F2.response_P3_`option'==1
replace response_bp_`option'_w14=0 if wave==12 & (response_P3_`option'==0|F.response_P3_`option'==0|F2.response_P3_`option'==0)
replace response_bp_`option'_w14=. if wave==12 & (response_P3_`option'==.|F.response_P3_`option'==.|F2.response_P3_`option'==.)
/* Wave 15 */
gen response_bp_`option'_w15=1 if wave==12 & response_P3_`option'==1 & F.response_P3_`option'==1 & F2.response_P3_`option'==1 & F3.response_P3_`option'==1
replace response_bp_`option'_w15=0 if wave==12 & (response_P3_`option'==0|F.response_P3_`option'==0|F2.response_P3_`option'==0|F3.response_P3_`option'==0)
replace response_bp_`option'_w15=. if wave==12 & (response_P3_`option'==.|F.response_P3_`option'==.|F2.response_P3_`option'==.|F3.response_P3_`option'==.)
/* Wave 16 */
gen response_bp_`option'_w16=1 if wave==12 & response_P3_`option'==1 & F.response_P3_`option'==1 & F2.response_P3_`option'==1 & F3.response_P3_`option'==1 & F4.response_P3_`option'==1
replace response_bp_`option'_w16=0 if wave==12 & (response_P3_`option'==0|F.response_P3_`option'==0|F2.response_P3_`option'==0|F3.response_P3_`option'==0|F4.response_P3_`option'==0)
replace response_bp_`option'_w16=. if wave==12 & (response_P3_`option'==.|F.response_P3_`option'==.|F2.response_P3_`option'==.|F3.response_P3_`option'==.|F4.response_P3_`option'==.)
end

forvalues opt = 1/9 {
bprr opt`opt'
}

* Paired wave response rates - base scenario

/* Wave 12 */
gen response_pw_w12=1 if wave==12 & response_P3==1
replace response_pw_w12=0 if wave==12 & response_P3==0
/* Wave 13 */
gen response_pw_w13=1 if wave==12 & F.response_P3==1
replace response_pw_w13=0 if wave==12 & F.response_P3==0
/* Wave 14 */
gen response_pw_w14=1 if wave==12 & F2.response_P3==1
replace response_pw_w14=0 if wave==12 & F2.response_P3==0
/* Wave 15 */
gen response_pw_w15=1 if wave==12 & F3.response_P3==1
replace response_pw_w15=0 if wave==12 & F3.response_P3==0
/* Wave 16 */
gen response_pw_w16=1 if wave==12 & F4.response_P3==1
replace response_pw_w16=0 if wave==12 & F4.response_P3==0

capture program drop pwrr
program define pwrr
args option
/* Wave 12 */
gen response_pw_`option'_w12=1 if wave==12 & response_P3_`option'==1
replace response_pw_`option'_w12=0 if wave==12 & response_P3_`option'==0
/* Wave 13 */
gen response_pw_`option'_w13=1 if wave==12 & F.response_P3_`option'==1
replace response_pw_`option'_w13=0 if wave==12 & F.response_P3_`option'==0
/* Wave 14 */
gen response_pw_`option'_w14=1 if wave==12 & F2.response_P3_`option'==1
replace response_pw_`option'_w14=0 if wave==12 & F2.response_P3_`option'==0
/* Wave 15 */
gen response_pw_`option'_w15=1 if wave==12 & F3.response_P3_`option'==1
replace response_pw_`option'_w15=0 if wave==12 & F3.response_P3_`option'==0
/* Wave 16 */
gen response_pw_`option'_w16=1 if wave==12 & F4.response_P3_`option'==1
replace response_pw_`option'_w16=0 if wave==12 & F4.response_P3_`option'==0
end

forvalues opt = 1/9 {
pwrr opt`opt'
}

/* summary of response variables - following two lines checks what is produced by subsequent code - gives the same response rates */
bys topupsample: sum response_pw_w12 response_pw_w13 response_pw_w14 response_pw_w15 response_pw_w16 if wave==12
bys topupsample: sum response_pw_opt1_w12 response_pw_opt1_w13 response_pw_opt1_w14 response_pw_opt1_w15 response_pw_opt1_w16 if wave==12

save "$workinglocation\Responserates\responserates", replace

capture program drop rrtablecells
program define rrtablecells
args sample rrtype wave option
if "`option'"=="base" {
collapse response_`rrtype'_w`wave' if wave==12 & iw_hhtup==`sample'
rename response_`rrtype'_w`wave' base_`rrtype'
save "$workinglocation\Responserates\sample`sample'_rr`rrtype'_base_w`wave'", replace
use "$workinglocation\Responserates\responserates", clear
}
if "`option'"~="base" { 
args sample rrtype wave option
collapse response_`rrtype'_`option'_w`wave' if wave==12 & iw_hhtup==`sample'
rename response_`rrtype'_`option'_w`wave' `option'_`rrtype'
save "$workinglocation\Responserates\sample`sample'_rr`rrtype'_`option'_w`wave'", replace
use "$workinglocation\Responserates\responserates", clear
}
end

forvalues w=$startwave/$endwave {
  forvalues opt = 0/9 {
   if `opt'==0 {
     rrtablecells 0 bp `w' base
     rrtablecells 0 pw `w' base
     rrtablecells 1 bp `w' base
     rrtablecells 1 pw `w' base
   }
   else {
     rrtablecells 0 bp `w' opt`opt'
     rrtablecells 0 pw `w' opt`opt'
     rrtablecells 1 bp `w' opt`opt'
     rrtablecells 1 pw `w' opt`opt'
   }
  }
}  

* Assemble table - Main, Paired Wave

use "$workinglocation\Responserates\sample0_rrpw_base_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample0_rrpw_base_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample0_rrpw_base_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample0_rrpw_base_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample0_rrpw_base_w16.dta"
replace wave=16 if wave==.
order wave base_pw
save "$workinglocation\Responserates\pw_base.dta", replace

capture program drop finaltablepwmain
program define finaltablepwmain
args option
use "$workinglocation\Responserates\sample0_rrpw_`option'_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample0_rrpw_`option'_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample0_rrpw_`option'_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample0_rrpw_`option'_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample0_rrpw_`option'_w16.dta"
replace wave=16 if wave==.
order wave `option'_pw
save "$workinglocation\Responserates\pw_`option'.dta", replace
end

forvalues opt = 1/9 {
  finaltablepwmain opt`opt'
}

use "$workinglocation\Responserates\pw_base.dta", clear
merge 1:1 wave using "$workinglocation\Responserates\pw_opt1.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt2.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt3.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt4.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt5.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt6.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt7.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt8.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt9.dta"
drop _merge
save "$workinglocation\Responserates\pw_main.dta", replace

export excel "$workinglocation\Responserates\pw_main.xlsx", replace

* Assemble table - TopUp, Paired Wave

use "$workinglocation\Responserates\sample1_rrpw_base_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample1_rrpw_base_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample1_rrpw_base_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample1_rrpw_base_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample1_rrpw_base_w16.dta"
replace wave=16 if wave==.
order wave base_pw
save "$workinglocation\Responserates\pw_base.dta", replace

capture program drop finaltablepwtopup
program define finaltablepwtopup
args option
use "$workinglocation\Responserates\sample1_rrpw_`option'_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample1_rrpw_`option'_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample1_rrpw_`option'_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample1_rrpw_`option'_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample1_rrpw_`option'_w16.dta"
replace wave=16 if wave==.
order wave `option'_pw
save "$workinglocation\Responserates\pw_`option'.dta", replace
end

forvalues opt = 1/9 {
  finaltablepwtopup opt`opt'
}

use "$workinglocation\Responserates\pw_base.dta", clear
merge 1:1 wave using "$workinglocation\Responserates\pw_opt1.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt2.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt3.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt4.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt5.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt6.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt7.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt8.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\pw_opt9.dta"
drop _merge
save "$workinglocation\Responserates\pw_topup.dta", replace

export excel "$workinglocation\Responserates\pw_topup.xlsx", replace

* Assemble table - Main, Balanced Panel

use "$workinglocation\Responserates\sample0_rrbp_base_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample0_rrbp_base_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample0_rrbp_base_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample0_rrbp_base_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample0_rrbp_base_w16.dta"
replace wave=16 if wave==.
order wave base_bp
save "$workinglocation\Responserates\bp_base.dta", replace

capture program drop finaltablebpmain
program define finaltablebpmain
args option
use "$workinglocation\Responserates\sample0_rrbp_`option'_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample0_rrbp_`option'_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample0_rrbp_`option'_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample0_rrbp_`option'_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample0_rrbp_`option'_w16.dta"
replace wave=16 if wave==.
order wave `option'_bp
save "$workinglocation\Responserates\bp_`option'.dta", replace
end

forvalues opt = 1/9 {
  finaltablebpmain opt`opt'
}

use "$workinglocation\Responserates\bp_base.dta", clear
merge 1:1 wave using "$workinglocation\Responserates\bp_opt1.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt2.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt3.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt4.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt5.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt6.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt7.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt8.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt9.dta"
drop _merge
save "$workinglocation\Responserates\bp_main.dta", replace

export excel "$workinglocation\Responserates\bp_main.xlsx", replace

* Assemble table - TopUp, Balanced Panel

use "$workinglocation\Responserates\sample1_rrbp_base_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample1_rrbp_base_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample1_rrbp_base_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample1_rrbp_base_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample1_rrbp_base_w16.dta"
replace wave=16 if wave==.
order wave base_bp
save "$workinglocation\Responserates\bp_base.dta", replace

capture program drop finaltablebptopup
program define finaltablebptopup
args option
use "$workinglocation\Responserates\sample1_rrbp_`option'_w12.dta", clear
gen wave=12 
append using "$workinglocation\Responserates\sample1_rrbp_`option'_w13.dta"
replace wave=13 if wave==.
append using "$workinglocation\Responserates\sample1_rrbp_`option'_w14.dta"
replace wave=14 if wave==.
append using "$workinglocation\Responserates\sample1_rrbp_`option'_w15.dta"
replace wave=15 if wave==.
append using "$workinglocation\Responserates\sample1_rrbp_`option'_w16.dta"
replace wave=16 if wave==.
order wave `option'_bp
save "$workinglocation\Responserates\bp_`option'.dta", replace
end

forvalues opt = 1/9 {
  finaltablebptopup opt`opt'
}

use "$workinglocation\Responserates\bp_base.dta", clear
merge 1:1 wave using "$workinglocation\Responserates\bp_opt1.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt2.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt3.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt4.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt5.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt6.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt7.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt8.dta"
drop _merge
merge 1:1 wave using "$workinglocation\Responserates\bp_opt9.dta"
drop _merge
save "$workinglocation\Responserates\bp_topup.dta", replace

export excel "$workinglocation\Responserates\bp_topup.xlsx", replace

log close
