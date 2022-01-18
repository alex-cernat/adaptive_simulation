
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
log using "$logslocation\distribution of outcomes.log", replace

* Propensities dataset (wave 11 characteristics, waves 12-16)
use "$workinglocation\propensities_w11r_sim.dta", clear

* Tabulate outcomes for each wave by wave 11 sample type
gen outcome_P1=response_P1 // 0=non-resp, 1=resp, .=oos
replace outcome_P1=2 if response_P1==. // OOS
replace outcome_P1=3 if hhresp_master==100 //not issued this wave

gen outcome_P3=response_P3 // 0=non-resp, 1=resp, .=oos
replace outcome_P3=2 if response_P3==. // OOS
replace outcome_P3=3 if p23callcount==0 | p23callcount==.  //not issued to P2/3

bys iw_hhtup: tab outcome_P1 wave, miss
bys iw_hhtup: tab outcome_P3 wave if outcome_P1==0, miss

log close
