

*********************************************************************************************************************************************************************************
*********************************************************************************************************************************************************************************
*																																												*
* This program constructs response propensity models and R-Indicators for each of waves 12 to 16 of HILDA, using the respondents from wave 11.									* 																																							*
*********************************************************************************************************************************************************************************
*********************************************************************************************************************************************************************************



****************************************************** PART 1 - PREDICTOR VARIABLES AND PROGRAM SETTINGS ************************************************************************

* Run parameters
set more off
macro drop _all
*pause on

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

log using "$logslocation\preparedataset.log", replace


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



*********************** PART 2 - CONSTRUCT DATASET OF INITIAL WAVE RESPONDENTS AND THEIR CHARACTERISTICS AND CURRENT WAVE INTERVIEW OUTCOMES ***********************************

* Loop over each wave
forvalues wave = $startwave/$endwave {
local initialwave = $initialwave
local w  = substr("$alphabet",`wave',1)
local iw = substr("$alphabet",`initialwave',1)
display in red "*******************************************************************************************************************************************************************"
display in red "Constructing long dataset of respondents and initial wave information - adding wave `wave' response information and wave `initialwave' respondent characteristics"
display in red "*******************************************************************************************************************************************************************"

* Read in master file 
use xwaveid `w'hhpid enumptn ivwptn `w'hhresp `w'fstatus hhsm using "$hildalocation\master_$releasew$releaseu.dta", clear
rename `w'hhresp hhresp_master
rename `w'fstatus fstatus_master
rename `w'hhpid hhpid
sort hhpid

* Keep only initial wave respondents
drop if substr(ivwptn,`initialwave',1)~="X"
save "$workinglocation\master_w`wave'.dta", replace

* For now, also drop initial wave respondents who are not issued to field in the current wave. Save them to a separate dataset.
keep if hhresp_master>=100
gen issued=0
save "$workinglocation\master_w`wave'_notissued.dta", replace
use "$workinglocation\master_w`wave'.dta", clear
drop if hhresp_master>=100
gen issued=1
save "$workinglocation\master_w`wave'.dta", replace

* Read in current wave household file (since the u household file contains all current wave non-respondents, not just those in responding households)
use `w'hhid `w'hhrespi `w'hhrespf `w'hhresp `w'hgri1-`w'hgri20 `w'hgrf1-`w'hgrf20 `w'hgivw1-`w'hgivw20 `w'hhcomps `w'hhcompi `w'hhcompf `w'hhivws `w'hhcalls ///
	using "$hildalocation\household_`w'$releaseu.dta", clear
rename `w'* *

* Convert person variables to long format (variable pers is created)
reshape long hgri hgrf hgivw, i(hhid) j(pers)

* Construct person ID to prepare for merging on information from the master file 
gen hhpid=hhid+"0"+string(pers) if pers< 10
replace hhpid=hhid+string(pers) if pers>=10
sort hhpid

* Merge on the master file by hhpid 
merge 1:1 hhpid using "$workinglocation\master_w`wave'.dta"
tab _merge

* Remove records which are not on the master file (eg, remove all initial wave non-respondents)
drop if _merge == 1

* Remove people who are only on the master file and who were in households which were not issued to field in the current wave, AND who were out of scope [final condition added 28/5/18]
drop if _merge == 2 & hhresp_master>=100 & substr(ivwptn,`wave',1)=="-"
drop _merge

sort hhpid
save "$workinglocation\propensities_w`wave'.dta", replace

* Attach the interview completion date from the responding person file  
use `w'hhpid `w'hhidate using "$hildalocation\rperson_`w'$releaseu.dta", clear
rename `w'* *
sort hhpid
save "$workinglocation\rperson_w`wave'.dta", replace
use "$workinglocation\propensities_w`wave'.dta", clear
merge 1:1 hhpid using "$workinglocation\rperson_w`wave'.dta"
tab _merge
drop if _merge==2
drop _merge
save "$workinglocation\propensities_w`wave'.dta", replace

* Append the wave 11 respondents not issued to field in the current wave
append using "$workinglocation\master_w`wave'_notissued.dta"
sort hhpid
save "$workinglocation\propensities_w`wave'.dta", replace

* Attach respondent characteristics from initial wave to model response propensities in current wave
use xwaveid `iw'hhtype `iw'hhmove `iw'mhnyr `iw'hhpers `iw'hhrih `iw'hgage `iw'hgsex `iw'hhstate `iw'hhsad10 `iw'hstenr `iw'hhpq `iw'jbhruc `iw'mrcurr `iw'hifapti ///
			`iw'tcr `iw'edfts `iw'edhigh1 `iw'esbrd `iw'esdtl `iw'wsfei `iw'wsfe `iw'helth `iw'helthwk `iw'helthdg `iw'gh1 `iw'losat `iw'hhold `iw'hifditp `iw'hifditn ///
			`iw'aneab `iw'anlote `iw'hhtup `iw'losatyh `iw'anbcob `iw'hhpers `iw'hhadult `iw'hhra `iw'edfts `iw'edhists `iw'edagels `iw'edq600 `iw'edq611 `iw'hhssos ///
			using "$hildalocation\combined_`iw'$releaseu.dta", clear
rename `iw'* iw_*
save "$workinglocation\combined_w`initialwave'.dta", replace
use "$workinglocation\propensities_w`wave'.dta", clear
merge 1:1 xwaveid using "$workinglocation\combined_w`initialwave'.dta"
drop if _merge==2
drop _merge
save "$workinglocation\propensities_w`wave'.dta", replace

* Construct numeric date variables as the number of days since 1/1/1960
gen hhidate_days = date(hhidate,"DMY")
gen hhcomps_days = date(hhcomps,"DMY")
gen hhcompi_days = date(hhcompi,"DMY")
gen hhcompf_days = date(hhcompf,"DMY")

* Construct interview and enumeration statuses
gen ivwptn_cw = substr(ivwptn,`wave',1)
gen enumptn_cw = substr(enumptn,`wave',1)

* Construct count of number of interviews up to and including the initial wave
forvalues countwave=1/`initialwave' {
	gen intcount_w`countwave'=1 if substr(ivwptn,`countwave',1)=="X"
	replace intcount_w`countwave'=0 if substr(ivwptn,`countwave',1)~="X"
}
gen numberivws = intcount_w1
forvalues countwave=2/`initialwave' {
	replace numberivws=numberivws+intcount_w`countwave'
}
drop intcount_w*

* Calculate days in field for respondents
local p1start = "w"+"`wave'"+"_p1start"
local p1end   = "w"+"`wave'"+"_p1end"
local p2start = "w"+"`wave'"+"_p2start"
local p2end   = "w"+"`wave'"+"_p2end"
local p3start = "w"+"`wave'"+"_p3start"
local p3end   = "w"+"`wave'"+"_p3end"
gen p1_days=td($`p1end')-td($`p1start')+1
gen p2_days=td($`p2end')-td($`p2start')+1
gen p3_days=td($`p3end')-td($`p3start')+1
gen total_days_field = p1_days + p2_days + p3_days

* Definition of days in field.
* If interviewed before P1, shift to day 1. If interviewed between P1 and P2, shift to end of P1. If interviewed between P2 and P3, shift to end of P2. If interviewed after end P3, shift to end of P3.
gen daysfield = .
replace daysfield = hhidate_days-td($`p1start')+1 if hhidate_days-td($`p1end')<=0 & ivwptn_cw == "X" 
replace daysfield = td($`p1end')-td($`p1start')+1 if hhidate_days-td($`p1end')>0 & hhidate_days-td($`p2start')<0 & ivwptn_cw == "X" 
replace daysfield = (td($`p1end')-td($`p1start')+1) + (hhidate_days-td($`p2start')+1) if hhidate_days-td($`p2start')>=0 & hhidate_days-td($`p2end')<=0 & ivwptn_cw == "X" 
replace daysfield = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) if hhidate_days-td($`p2end')>0 & hhidate_days-td($`p3start')<0 & ivwptn_cw == "X" 
replace daysfield = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (hhidate_days-td($`p3start')+1) if hhidate_days-td($`p3start')>=0 & hhidate_days-td($`p3end')<=0 & ivwptn_cw == "X" 
replace daysfield = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (td($`p3end')-td($`p3start')+1) if hhidate_days-td($`p3end')>0 & ivwptn_cw == "X" 

display in red "Check distribution of days in field by wave `wave' outcome"
bys ivwptn_cw: tab daysfield, miss
display in red "Check number of respondents with interview dates after the official end of P3 fieldwork"
tab hhidate if ( daysfield > total_days_field & ivwptn_cw == "X" )
list daysfield if ( daysfield > total_days_field & ivwptn_cw == "X" )
* 26 respondents in wave 12 have more than 149 days in field (ranging from 150 to 164 days)
* 11 respondents in wave 13 have more than 149 days in field (ranging from 150 to 154 days)
* 7  respondents in wave 14 have more than 149 days in field (ranging from 150 to 154 days)
* 26 respondents in wave 15 have more than 149 days in field (ranging from 150 to 165 days)
* 33 respondents in wave 16 have more than 149 days in field (ranging from 150 to 156 days)

* Calculate weeks in field for respondents
gen weeksfield = .
replace weeksfield = ceil(daysfield/7) if daysfield ~= .
replace weeksfield = 21 if weeksfield == 22 /* Largest possible days in field is 149, and 149/7=21.3, so move respondents interviewed on days 148 and 149 from week22 to week21*/
display in red "Check distribution of weeks in field by wave `wave' outcome"
bys ivwptn_cw: tab weeksfield, miss

* For non-responding households (and for non-respondents in responding households), calculate days taken to establish structure of household
gen daysstructure = .
replace daysstructure = hhcomps_days-td($`p1start')+1 if hhcomps_days-td($`p1end')<=0 & (hhresp >= 69 | (hhresp >= 62 & hhresp >= 68 & (ivwptn_cw=="O" | ivwptn_cw=="-")))
replace daysstructure = td($`p1end')-td($`p1start')+1 if hhcomps_days-td($`p1end')>0 & hhcomps_days-td($`p2start')<0 & (hhresp >= 69 | (hhresp >= 62 & hhresp >= 68 & (ivwptn_cw=="O" | ivwptn_cw=="-")))
replace daysstructure = (td($`p1end')-td($`p1start')+1) + (hhcomps_days-td($`p2start')+1) if hhcomps_days-td($`p2start')>=0 & hhcomps_days-td($`p2end')<=0 & (hhresp >= 69 | (hhresp >= 62 & hhresp >= 68 & (ivwptn_cw=="O" | ivwptn_cw=="-")))
replace daysstructure = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) if hhcomps_days-td($`p2end')>0 & hhcomps_days-td($`p3start')<0 & (hhresp >= 69 | (hhresp >= 62 & hhresp >= 68 & (ivwptn_cw=="O" | ivwptn_cw=="-")))
replace daysstructure = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (hhcomps_days-td($`p3start')+1) if hhcomps_days-td($`p3start')>=0 & hhcomps_days-td($`p3end')<=0 & (hhresp >= 69 | (hhresp >= 62 & hhresp >= 68 & (ivwptn_cw=="O" | ivwptn_cw=="-")))
replace daysstructure = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (td($`p3end')-td($`p3start')+1) if hhcomps_days-td($`p3end')>0 & (hhresp >= 69 | (hhresp >= 62 & hhresp >= 68 & (ivwptn_cw=="O" | ivwptn_cw=="-")))
display in red "Check distribution of daysstructure"
bys ivwptn_cw: tab daysstructure, miss

* Wave 12: 4 cases. Each have hhresp of 99 (untraceable this wave) and hhcomps 29/1/2012 (long before the beginning of P1 in July 2012). Set tracking indicator to 0 for all days
* of fieldwork using  manual edits in Part 4 below. Set daysstructure to missing.
* Wave 13: 1 case. Error since structure date cannot be before P1 (as above), but for this case hgri indicates they are overseas. Set to out of scope after day 1 of fieldwork.
* Wave 14: A large number of cases (70) have daysstructure=0, set these to 1
* Wave 15: 17 cases have negative daysstructure (ranging from -4 to -53), set these to 1
* Wave 16: 17 cases have daysstructure=0, set these to 1
if `wave'==12 {
replace daysstructure = . if inlist(xwaveid,"1100688","1100945","1100946","1100947")
}
if `wave'==13 {
replace daysstructure = 1 if daysstructure==-7&xwaveid=="0109155"
}
if `wave'==14 {
replace daysstructure = 1 if daysstructure == 0
}
if `wave'==15 {
replace daysstructure = 1 if daysstructure < 0
}
if `wave'==16 {
replace daysstructure = 1 if daysstructure == 0
}

* Calculate days taken to record the initial stage fieldwork outcome
gen daysISdate = .
replace daysISdate = hhcompi_days-td($`p1start')+1 if hhcompi_days-td($`p1end')<=0 
replace daysISdate = td($`p1end')-td($`p1start')+1 if hhcompi_days-td($`p1end')>0 & hhcompi_days-td($`p2start')<0 
replace daysISdate = (td($`p1end')-td($`p1start')+1) + (hhcompi_days-td($`p2start')+1) if hhcompi_days-td($`p2start')>=0 & hhcompi_days-td($`p2end')<=0 
replace daysISdate = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) if hhcompi_days-td($`p2end')>0 & hhcompi_days-td($`p3start')<0 
replace daysISdate = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (hhcompi_days-td($`p3start')+1) if hhcompi_days-td($`p3start')>=0 & hhcompi_days-td($`p3end')<=0 
replace daysISdate = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (td($`p3end')-td($`p3start')+1) if hhcompi_days-td($`p3end')>0 
display in red "Check distribution of daysISdate"
bys ivwptn_cw: tab daysISdate, miss

if `wave'==12 {
replace daysISdate = . if inlist(xwaveid,"1100688","1100945","1100946","1100947")
}
if `wave'==13 {
replace daysISdate = 1 if daysISdate==-203&xwaveid=="0100823"
replace daysISdate = 1 if daysISdate==-7&xwaveid=="0109155"
}
if `wave'==14 {
replace daysISdate = 1 if daysISdate == 0
}
if `wave'==15 {
replace daysISdate = 1 if daysISdate < 0
}
if `wave'==16 {
replace daysISdate = 1 if daysISdate < 0
}

* Calculate days taken to record the final stage fieldwork outcome
gen daysFSdate = .
replace daysFSdate = hhcompf_days-td($`p1start')+1 if hhcompf_days-td($`p1end')<=0 
replace daysFSdate = td($`p1end')-td($`p1start')+1 if hhcompf_days-td($`p1end')>0 & hhcompf_days-td($`p2start')<0 
replace daysFSdate = (td($`p1end')-td($`p1start')+1) + (hhcompf_days-td($`p2start')+1) if hhcompf_days-td($`p2start')>=0 & hhcompf_days-td($`p2end')<=0 
replace daysFSdate = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) if hhcompf_days-td($`p2end')>0 & hhcompf_days-td($`p3start')<0 
replace daysFSdate = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (hhcompf_days-td($`p3start')+1) if hhcompf_days-td($`p3start')>=0 & hhcompf_days-td($`p3end')<=0 
replace daysFSdate = (td($`p1end')-td($`p1start')+1) + (td($`p2end')-td($`p2start')+1) + (td($`p3end')-td($`p3start')+1) if hhcompf_days-td($`p3end')>0 
display in red "Check distribution of daysFSdate"
bys ivwptn_cw: tab daysFSdate, miss

* Check for cases with non-missing hhcompf but were found to be out of scope in the initial fieldwork period
display in red "Check for cases with non-missing hhcompf but were found to be out of scope in the initial fieldwork period"
tab hhcompf if hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27
* 44 cases in wave 12 with non-missing hhcompf dates but out of scope in initial fieldwork period
if `wave'==12 {
list xwaveid hhresp ivwptn_cw hhcompi hhcompf hgri hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27), nolabel
tab hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27)
* All 44 cases in w12 have out of scope codes for hgrf, so no cleans required
}
if `wave'==13 {
list xwaveid hhresp ivwptn_cw hhcompi hhcompf hgri hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27), nolabel
tab hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27)
* All 38 cases in w13 cases have out of scope codes for hgrf, so no cleans required
}
if `wave'==14 {
list xwaveid hhresp ivwptn_cw hhcompi hhcompf hgri hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27), nolabel
tab hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27)
* All 28 cases in w14 cases have out of scope codes for hgrf, so no cleans required
}
if `wave'==15 {
list xwaveid hhresp ivwptn_cw hhcompi hhcompf hgri hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27), nolabel
tab hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27)
* All 34 cases in w15 cases have out of scope codes for hgrf, so no cleans required
}
if `wave'==16 {
list xwaveid hhresp ivwptn_cw hhcompi hhcompf hgri hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27), nolabel
tab hgrf if hhcompf~="-1"&hhcompf~="-9"&(hgri==3|hgri==4|hgri==6|hgri==12|hgri==13|hgri==28|hgri==21|hgri==27)
* All 20 cases in w16 cases have out of scope codes for hgrf, so no cleans required
}

* Additional wave specific cleans
if `wave'==16 {
	* Clean specific to wave 16: One case has phhcompi=phhcomps=26/8/17, assume this is incorrectly coded and should be 26/8/16
	replace daysISdate = (td(26/08/2016)-td(26/07/2016)+1) if daysISdate==351
}

* Create long file
gen wave = `wave'
if `wave'==$startwave {
save "$workinglocation\propensities_wave11respondents", replace
}
if `wave'~=$startwave {
append using "$workinglocation\propensities_wave11respondents"
save "$workinglocation\propensities_wave11respondents", replace
}

}

*********************************************************************************************************************************************************************************


*********************************************** PART 3 - CONSTRUCT DAILY AND WEEKLY FIELDWORK TRACKING INDICATOR ****************************************************************

display in red "*******************************************************************************************"
display in red "Constructing daily and weekly fieldwork tracking indicators and response outcome indicators"
display in red "*******************************************************************************************"

* Define field tracking indicator as:
	* 0=Presumed in-scope, non-response as at this day
	* 1=Interviewed
	* 2=Confirmed nonresponse (NW - covers all kinds of non-response not just refused hgri=7-11)
	* 3=Out of scope
	
quietly sum daysfield
*local maxdaysfield = r(max) 
/* Largest possible days in field is 69+48+32 = 149 */
forvalues day = 1/149 {
	/* All initial wave respondents who were issued to field in the current wave start as in-scope */
	gen tracking_day`day'=0	if issued==1
	/* Initial wave respondents who were not issued to field in the current wave and who are out of scope */
	replace tracking_day`day'=3 if issued==0 & ivwptn_cw=="-"
	/* Initial wave respondents who were not issued to field in the current wave and who are counted as nonrespondents */
	replace tracking_day`day'=2 if issued==0 & ivwptn_cw=="O"
	/* Set w16 respondents to interviewed from the day following their interview date and onward */
	replace tracking_day`day'=1 if (`day'>daysfield) & (ivwptn_cw=="X")
	/* Set out of scopes based on hh structure date and IS/FS dates */
		/* Found out of scope in initial fieldwork stage */
		replace tracking_day`day'=3 if inlist(hgri,3,4,6,12,13,28,21,27) & `day'>=daysISdate  // NW changed > to >=
		/* Found out of scope in follow-up fieldwork stage */
		replace tracking_day`day'=3 if inlist(hgrf,3,4,6,12,13,28,21,27) & `day'>=daysFSdate  // NW changed > to >=
		/* Nonresponse in initial fieldwork period, then found out of scope in follow-up period - NW revised codes 1/7/19 - MOS had 7,8,9,10,11, NW changed this to 5,7,8,9,10,11,14,15,16,17,18,19  */
		replace tracking_day`day'=3 if inlist(hgri,5,7,8,9,10,11,14,15,16,17,18,19) & inlist(hgrf,3,4,6,12,13,28,21,27) & `day'>daysISdate & `day'<=daysFSdate
		replace tracking_day`day'=3 if inlist(hgri,5,7,8,9,10,11,14,15,16,17,18,19) & inlist(hgrf,3,4,6,12,13,28,21,27) & `day'>daysFSdate	
		/* out of scope in final status but does not have this code at IS or FS or is not recorded in a HF */
		replace tracking_day`day'=3 if tracking_day`day'==0 & inlist(fstatus_master,3,4,6,12,13,28,21,27) & `day'>=daysstructure	
	/* Nonresponse in initial fieldwork period, then interviewed in follow-up period */
	replace tracking_day`day'=2 if tracking_day`day'==0 & inlist(hgri,5,7,8,9,10,11,14,15,16,17,18,19) & inlist(hgrf,1,2) & `day'>daysISdate & `day'<=daysFSdate
	/* Nonresponse in both initial and follow-up periods */
	replace tracking_day`day'=2 if tracking_day`day'==0 & inlist(hgri,5,7,8,9,10,11,14,15,16,17,18,19) & inlist(hgrf,5,7,8,9,10,11,14,15,16,17,18,19) & `day'>daysISdate 
	/* Nonresponse in initial fieldwork period, then unable to interview in follow-up period - NW this no longer applies given change made to overall codes */
	replace tracking_day`day'=2 if tracking_day`day'==0 & inlist(hgri,5,7,8,9,10,11,14,15,16,17,18,19) & inlist(hgrf,14,15,16,17,18,19) & `day'>daysISdate 
	/* Nonresponse in initial fieldwork period, then either not asked or non-responding household in follow-up period */
	replace tracking_day`day'=2 if tracking_day`day'==0 & inlist(hgri,5,7,8,9,10,11,14,15,16,17,18,19) & inlist(hgrf,-1,-9) & `day'>daysISdate 
	/* Nonresponse in final fieldwork period */
	replace tracking_day`day'=2 if tracking_day`day'==0 & inlist(hgrf,5,7,8,9,10,11,14,15,16,17,18,19) & `day'>daysFSdate 
		
}

* Weekly tracking indicator (for decomposition of R indicators)
gen tracking_week1 = tracking_day7
gen tracking_week2 = tracking_day14
gen tracking_week3 = tracking_day21
gen tracking_week4 = tracking_day28
gen tracking_week5 = tracking_day35
gen tracking_week6 = tracking_day42
gen tracking_week7 = tracking_day49
gen tracking_week8 = tracking_day56
gen tracking_week9 = tracking_day63
gen tracking_week10 = tracking_day70
gen tracking_week11 = tracking_day77
gen tracking_week12 = tracking_day84
gen tracking_week13 = tracking_day91
gen tracking_week14 = tracking_day98
gen tracking_week15 = tracking_day105
gen tracking_week16 = tracking_day112
gen tracking_week17 = tracking_day119
gen tracking_week18 = tracking_day126
gen tracking_week19 = tracking_day133
gen tracking_week20 = tracking_day140
gen tracking_week21 = tracking_day149

	
* Construct response indicators as at the end of each of the three fieldwork periods, for use as the dependent variables in the response propensity regressions
gen response_P1 = .
replace response_P1 = 1 if tracking_day69==1
replace response_P1 = 0 if inlist(tracking_day69,0,2)
gen response_P2 = .
replace response_P2 = 1 if tracking_day117==1
replace response_P2 = 0 if inlist(tracking_day117,0,2)
gen response_P3 = .
replace response_P3 = 1 if tracking_day149==1
replace response_P3 = 0 if inlist(tracking_day149,0,2)
	
* Save dataset ;
save "$workinglocation\propensities_wave11respondents.dta", replace

*********************************************************************************************************************************************************************************


*********************************************** PART 3B - TABLES OF INTERVIEW COMPLETIONS BY WEEK AND PERIOD (ADDED 23/7/2018) **************************************************
forvalues week = 1/21 {
	use "$workinglocation\propensities_wave11respondents.dta", clear
	gen personcounter=1
	collapse (sum) personcounter if tracking_week`week'==1, by(wave) 
	rename personcounter ivw_by_w`week'
	save "$workinglocation\ivwbyw`week'.dta", replace	
}
forvalues week = 1/21 {
	if `week'==1 {
	use "$workinglocation\ivwbyw`week'.dta", clear
	save "$workinglocation\ivwsbyweek.dta", replace
	}
	if `week'~=1 {
	use "$workinglocation\ivwsbyweek.dta", clear
	merge 1:1 wave using "$workinglocation\ivwbyw`week'.dta"
	drop _merge
	save "$workinglocation\ivwsbyweek.dta", replace
	}
}
forvalues week = 1/21 {
	if `week'==1 {
		gen ivw_in_w`week'=ivw_by_w`week'
	}
	if `week'~=1 {
		local prevweek = `week'-1
		gen ivw_in_w`week'=ivw_by_w`week'-ivw_by_w`prevweek'
	}
}
drop ivw_by*
gen total_ivw = ivw_in_w1+ivw_in_w2+ivw_in_w3+ivw_in_w4+ivw_in_w5+ivw_in_w6+ivw_in_w7+ivw_in_w8+ivw_in_w9+ivw_in_w10+ivw_in_w11+ivw_in_w12+ivw_in_w13+ivw_in_w14+ivw_in_w15+ ///
				ivw_in_w16+ivw_in_w17+ivw_in_w18+ivw_in_w19+ivw_in_w20+ivw_in_w21
export  excel "$resultslocation\interviews_w11respondents.xlsx" , ///
		sheet("By week - Overall Sample") sheetmodify firstrow(variables) cell(A1) 

forvalues week = 1/21 {
	use "$workinglocation\propensities_wave11respondents.dta", clear
	gen personcounter=1
	collapse (sum) personcounter if tracking_week`week'==1 & iw_hhtup==0 , by(wave) 
	rename personcounter ivw_by_w`week'
	save "$workinglocation\ivwbyw`week'.dta", replace	
}
forvalues week = 1/21 {
	if `week'==1 {
	use "$workinglocation\ivwbyw`week'.dta", clear
	save "$workinglocation\ivwsbyweek.dta", replace
	}
	if `week'~=1 {
	use "$workinglocation\ivwsbyweek.dta", clear
	merge 1:1 wave using "$workinglocation\ivwbyw`week'.dta"
	drop _merge
	save "$workinglocation\ivwsbyweek.dta", replace
	}
}
forvalues week = 1/21 {
	if `week'==1 {
		gen ivw_in_w`week'=ivw_by_w`week'
	}
	if `week'~=1 {
		local prevweek = `week'-1
		gen ivw_in_w`week'=ivw_by_w`week'-ivw_by_w`prevweek'
	}
}
drop ivw_by*
gen total_ivw = ivw_in_w1+ivw_in_w2+ivw_in_w3+ivw_in_w4+ivw_in_w5+ivw_in_w6+ivw_in_w7+ivw_in_w8+ivw_in_w9+ivw_in_w10+ivw_in_w11+ivw_in_w12+ivw_in_w13+ivw_in_w14+ivw_in_w15+ ///
				ivw_in_w16+ivw_in_w17+ivw_in_w18+ivw_in_w19+ivw_in_w20+ivw_in_w21
export  excel "$resultslocation\interviews_w11respondents.xlsx" , ///
		sheet("By week - Main Sample") sheetmodify firstrow(variables) cell(A1)
		
forvalues week = 1/21 {
	use "$workinglocation\propensities_wave11respondents.dta", clear
	gen personcounter=1
	collapse (sum) personcounter if tracking_week`week'==1 & iw_hhtup==1 , by(wave) 
	rename personcounter ivw_by_w`week'
	save "$workinglocation\ivwbyw`week'.dta", replace	
}
forvalues week = 1/21 {
	if `week'==1 {
	use "$workinglocation\ivwbyw`week'.dta", clear
	save "$workinglocation\ivwsbyweek.dta", replace
	}
	if `week'~=1 {
	use "$workinglocation\ivwsbyweek.dta", clear
	merge 1:1 wave using "$workinglocation\ivwbyw`week'.dta"
	drop _merge
	save "$workinglocation\ivwsbyweek.dta", replace
	}
}
forvalues week = 1/21 {
	if `week'==1 {
		gen ivw_in_w`week'=ivw_by_w`week'
	}
	if `week'~=1 {
		local prevweek = `week'-1
		gen ivw_in_w`week'=ivw_by_w`week'-ivw_by_w`prevweek'
	}
}
drop ivw_by*
gen total_ivw = ivw_in_w1+ivw_in_w2+ivw_in_w3+ivw_in_w4+ivw_in_w5+ivw_in_w6+ivw_in_w7+ivw_in_w8+ivw_in_w9+ivw_in_w10+ivw_in_w11+ivw_in_w12+ivw_in_w13+ivw_in_w14+ivw_in_w15+ ///
				ivw_in_w16+ivw_in_w17+ivw_in_w18+ivw_in_w19+ivw_in_w20+ivw_in_w21
export  excel "$resultslocation\interviews_w11respondents.xlsx" , ///
		sheet("By week - TopUp Sample") sheetmodify firstrow(variables) cell(A1)



forvalues period = 1/3 {
	use "$workinglocation\propensities_wave11respondents.dta", clear
	gen personcounter=1
	collapse (sum) personcounter if response_P`period'==1, by(wave) 
	rename personcounter ivw_by_p`period'
	save "$workinglocation\ivwbyp`period'.dta", replace	
}
forvalues period = 1/3 {
	if `period'==1 {
	use "$workinglocation\ivwbyp`period'.dta", clear
	save "$workinglocation\ivwsbyperiod.dta", replace
	}
	if `period'~=1 {
	use "$workinglocation\ivwsbyperiod.dta", clear
	merge 1:1 wave using "$workinglocation\ivwbyp`period'.dta"
	drop _merge
	save "$workinglocation\ivwsbyperiod.dta", replace
	}
}
forvalues period = 1/3 {
	if `period'==1 {
		gen ivw_in_p`period'=ivw_by_p`period'
	}
	if `period'~=1 {
		local prevperiod = `period'-1
		gen ivw_in_p`period'=ivw_by_p`period'-ivw_by_p`prevperiod'
	}
}
drop ivw_by*
gen total_ivw = ivw_in_p1 + ivw_in_p2 + ivw_in_p3
export  excel "$resultslocation\interviews_w11respondents.xlsx" , ///
		sheet("By period - Overall Sample") sheetmodify firstrow(variables) cell(A1) 		
		
forvalues period = 1/3 {
	use "$workinglocation\propensities_wave11respondents.dta", clear
	gen personcounter=1
	collapse (sum) personcounter if response_P`period'==1 & iw_hhtup==0, by(wave) 
	rename personcounter ivw_by_p`period'
	save "$workinglocation\ivwbyp`period'.dta", replace	
}
forvalues period = 1/3 {
	if `period'==1 {
	use "$workinglocation\ivwbyp`period'.dta", clear
	save "$workinglocation\ivwsbyperiod.dta", replace
	}
	if `period'~=1 {
	use "$workinglocation\ivwsbyperiod.dta", clear
	merge 1:1 wave using "$workinglocation\ivwbyp`period'.dta"
	drop _merge
	save "$workinglocation\ivwsbyperiod.dta", replace
	}
}
forvalues period = 1/3 {
	if `period'==1 {
		gen ivw_in_p`period'=ivw_by_p`period'
	}
	if `period'~=1 {
		local prevperiod = `period'-1
		gen ivw_in_p`period'=ivw_by_p`period'-ivw_by_p`prevperiod'
	}
}
drop ivw_by*
gen total_ivw = ivw_in_p1 + ivw_in_p2 + ivw_in_p3
export  excel "$resultslocation\interviews_w11respondents.xlsx" , ///
		sheet("By period - Main Sample") sheetmodify firstrow(variables) cell(A1) 		

forvalues period = 1/3 {
	use "$workinglocation\propensities_wave11respondents.dta", clear
	gen personcounter=1
	collapse (sum) personcounter if response_P`period'==1 & iw_hhtup==1, by(wave) 
	rename personcounter ivw_by_p`period'
	save "$workinglocation\ivwbyp`period'.dta", replace	
}
forvalues period = 1/3 {
	if `period'==1 {
	use "$workinglocation\ivwbyp`period'.dta", clear
	save "$workinglocation\ivwsbyperiod.dta", replace
	}
	if `period'~=1 {
	use "$workinglocation\ivwsbyperiod.dta", clear
	merge 1:1 wave using "$workinglocation\ivwbyp`period'.dta"
	drop _merge
	save "$workinglocation\ivwsbyperiod.dta", replace
	}
}
forvalues period = 1/3 {
	if `period'==1 {
		gen ivw_in_p`period'=ivw_by_p`period'
	}
	if `period'~=1 {
		local prevperiod = `period'-1
		gen ivw_in_p`period'=ivw_by_p`period'-ivw_by_p`prevperiod'
	}
}
drop ivw_by*
gen total_ivw = ivw_in_p1 + ivw_in_p2 + ivw_in_p3
export  excel "$resultslocation\interviews_w11respondents.xlsx" , ///
		sheet("By period - TopUp Sample") sheetmodify firstrow(variables) cell(A1) 

*********************************************************************************************************************************************************************************
		
		
************************************************************ PART 4 - CONSTRUCT PREDICTOR VARIABLES *****************************************************************************

use "$workinglocation\propensities_wave11respondents.dta", clear

* agerange

gen agerange = .
replace agerange = 1 if iw_hgage <= 19
replace agerange = 2 if iw_hgage >= 20 & iw_hgage <= 24
replace agerange = 3 if iw_hgage >= 25 & iw_hgage <= 34
replace agerange = 4 if iw_hgage >= 35 & iw_hgage <= 44
replace agerange = 5 if iw_hgage >= 45 & iw_hgage <= 54
replace agerange = 6 if iw_hgage >= 55 & iw_hgage <= 64
replace agerange = 7 if iw_hgage >= 65 & iw_hgage <= 74
replace agerange = 8 if iw_hgage >= 75

label define agerange_label	1 "15 to 19 years" ///
							2 "20 to 24 years" ///
							3 "25 to 34 years" ///
							4 "35 to 44 years"  ///
							5 "45 to 54 years" ///
							6 "55 to 64 years"  ///
							7 "65 to 74 years"  ///
							8 "75 years or over"

							
* employment

gen employment = .
replace employment = 0 if iw_esbrd==3
replace employment = 1 if iw_esbrd==2
replace employment = 2 if iw_esdtl==1 
replace employment = 3 if iw_esdtl==2
* 25 cases missing hours worked so cannot split between FT and PT work

label define employment_label	0 "Not in the labour force" ///
								1 "Unemployed" ///
								2 "Employed full-time" ///
								3 "Employed part-time" 

								
* owner

gen owner = .
replace owner = 1 if inlist(iw_hstenr,1,3)
replace owner = 0 if inlist(iw_hstenr,/*-4,-3,*/2,4) /* For neatness of R indicator, assume missings are renting - NW removed 12/12/19 */

label define owner_label 	0 "Renter" ///
							1 "Owner"

							
* sectionofstate

gen sectionofstate = .
replace sectionofstate = 1 if iw_hhssos == 0
replace sectionofstate = 2 if iw_hhssos == 1
replace sectionofstate = 3 if inlist(iw_hhssos,2,3,7,9/*,-7*/) /* NWmissing codes removed 12/12/19 */

label define sectionofstate_label	1 "Major Urban (Pop 100,000+)" ///
									2 "Other Urban (Pop 1,000-99,999)" ///
									3 "Bounded Locality, Rural Balance, Migratory, or no usual address"
									

						
* relationship

gen relationship = .
replace relationship = 1 if iw_mrcurr==1						/* Married */
replace relationship = 2 if iw_mrcurr==2 						/* De facto */
replace relationship = 3 if inlist(iw_mrcurr,3,4,5)				/* Separated, Divorced or Widowed */
replace relationship = 4 if inlist(iw_mrcurr,/*-4,-3,*/6)			/* Never married and not de facto (Assume the 2 wave 15 respondents who refused to indicate their marital status - NW missing removed 12/12/19 ///
															    have never been married and are not de facto) */
															   
label define relationship_label		1 "Married" ///
									2 "De facto" ///
									3 "Separated, Divorced or Widowed" ///
									4 "Never married and not de facto"



* likelymove

gen likelymove = .
replace likelymove = 0 if inlist(iw_mhnyr,/*-4,*/3,4,5) /* NW missing value removed 12/12/19 */
replace likelymove = 1 if inlist(iw_mhnyr,1,2)

label define likelymove_label	0 "Not sure or unlikely to move in next 12 months" ///
								1 "Very likely or likely to move in next 12 months"

								
* healthcondition

gen healthcondition = .
replace healthcondition = 0 if iw_helth==2
replace healthcondition = 1 if iw_helth==1 & ( iw_helthwk==2 | ( iw_helthwk==1 & iw_helthdg>=0 & iw_helthdg<=5 ) )
replace healthcondition = 2 if iw_helth==1 & ( (iw_helthwk==1 & iw_helthdg>=6 & iw_helthdg<=10) | iw_helthwk==3 )

label define healthcondition_label		0 "Does not have a long term health condition" ///
										1 "Has a long term health condition with either no or low-med impact on work" ///
										2 "Has a long term health condition with a high impact on work" 

										
* countryofbirth

gen countryofbirth = .
replace countryofbirth = 1 if iw_anbcob==1			/* Australia */
replace countryofbirth = 2 if iw_anbcob==2			/* Main English speaking */
replace countryofbirth = 3 if inlist(iw_anbcob,/*-4,*/3)	/* Other (including refused/not stated) */

label define countryofbirth_label	1 "Australia" ///
									2 "Main English speaking" ///
									3 "Other"

									
* education 

gen education = .
replace education = 1 if inlist(iw_edhigh1,1,2,3)
replace education = 2 if inlist(iw_edhigh1,4)
replace education = 3 if inlist(iw_edhigh1,8)
replace education = 5 if inlist(iw_edhigh1,5)
replace education = 4 if education==. & inlist(iw_edhigh1,9) & ( (iw_edagels==2&(iw_edhists==1|iw_edhists==2)) | (iw_edagels>2&iw_edagels~=.&inlist(iw_edhists,2,3)))
replace education = 6 if education==. & ( inlist(iw_edhigh1,10) | (iw_edagels==2&iw_edhists>=3&iw_edhists<=9) | (iw_edagels==1) | (iw_edagels>2&iw_edagels~=.&inlist(iw_edhists,4,5,6,7,8)) ///
							| ( iw_edhigh1==9 & inlist(iw_edhists,-3,-4,-7))	/* 34 cases across waves 11-15 */ ///
							| ( iw_edhigh1==9 & iw_edhists==-1)	)				/* 2 cases across waves 11-15 */
							
label define education_label	1 "Degree (incl. postgrad)" ///
								2 "Other higher education" ///
								3 "Year 12 (equiv. to UK A level)"	///
								4 "Year 10 (equiv. to UK GCSE)"	///
								5 "Other qualification" ///
								6 "No qualification"


* sex

gen sex = iw_hgsex

label define sex_label			1 "Male" ///
								2 "Female"
								

* childreninhh

gen iw_hhchild=iw_hhpers-iw_hhadult
gen childreninhh = .
replace childreninhh = 0 if iw_hhchild==0
replace childreninhh = 1 if iw_hhchild==1
replace childreninhh = 2 if iw_hhchild>=2

label define childreninhh_label	0 "No children in household" ///
								1 "1 child in household" ///
								2 "2 or more children in household"

								
* adultsinhh

gen adultsinhh = .
replace adultsinhh = iw_hhadult
replace adultsinhh = 3 if adultsinhh >= 3

label define adultsinhh_label	1 "1 adult"  ///
								2 "2 adults" ///
								3 "3 or more adults" 

								
* benefitreliant

gen benefitreliant = 0
replace benefitreliant = 1 if (iw_hifapti>=(0.6*(iw_hifditp-iw_hifditn)))&(iw_hifapti>0)

label define benefitreliant_label		0 "Household is not reliant on Government benefits" ///
										1 "Household has 60% or more of their disp inc from Government benefits"

		
		
* ftstudent

gen ftstudent = iw_edfts

label define ftstudent_label			0 "Not studying full-time" ///
										1 "Studying full-time"
	
	
* topupsample

gen topupsample = iw_hhtup

label define topupsample_label			0 "Main Sample" ///
										1 "Top-Up Sample"
										
										
* Assign labels				
foreach set of global predictors {	
	local variable = subinstr("`set'","i.","",1)
	label values `variable' `variable'_label
}

* Check frequency tabulations by wave
foreach set of global predictors {
	local variable = subinstr("`set'","i.","",1)
	tab `variable' wave , miss
}

* Save dataset 
save "$workinglocation\propensities_wave11respondents.dta", replace

*********************************************************************************************************************************************************************************

log close

