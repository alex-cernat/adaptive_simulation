* Calculates number of PQs completed each week of field for waves 12-16.

* Run parameters
set more off
macro drop _all

* Specify HILDA release
global release = 160

* Other global and directory definitions
global releasenum = substr("$release",1,2)
global releaseu = "$release"+"u"
global releasew = substr("$alphabet",$releasenum,1)
global hildalocation   "X:\HILDA\Release $release\files\STATA $releaseu"
global workinglocation "H:\E\working_arc\Adaptivedesign"
global logslocation    "H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Logfiles"

log using "$logslocation\interviewcountsummary.log", replace

* Fieldwork period start dates (i.e., first day after first F2F training session)
global w16_p1start = "26/07/2016"
global w15_p1start = "28/07/2015"
global w14_p1start = "29/07/2014"
global w13_p1start = "30/07/2013"
global w12_p1start = "31/07/2012"

* Calculate days in field and weeks in field
forvalues wave = 12/16 {
	local w = substr("abcdefghijklmnopqrstuvwxyz",`wave',1)
	use xwaveid `w'hhidate `w'hhtup using "$hildalocation\rperson_`w'$releaseu", clear
	rename `w'* *
	gen wave = `wave'
	gen hhidate_days = date(hhidate,"DMY")
	local p1start = "w"+"`wave'"+"_p1start"
	gen daysfield = hhidate_days-td($`p1start')+1
	replace daysfield=1 if daysfield<=0
	gen weeksfield = 1 if daysfield>=1 & daysfield<=7
	forvalues weeks = 2/31 {
		local weeks_minus_one = `weeks'-1
		replace weeksfield = `weeks' if daysfield>(`weeks_minus_one'*7) & daysfield<=(`weeks'*7)
	}
	replace weeksfield = 2 if daysfield>7 & daysfield<=14
	
    * Create long file
	if `wave' == 12 {
        save "$workinglocation\rperson_allwaves", replace
	}
	else {
		append using "$workinglocation\rperson_allwaves"
		sort wave
        save "$workinglocation\rperson_allwaves", replace
	}
}

* Overall sample
*display in red "Days in field - Overall sample"
*tab daysfield wave, miss
display in red "Weeks in field - Overall sample"
tab weeksfield wave, miss

* Main sample
*display in red "Days in field - Main sample"
*tab daysfield wave if hhtup==0, miss
display in red "Weeks in field - Main sample"
tab weeksfield wave if hhtup==0, miss

* TopUp sample
*display in red "Days in field - TopUp sample"
*tab daysfield wave if hhtup==1, miss
display in red "Weeks in field - TopUp sample"
tab weeksfield wave if hhtup==1, miss

log close
