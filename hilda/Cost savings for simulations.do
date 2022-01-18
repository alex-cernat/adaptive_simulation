
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
log using "$logslocation\costsavings for sim.log", replace

* Propensities dataset (wave 11 characteristics, waves 12-16)
use "$workinglocation\propensities_w11r_sim.dta", clear
sort xwaveid wave hhid
drop _merge
merge 1:1 xwaveid wave hhid using "$workinglocation\pqcall" //, keep(master match)
drop _merge

* calculate number of calls made in P2 and P3 (combined). 
gen telcount=callcount-f2fcount
tab telcount
gen p1telcount=p1callcount-p1f2fcount
tab p1telcount

capture drop p23callcount
gen p23callcount=callcount-p1callcount
gen p23f2fcount=f2fcount-p1f2fcount
gen p23telcount=p23callcount-p23f2fcount

gen ivwtelcount=ivwcallcount-ivwf2fcount

* Identify problems in call counts
* PROBLEM 1: Has P23 calls less than zero
count if p23callcount<0
*list xwaveid hhid wave callcount p1callcount p23callcount field_opt1 if p23callcount<0
* PROBLEM 2: Has P23 f2f calls less than zero
count if p23f2fcount<0
*list xwaveid hhid wave callcount p1callcount p23callcount f2fcount p1f2fcount p23f2fcount field_opt1 if p23f2fcount<0
* PROBLEM 3: Has P23 tel calls less than zero
count if p23telcount<0
*list xwaveid hhid wave callcount p1callcount p23callcount telcount p1telcount p23telcount field_opt1 if p23telcount<0

bys wave hhid: egen hhfield_opt1=mean(field_opt1) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt2=mean(field_opt2) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt3=mean(field_opt3) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt4=mean(field_opt4) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt5=mean(field_opt5) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt6=mean(field_opt6) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt7=mean(field_opt7) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt8=mean(field_opt8) if p23callcount>0 & p23callcount!=. & response_P1==0
bys wave hhid: egen hhfield_opt9=mean(field_opt9) if p23callcount>0 & p23callcount!=. & response_P1==0

tab hhfield_opt1 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt2 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt3 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt4 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt5 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt6 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt7 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt8 if p23callcount>0 & p23callcount!=. & response_P1==0
tab hhfield_opt9 if p23callcount>0 & p23callcount!=. & response_P1==0

* carry HH level call count info to people interviewed in current wave but were not W11 respondents (eg W11 non-respondents, new entrants, people turning 15 etc)
sort wave hhid p1callcount
bys wave hhid: replace callcount   =callcount[_n-1]    if callcount   ==.
bys wave hhid: replace f2fcount    =f2fcount[_n-1]     if f2fcount    ==.
bys wave hhid: replace telcount    =telcount[_n-1]     if telcount    ==.
bys wave hhid: replace p1callcount =p1callcount[_n-1]  if p1callcount ==.
bys wave hhid: replace p1f2fcount  =p1f2fcount[_n-1]   if p1f2fcount  ==.
bys wave hhid: replace p1telcount  =p1telcount[_n-1]   if p1telcount  ==.
bys wave hhid: replace p23callcount=p23callcount[_n-1] if p23callcount==.
bys wave hhid: replace p23f2fcount =p23f2fcount[_n-1]  if p23f2fcount ==.
bys wave hhid: replace p23telcount =p23telcount[_n-1]  if p23telcount ==.

* generate calls saved at HH level if not issue particular people in HH (in HH where issued person to P23 and interview gained, then calls saved are those after last interview). If all remaining NR not issued, then calls saved are those made in P23 (note this assumes no additional work done on people who were not responding in base wave).
forvalues opt = 1/9 {
  foreach type in callcount f2fcount telcount {
    display in red "Simulation option `opt', calltype = `type'"
    bys wave hhid: egen ivw`type'max_opt`opt'=max(ivw`type') if hhfield_opt`opt'>0 & hhfield_opt`opt'<=1   // some people in HH have been issued to field in P23
    gen `type'save_opt`opt'=`type'-ivw`type'max_opt`opt' if field_opt`opt'==0 & p23callcount>0 & p23callcount!=. & response_P1==0 & ivw`type'max_opt`opt' != .
    replace `type'save_opt`opt'=`type'-p1`type' if field_opt`opt'==0 & p23callcount>0 & p23callcount!=. & response_P1==0 & hhfield_opt`opt'==0  // noone in HH has been issued to field in P23
    replace `type'save_opt`opt'=0 if field_opt`opt'==0 & p23callcount>0 & p23callcount!=. & response_P1==0 & hhfield_opt`opt'>0 & `type'save_opt`opt'==.
  }
  display in red "Summary for simulation option `opt'"
  bys wave hhid field_opt`opt': gen notissuedcount_opt`opt'=_n if field_opt`opt'==0 & p23callcount>0 & p23callcount!=. & response_P1==0
  tab callcountsave_opt`opt' if notissuedcount_opt`opt'==1
  tab f2fcountsave_opt`opt' if notissuedcount_opt`opt'==1
  tab telcountsave_opt`opt' if notissuedcount_opt`opt'==1
  bys topupsample wave: sum callcountsave_opt`opt' f2fcountsave_opt`opt' telcountsave_opt`opt' if notissuedcount_opt`opt'==1
}

* count total calls
bys topupsample wave hhid: gen n=_n
bys topupsample wave: sum callcount f2fcount telcount p23callcount p23f2fcount p23telcount if n==1  // count one per HH

matrix c=J(10,40,-999)
forvalues s=0/1 {
  forvalues w=$startwave/$endwave {
    forvalues opt = 0/9 {
	  local row=`w'-$initialwave+`s'*5
      foreach type in f2fcount telcount {
	    local col1=`opt'*4+1
	    local col2=`opt'*4+2
	    local col3=`opt'*4+3
	    local col4=`opt'*4+4
        if `opt'==0 {
	      if "`type'"=="f2fcount" {
            quietly count if topupsample==`s' & wave==`w'   // counts persons
	        matrix c[`row',`col1']=r(N)
            quietly count if topupsample==`s' & wave==`w' & n==1  // counts hh
            matrix c[`row',`col2']=r(N)
	      }
          quietly sum `type' if topupsample==`s' & wave==`w' & n==1  // counts hh
	      if "`type'"=="f2fcount" {
	        matrix c[`row',`col3']=r(mean)
	      }
	      else {
	        matrix c[`row',`col4']=r(mean)
		  }  
		}
		else {
          if `w'==12 {
            matrix c[`row',`col1']=0
            matrix c[`row',`col2']=0
            matrix c[`row',`col3']=0
            matrix c[`row',`col4']=0
		  }
		  else {
	        if "`type'"=="f2fcount" {
              quietly count if topupsample==`s' & wave==`w' & notissuedcount_opt`opt'>=1 & notissuedcount_opt`opt'!=.  // counts persons
	          matrix c[`row',`col1']=r(N)
              quietly count if topupsample==`s' & wave==`w' & notissuedcount_opt`opt'==1  // counts hh
	          matrix c[`row',`col2']=r(N)		  
	        }
            quietly sum `type'save_opt`opt' if topupsample==`s' & wave==`w' & notissuedcount_opt`opt'==1  // counts hh
	        if "`type'"=="f2fcount" {
	          matrix c[`row',`col3']=r(mean)
	        }
	        else {
	          matrix c[`row',`col4']=r(mean)
		    }  
		  }
		}
	  }
    } 
  }
}
matrix rownames c = mainw12 mainw13 mainw14 mainw15 mainw16 topupw12 topupw13 topupw14 topupw15 topupw16
matrix colnames c = Allpers Allhh AvF2F AvPh  Spers_1 Shh_1 Sf2f_1 Sph_1  Spers_2 Shh_2 Sf2f_2 Sph_2  Spers_3 Shh_3 Sf2f_3 Sph_3  Spers_4 Shh_4 Sf2f_4 Sph_4  Spers_5 Shh_5 Sf2f_5 Sph_5  Spers_6 Shh_6 Sf2f_6 Sph_6  Spers_7 Shh_7 Sf2f_7 Sph_7  Spers_8 Shh_8 Sf2f_8 Sph_8  Spers_9 Shh_9 Sf2f_9 Sph_9

matlist c

log close
