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

global predvars "agerange employment owner sectionofstate relationship likelymove healthcondition countryofbirth education sex childreninhh adultsinhh benefitreliant ftstudent"
global callvars_freq "p1lastoutcome p1type p1lasttime p1lastday p1ivw" 
global callvars_sum "p1callcount p1propnc p1duration"

use "$workinglocation\propensities_w11r_sim.dta", clear

* check for missings in original variables
*tab iw_hgage if wave==$startwavep1, miss  // no missings
*tab iw_esbrd if wave==$startwavep1, miss  // no missings
tab iw_esdtl if wave==$startwavep1, miss  // 25 missings
tab iw_jbhruc if wave==$startwavep1, miss  // 30 missings
replace employment=. if iw_esdtl==7
tab iw_mrcurr if wave==$startwavep1, miss  // 7 missings
replace relationship=. if iw_mrcurr<0
tab iw_hstenr if wave==$startwavep1, miss  // 42 cases missing
replace owner=. if iw_hstenr<0
*tab iw_hhsos if wave==$startwavep1, miss  // no missing
*tab iw_hhrih if wave==$startwavep1, miss  // no missing
tab iw_mhnyr if wave==$startwavep1, miss  // 5 cases missing
replace likelymove=. if iw_mhnyr<0
tab iw_helth if wave==$startwavep1, miss  // 4 cases missing
tab iw_helthwk if wave==$startwavep1, miss  // 6 cases missing
tab iw_helthdg if wave==$startwavep1, miss  // 19 cases missing
replace healthcondition=. if iw_helth<0 | iw_helthwk<-1 | iw_helthdg<-1
tab iw_anbcob if wave==$startwavep1, miss  // 5 cases missing
replace countryofbirth=. if iw_anbcob<0
*tab iw_edhigh if wave==$startwavep1, miss  // no missing
*tab iw_hgsex if wave==$startwavep1, miss  // no missing
*tab iw_hhadult if wave==$startwavep1, miss  // no missing
*tab iw_hhpers if wave==$startwavep1, miss  // no missing
*sum iw_hifapti iw_hifditp iw_hifditn if wave==$startwavep1  // no missing (imputed)
*tab iw_edfts if wave==$startwavep1, miss  // no missing

gen issuep23=1 if response_P1==0 & p23callcount>0 
replace issuep23=0 if issuep23==. & response_P1==1
replace issuep23=0 if issuep23==. & response_P1==0 & p23callcount==0
tab issuep23, miss

forvalues samplenumber=1/2 {
  if `samplenumber'==1 {
    foreach var of global predvars  {
      label list `var'_label
    }
    foreach var of global callvars_freq  {
      label list `var'_label
    }
    display "main sample"
  }
  else if `samplenumber'==2 {
    display "topup sample"
  }
  tab1 $predvars if wave==$startwave & iw_hhtup==(`samplenumber'-1) & issued==1, miss nolabel
  foreach var of global callvars_freq  {
   tab `var' wave if (callcount>=0 & callcount!=.) & iw_hhtup==(`samplenumber'-1) & issued==1, miss col nolabel
  }
  tab issuep23 wave if (callcount>=0 & callcount!=.) & iw_hhtup==(`samplenumber'-1) & issued==1, miss col nolabel
  forvalues w=$startwave/$endwave {
	display "Summary statistics for continuous variables, wave=`w'"
    sum  $callvars_sum  if (callcount>=0 & callcount!=.) & wave==`w' & iw_hhtup==(`samplenumber'-1) & issued==1 & (p1callcount>0 & p1callcount!=.)
  }	  
}

