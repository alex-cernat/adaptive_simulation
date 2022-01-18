%let workloc = H:\E\working_arc\Adaptivedesign; 
%let logsloc = H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Logfiles; 
%let resultsloc = H:\Documents\HILDA Project\ARC Methodology\Adaptive design\Results; 

* Create runtimes file ;
	filename runtimes "&logsloc.\Run Times - w11r - simulations.txt";
	%put ;

	* Enter start time in runtimes file ;
	data _null_ ;
	  file runtimes ;
	  t = datetime() ;
	  put " RISQ program starting at " t datetime.;
	  put "   ";
	run; 

	* Define macro for adding to runtimes file ;
	%macro logentry ( fref , msg ) ;
		data _null_ ;
	    file &fref. mod ;
	    t = datetime() ;
	    put t datetime. " : " &msg. ;
	  	run;
	%mend logentry;

%let startwave=12;
%let endwave=16;

%macro risqprogram(simopt,wave,sample,period) ;

proc printto new
	log = "&logsloc.\RISQLog_w11r_simopt&simopt._wave&wave._&period._&sample..log"
	print = "&logsloc.\RISQResults_w11r_simopt&simopt._wave&wave._&period._&sample..lst" ;
run ;

* Indicate start wave, sample, period combination ;
%logentry( fref = runtimes , msg = "Starting simulation option &simopt, wave &wave., &sample. sample, period &period. ") ;


**** FOLLOWING CODE AS SPECIFIED BY RISQ MANUAL *****;

/********************** change the name of the library *******************/
libname risq "&workloc." ; run; /* updated from RISQ default */
 /**************  fill out this section   **********************************************/

%if &sample.=Overall %then %do ;
	%let variablenum=15;  
	%let variablenoint=15;
	%if &wave.=12 %then %do ; 	
		%let popsize=17646114;
		%let samsize=17612; 
	%end ;
	%if &wave.=13 %then %do ; 
		%let popsize=17646114;
		%let samsize=17562;
	%end ;
	%if &wave.=14 %then %do ; 
		%let popsize=17646114;
		%let samsize=17490;
	%end ;
	%if &wave.=15 %then %do ;
		%let popsize=17646114;
		%let samsize=17424; 
	%end ;
	%if &wave.=16 %then %do ; 
		%let popsize=17646114;
		%let samsize=17386;
	%end ;
%end ;
%if &sample.=Main  %then %do ;
	%let variablenum=14;  
	%let variablenoint=14;
	%if &wave.=12 %then %do ; 	
		%let popsize=17646114;
		%let samsize=13603; 
	%end ;
	%if &wave.=13 %then %do ; 
		%let popsize=17646114;
		%let samsize=13572;
	%end ;
	%if &wave.=14 %then %do ; 
		%let popsize=17646114;
		%let samsize=13524;
	%end ;
	%if &wave.=15 %then %do ;
		%let popsize=17646114;
		%let samsize=13484; 
	%end ;
	%if &wave.=16 %then %do ; 
		%let popsize=17646114;
		%let samsize=13458;
	%end ;
%end ;
%if &sample.=TopUp %then %do ;
	%let variablenum=14;  
	%let variablenoint=14;
	%if &wave.=12 %then %do ; 	
		%let popsize=17646114;
		%let samsize=4009; 
	%end ;
	%if &wave.=13 %then %do ; 
		%let popsize=17646114;
		%let samsize=3990;
	%end ;
	%if &wave.=14 %then %do ; 
		%let popsize=17646114;
		%let samsize=3966;
	%end ;
	%if &wave.=15 %then %do ;
		%let popsize=17646114;
		%let samsize=3940; 
	%end ;
	%if &wave.=16 %then %do ; 
		%let popsize=17646114;
		%let samsize=3928;
	%end ;
%end ;

/* %let variablestrat=1;*/ /** number of stratifying variables not in the model for unconditional partial R-indicator**/
/** names of stratifying variable for unconditional partial R-indicator not in the model**/
/* %let strat1='joba'; */
%let variablestrat= 0;		
%let strat1= ;				

%let variableinter=0;/** number of variables that are in interactions (do not repeat variables)**/ 
/** names of  variables in interactions**/
/* %let vvar1='gender'; */
/* %let vvar2='urb'; */

/** names of   variables in original response model **/ 
%if &sample.=Overall %then %do ;
	%let var1= 'agerange';
	%let var2= 'employment';
	%let var3= 'owner';
	%let var4= 'sectionofstate';
	%let var5= 'relationship';
	%let var6= 'likelymove';
	%let var7= 'healthcondition';
	%let var8= 'countryofbirth';
	%let var9='education';
	%let var10='sex';
	%let var11='childreninhh';
	%let var12='adultsinhh';
	%let var13='benefitreliant';
	%let var14='ftstudent';
	%let var15='topupsample';
%end ;
%if &sample.=Main  %then %do ;
	%let var1= 'agerange';
	%let var2= 'employment';
	%let var3= 'owner';
	%let var4= 'sectionofstate';
	%let var5= 'relationship';
	%let var6= 'likelymove';
	%let var7= 'healthcondition';
	%let var8= 'countryofbirth';
	%let var9='education';
	%let var10='sex';
	%let var11='childreninhh';
	%let var12='adultsinhh';
	%let var13='benefitreliant';
	%let var14='ftstudent';
%end ;
%if &sample.=TopUp %then %do ;
	%let var1= 'agerange';
	%let var2= 'employment';
	%let var3= 'owner';
	%let var4= 'sectionofstate';
	%let var5= 'relationship';
	%let var6= 'likelymove';
	%let var7= 'healthcondition';
	%let var8= 'countryofbirth';
	%let var9='education';
	%let var10='sex';
	%let var11='childreninhh';
	%let var12='adultsinhh';
	%let var13='benefitreliant';
	%let var14='ftstudent';
%end ;
%if &sample.=Overall %then %do ;
	%let xvar1= agerange;
	%let xvar2= employment;
	%let xvar3= owner;
	%let xvar4= sectionofstate;
	%let xvar5= relationship;
	%let xvar6= likelymove;
	%let xvar7= healthcondition;
	%let xvar8= countryofbirth;
	%let xvar9=education;
	%let xvar10=sex;
	%let xvar11=childreninhh;
	%let xvar12=adultsinhh;
	%let xvar13=benefitreliant;
	%let xvar14=ftstudent;
	%let xvar15=topupsample;
%end ;
%if &sample.=Main  %then %do ; 
	%let xvar1= agerange;
	%let xvar2= employment;
	%let xvar3= owner;
	%let xvar4= sectionofstate;
	%let xvar5= relationship;
	%let xvar6= likelymove;
	%let xvar7= healthcondition;
	%let xvar8= countryofbirth;
	%let xvar9=education;
	%let xvar10=sex;
	%let xvar11=childreninhh;
	%let xvar12=adultsinhh;
	%let xvar13=benefitreliant;
	%let xvar14=ftstudent;
%end ;
%if &sample.=TopUp %then %do ; 
	%let xvar1= agerange;
	%let xvar2= employment;
	%let xvar3= owner;
	%let xvar4= sectionofstate;
	%let xvar5= relationship;
	%let xvar6= likelymove;
	%let xvar7= healthcondition;
	%let xvar8= countryofbirth;
	%let xvar9=education;
	%let xvar10=sex;
	%let xvar11=childreninhh;
	%let xvar12=adultsinhh;
	%let xvar13=benefitreliant;
	%let xvar14=ftstudent;
%end ;
%if &sample.=Overall %then %do ; 
	%let nvar1=8;
	%let nvar2=4;  * changed from 7 to 4 when reduced working hours to FT & PT;
	%let nvar3=2;
	%let nvar4=3;
	%let nvar5=4;
	%let nvar6=2;
	%let nvar7=3;
	%let nvar8=3;
	%let nvar9=6;
	%let nvar10=2;
	%let nvar11=3;
	%let nvar12=3;
	%let nvar13=2;
	%let nvar14=2;
	%let nvar15=2;
%end ;
%if &sample.=Main  %then %do ; 
	%let nvar1=8;
	%let nvar2=4;  * changed from 7 to 4 when reduced working hours to FT & PT;
	%let nvar3=2;
	%let nvar4=3;
	%let nvar5=4;
	%let nvar6=2;
	%let nvar7=3;
	%let nvar8=3;
	%let nvar9=6;
	%let nvar10=2;
	%let nvar11=3;
	%let nvar12=3;
	%let nvar13=2;
	%let nvar14=2;
%end ;
%if &sample.=TopUp %then %do ; 
	%let nvar1=8;
	%let nvar2=4; * changed from 7 to 4 when reduced working hours to FT & PT;
	%let nvar3=2;
	%let nvar4=3;
	%let nvar5=4;
	%let nvar6=2;
	%let nvar7=3;
	%let nvar8=3;
	%let nvar9=6;
	%let nvar10=2;
	%let nvar11=3;
	%let nvar12=3;
	%let nvar13=2;
	%let nvar14=2;
%end ;



data  att;
set risq.propensities_w11r_sim; /* Updated from controlcbs to the name of the downloaded dataset */

/***** responsesamp1 is the indicator for  response, 1=response, 0=non-response*/
%if &sample=Overall %then %do ;
	where wave=&wave. ;
%end ;
%if &sample=Main %then %do ;
	where wave=&wave. and iw_hhtup=0 ;
%end ;
%if &sample=TopUp %then %do ;
	where wave=&wave. and iw_hhtup=1 ;
%end ;

/* Added for simulations */
%if &simopt=1 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt1=1 then responsesamp1=1;
		else if response_P2_opt1=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt1=1 then responsesamp1=1;
		else if response_P3_opt1=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=2 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt2=1 then responsesamp1=1;
		else if response_P2_opt2=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt2=1 then responsesamp1=1;
		else if response_P3_opt2=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=3 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt3=1 then responsesamp1=1;
		else if response_P2_opt3=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt3=1 then responsesamp1=1;
		else if response_P3_opt3=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=4 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt4=1 then responsesamp1=1;
		else if response_P2_opt4=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt4=1 then responsesamp1=1;
		else if response_P3_opt4=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=5 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt5=1 then responsesamp1=1;
		else if response_P2_opt5=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt5=1 then responsesamp1=1;
		else if response_P3_opt5=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=6 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt6=1 then responsesamp1=1;
		else if response_P2_opt6=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt6=1 then responsesamp1=1;
		else if response_P3_opt6=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=7 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt7=1 then responsesamp1=1;
		else if response_P2_opt7=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt7=1 then responsesamp1=1;
		else if response_P3_opt7=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=8 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt8=1 then responsesamp1=1;
		else if response_P2_opt8=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt8=1 then responsesamp1=1;
		else if response_P3_opt8=0 then responsesamp1=0;
	%end ;
%end ;
%if &simopt=9 %then %do ;
	%if &period=P1 %then %do ;
		if tracking_day69=1 then responsesamp1=1;
		else if tracking_day69 in (0,2) then responsesamp1=0;
	%end ;
	%if &period=P2 %then %do ;
		if response_P2_opt9=1 then responsesamp1=1;
		else if response_P2_opt9=0 then responsesamp1=0;
	%end ;
	%if &period=P3 %then %do ;
		if response_P3_opt9=1 then responsesamp1=1;
		else if response_P3_opt9=0 then responsesamp1=0;
	%end ;
%end ;

* drop cases with missing covariates (;
if agerange=. or employment =. or owner =. or sectionofstate =. or relationship =. or likelymove =. or healthcondition =. or countryofbirth =. or education =. or 
 sex =. or childreninhh =. or adultsinhh =. or  benefitreliant =. or ftstudent  =. then delete;

 /******define weight or define piinv= dweight if there are differential design weights in the file*****/
 pi=&samsize/&popsize;
 piinv=1/pi;  
 /*******************************************************/
 run;

 
/************   no need to go beyond this point *******************/
/***********************************************************************/
/**********************************************************************/

data risq.a;
  set att;
run;

data rlll;
data stt;
%macro c;
%do po=1 %to &variablenum;
data  astt;
 set stt;
 length t $20;
t= &&var&po;
 output;
 run;
 data rlll;
 set rlll astt  ;
* drop t;
%end;
%mend c;
%c;
run;
data flll;
data rst;
%macro c;
%do pr=1 %to &variablestrat;
data hrts;
set rst;
length t $20;
t=&&strat&pr;
output;
run;
data flll;
set flll hrts;
%end;
%mend c;
%c;
run;
 data nlll;
data ntt;
%macro c;
%do po=1 %to &variablenoint;
data  nstt;
 set ntt;
 length t $20;
t= &&var&po;
 output;
 run;
 data nlll;
 set nlll nstt  ;
 %end;
 %do po=1 %to &variableinter;
 data ostt;
 set ntt;
 length t $20;
 t=&&vvar&po;
 output;
 run;
 data nlll;
 set nlll ostt;
 %end;
  %mend c;
%c;
run;
%let jj=%eval(&variableinter+&variablenoint);
%let allv=%eval(&variableinter+&variablenoint+&variablestrat);
 data cllla;
set nlll;
if t ne ' ';
row+1;
run;
data t;
set cllla;
drop row;
data listofpartialbs;
set t flll;
if t ne ' ';
row+1;
 run;
data listofpartialws;
set cllla;
if t ne ' ';
 run;
%macro fff;
%do po=1 %to &jj;
data clll&po;
set cllla;
 if row=&po then delete;
 run;
 %end;
%mend fff;
%fff;
run;
data sx ;
set rlll  (keep=t);
length allvars $10000;
retain allvars ' ';
set rlll  end=eof;
allvars=trim(left(allvars))||' '||left(t);
if eof then call symput ('varlist', allvars);
run;
data sx ;
set nlll  (keep=t);
length allvars $10000;
retain allvars ' ';
set nlll  end=eof;
allvars=trim(left(allvars))||' '||left(t);
if eof then call symput ('varlistno', allvars);
run;

 proc logistic data=risq.a outest= betasamp covout   outdesign=xdfull /*noprint*/   ;
      class      &varlistno ;
      model responsesamp1=   &varlist
     / expb ;
           output out= predsamp  p=phatsamp  ;
      run;


data rindicator;
set predsamp;
ns=piinv;
 os=1;
 mmm=1;
 rphatsamp= 1-phatsamp;
run;
data predsampall;
set predsamp;
run;
data rindicatorall;
set rindicator;
run;
data betasampall;
set betasamp;
run;
data xdfullall;
set xdfull;
data rindicatora ;
set rindicator;
run;
data t;
set xdfullall;
drop responsesamp1;
 proc contents noprint data=t out=metadata;
run;
Proc means data=metadata nway noprint  ;
output out=df N=;
run;
data ddf;
set df;
g=_freq_;
call symput('nocat', g);
run;
%put &nocat;run;
 %macro calcind;
proc means data=rindicatora  nway sum mean     var  noprint  ;
var rphatsamp      ;
   weight piinv;
output out=tt sum(rphatsamp)=sphatsamp mean(rphatsamp)=mphatsamp var(rphatsamp)=vphatsamp   ;
run;
data uu;
set tt;
os=1;
HT=(1/&popsize)*sphatsamp;
keep HT sphatsamp mphatsamp   os;
 run;
 data ll;
 set uu;
 call symput('htmean',ht);
 run;
data cc;
merge rindicatora  (in=v1) uu;
by os;
if v1;
run;
data dd;
set cc;
 sr_ind= piinv* (rphatsamp-HT)**2  ;
proc means sum data=dd nway noprint    ;
var sr_ind  ;
output out= result sum=;
run;
proc means   data=dd nway var   sum   noprint   ;
var sr_ind    ;
output out= uuu var(sr_ind )= vsr_ind sum(sr_ind)=ssr_ind;
run;
data part1ofvar;
set uuu;
vvv =&samsize*(vsr_ind );
 run;
data aa;
set  betasamp;
if _type_='PARMS';
 drop _link_ _type_ _status_ _name_ _lnlike_;
run;
data aaa ;
set  betasamp;
if _type_='COV';
if intercept ne .;
 drop _link_ _type_ _status_ _name_ _lnlike_;
run;
data rdfull ;
set xdfull;
drop responsesamp1;
run;
proc iml;
use aaa  ;
read all into bet;
use aa ;
read all   into parms ;
use rdfull;
read all   into x1;
use rindicator;
read all var {piinv} into invpi;
ff=x1 *parms`;
eff=exp(ff)/(1+exp(ff));
dr=1-eff;
gexp=dr#eff;
nr=nrow(eff);
avero=eff[+]/nr;
rr=eff-avero;
b=gexp#x1;
nb=nrow(b);
aveb=b[+,]/nb;
nff=ncol(x1);
create columnsd from nff;
append from nff;
c=j(nb,nff,1);
d=c#aveb;
zz=b-d;
zzz=invpi#zz;
rf=rr`;
terma=rf*zzz;
tee=zzz`;
termb=tee*zz ;
first=terma*bet;
second=first*terma`;
one=second#4;
next=termb*bet;
tt=next*next ;
uuu=2#trace(tt);
secondterm=one+uuu;
create secondt from secondterm;
append from secondterm;
c=gexp#gexp;
create apelet from c;
append from c;
va=x1*bet;
create bpelet from va;
append from va;
create cpelet from x1 ;
append from x1;
create dpelet from invpi;
append from invpi;
quit;run;
data cc;
set columnsd;
call symput('numc',col1);
run;
%let numb=&numc;
run;
data gg;
set cpelet;
array acol acol1-acol&numb;
array  col  col1- col&numb;
do i=1 to &numb;
acol(i)=col(i);
end;
keep acol1-acol&numb;
run;
data hh;
set bpelet; set gg;
array acol acol1-acol&numb;
array  col  col1- col&numb;
h=0;
do i=1 to &numb;
h=h+acol(i)*col(i);
end;
run;
data rrr;
set apelet;
cccc=col1;
keep cccc;
data qqq;
set dpelet;
dddd=col1;
keep dddd;
data ii;
set hh; set rrr; set qqq;
o=h*cccc*dddd;
proc means sum data=ii nway noprint;
var o;
output out=kl sum=;
run;
data g;
set kl;
bia=o  ;
keep bia  ;
run;
data r;
set part1ofvar; set secondt;
variance_r=(1/&popsize)*(1/ssr_ind)*(vvv+col1);
std_r=sqrt(variance_r);
ci_r=std_r*1.96;
run;
data t;
set result;set g ;
srvar=(1/(&popsize-1))*((1+1/&samsize-1/&popsize)*sr_ind-bia);
r_indicator=1-2*sqrt(srvar);
svarb=(1/(&popsize-1)) *(sr_ind );
r_withbias=1-2*sqrt(svarb);
cv=sqrt(srvar)/&htmean;
cv_unadjusted=sqrt(svarb)/&htmean;
 run;
 data finalfilepart;
 set t;  set r;
 avgprop=&htmean;
 var_cv  =((variance_r/4)/(avgprop*avgprop))   +   (  (srvar *srvar ) /  (&samsize*(avgprop**4))) ;
 std_cv =sqrt(var_cv );
 var_cv_unadjusted =((variance_r/4)/(avgprop*avgprop))   +   (  (svarb *svarb ) /  (&samsize*(avgprop**4))) ;
 std_cv_unadjusted =sqrt(var_cv_unadjusted);
 keep r_indicator r_withbias variance_r std_r ci_r bia sr_ind srvar svarb avgprop cv cv_unadjusted std_cv std_cv_unadjusted;
 run;
 %mend calcind;
 %calcind;
 run;
 proc print  data= finalfilepart; run;
data bipartall;
set finalfilepart;
bipartall=bia/&popsize;
call symput('biasall',bipartall);
run;
data finalfileR_ind;
set finalfilepart;
run;
 
/* partial conditional R- indicator*/
data allwr ; data allbr ; data allwrr; data varc_level ;run;
%macro rrr;
%do po=1 %to &jj; 
data sx ;
set clll&po  (keep=t);
length allvars $10000;
retain allvars ' ';
set clll&po  end=eof;
allvars=trim(left(allvars))||' '||left(t);
if eof then call symput ('varlist1', allvars);
run;
 proc logistic data=risq.a outest= betasamp covout   outdesign=xdfull /*noprint*/         ;
      class      &varlist1 ;
      model responsesamp1=   &varlist1
     / expb ;
           output out= predsamp  p=phatsamp ;
      run;
 data rindicat  ;
set predsamp;
ns=piinv;
 os=1;
 mmm=1;
 rphatsamp= 1-phatsamp;
run;

data rindicatora;
set rindicat;
run;
 %calcind;
 
 data varx&po;
 set finalfilepart;
 varx=std_r/2;
 run;
proc means sum mean data= rindicatorall  nway noprint     ;
var ns rphatsamp   ;
class  &varlist1;
id mmm;
output out=epw  sum(ns)=fbar&po  mean(rphatsamp )=mrphat&po ;
run;
 proc sort data=rindicatorall; by &varlist1; run;
data rindi&po ;
 merge rindicatorall (in=v1) epw  ;
 by &varlist1;
 if v1;
 run;
proc means mean   data=rindicatorall nway noprint  ;
var  rphatsamp  ;
output out=ephar   mean(rphatsamp )=mrphatall ;
run;
data ephaar;
set ephar;
mmm=1;
data al;
merge epw  (in=v1) ephaar;
by mmm;
if v1;
run;
data partialaan ;
set al;
p2Zka&po =sqrt(((fbar&po )/&popsize))*(mrphat&po-mrphatall);
p1Zka&po =p2Zka&po*p2Zka&po;
  run;
   proc means sum nway noprint ;
 var p1Zka&po ;
 output out=varr  sum(p1Zka&po )=betweenvara;
 run;
data allbr ;
set allbr  varr ;
run;
data tall ;
set rindi&po ;
 p3Zka&po=(1/(&popsize-1))*piinv*(rphatsamp -mrphat&po)**2 ;
  run;
proc means sum data=tall    nway noprint ;
var p3Zka&po ;
output out=tzr  sum(p3Zka&po )=withinvara ; run;
run;
data allwr ;
set allwr  tzr ;
run;
data fmn;
set cllla;
if row ne &po then delete;
data afm;
set fmn;
call symput('hvar',t);
run;
data sss;
set tall;
sss=1;
 ttt=piinv;
run;
proc means sum var data=sss   nway  noprint  ;
 var p3Zka&po    sss ttt ;
class &hvar;
output out=tzrfina  sum(p3Zka&po    )=p3zk
  sum(sss ttt)=ns ps; run;
run;
data allwrr ;
set allwrr  tzrfina ;
data partialp3&po;
set tzrfina ;
sqtp3zk=sqrt(p3zk);
drop _type_ _freq_;
run;
data  varc_level;
set  varc_level  varx&po;
run;
 %end;
%mend rrr;
%rrr;
run;

/*************************variance p3 ******************************/

data gg;
 set   xdfullall;
drop responsesamp1;
run;
 proc contents noprint data=gg out=metadata;
run;
Proc means data=metadata nway noprint  ;
output out=df N=;
run;
data ddf;
set df;
g=_freq_;
call symput('noca', g);
run;
%put &noca;run;
data varfinalp3;run;
%macro varp3;
%do po=1 %to   &jj;
  data sx ;
set clll&po  (keep=t);
length allvars $10000;
retain allvars ' ';
set clll&po  end=eof;
allvars=trim(left(allvars))||' '||left(t);
if eof then call symput ('varlist1', allvars);
run;  
%do qo=1 %to   &&nvar&po;
data gr;
set predsampall;
 rphatsamp= phatsamp;
data trla;
 set gr;
 z1=rphatsamp*(1-rphatsamp);
 sortrow+1;
 run;
  data trl;
  set trla;
  keep z1;
  run; 
proc iml;
use trl  ;
read all into z;
use gg;
read all into x; 
f=z#x;
create terma from f;
append from f;
quit;run;
data l;
set trla; set terma;
run; 
%let noc=&noca;
proc means mean data=l nway noprint  ;
var col1-col&noc;
  class &varlist1; 
output out=kaa mean(col1-col&noc)=meanc1-meanc&noc;
run;
data aa;
set l;
run;
proc sort ; by &varlist1;
data bb;
set kaa;
drop _type_ _freq_;
proc sort; by &varlist1;
run;
data caa;
merge bb aa (in=v1);
by &varlist1 ;
if v1;
run;
proc sort data=caa; by sortrow; run;
data ll;
set caa;
array zz zz1-zz&noc;
array col col1-col&noc;
array meanc meanc1-meanc&noc;
do i=1 to &noc;
zz(i)=col(i)-meanc(i);
end;
run;
data trl;
 set gr;
 z1=rphatsamp ;
  sortrow+1;
  run;
proc means mean data=trl nway noprint;
var z1;
class &varlist1;
output out=ka  mean(z1)=mz1;
run;
data aa;
set trl;
proc sort; by &varlist1;
data bb;
set ka ;
drop _type_ _freq_;
proc sort; by &varlist1;
run;
data caa1;
merge bb aa (in=v1);
by &varlist1 ;
if v1;
run;
proc sort data=caa1; by sortrow;
data llf;
set caa1;
yy=rphatsamp-mz1;
run;
data ab1;
set llf;
keep yy;
data ab2;
set ll;
keep zz1-zz&noc;*&allv;
run;
data aaa ;
set  betasampall;
if _type_='COV';
if intercept ne .;
 drop _link_ _type_ _status_ _name_ _lnlike_;
run;
data rdfull ;
set xdfullall;
drop responsesamp1;
run;
data lla;
set llf;
if &&xvar&po=&qo then delta=1; else delta=0;
keep delta;
run;
proc iml;
use aaa  ;
read all into bet;
use rdfull;
read all   into x1;
use lla;
read all into delta;
use rindicatorall;
read all var {piinv} into invpi;
use ab1;
read all into rhoavg;
use ab2;
read all into zzavg;
rr=rhoavg#delta;
zz=zzavg#delta;
zzz=invpi#zz;
terma=rr`*zzz;
terman=terma/&popsize;
termb=   zzz`* zz ;
termbn=termb/&popsize;
first=terman*bet;
second=first*terman`;
one=second#4;
next=termbn*bet;
tt=next*next ;
uuu=2#trace(tt);
secondterm=one+uuu;
create secondt from secondterm;
append from secondterm;
quit;run;
/**************************************************************/
data fr;
set rindi&po;
if &&xvar&po=&qo then delta=1; else delta=0;
a=piinv* delta*rphatsamp*rphatsamp;
b=piinv*delta*rphatsamp;
c=piinv*rphatsamp;
d=piinv;
e=piinv*delta;
cou=1;
 run;
proc means sum var data=fr nway noprint;
var a b c d e   cou;
class &varlist1;
output out=ka sum(a b c d e   )=suma sumb sumc sumd sume
 sum(cou)=sc;
run;
 data f;
set ka;
g=(1/&popsize)*(suma-(2*sumb*sumc)/sumd+(sumc*sumc*sume)/(sumd*sumd));
run;
data a;
set f;
drop _type_ _freq_;
proc sort; by &varlist1;
data b;
set fr;
proc sort; by &varlist1;
run;
data c;
merge b a (in=v1);
by &varlist1 ;
if v1;
run;
data try;
set c;
 phi= (1/&popsize)*(
(delta*rphatsamp*rphatsamp)-
(2*sumc*delta*rphatsamp/sumd)+
(-2*sumb/sumd+2*sumc*sume/(sumd*sumd))*rphatsamp+
(2*sumb*sumc/(sumd*sumd)-2*sumc*sumc*sume/(sumd*sumd*sumd))
+(sumc*sumc*delta/(sumd*sumd)));
 dphi=  piinv*    phi;
count=1;
ccpop=piinv;
run;
proc means sum var data=try nway   noprint  ;
var ccpop count dphi       ;
output out=v1 sum(ccpop count
  dphi )=sumccpop sumcount
sdphi var(dphi)=vdphi;
run;
data uv;
set v1;
 g= &samsize*vdphi ;
proc means sum data=uv nway noprint;
var  g ;
output out=h1 sum=;
run;
data t;
set partialp3&po;
if   &&xvar&po=&qo;
call  symput('val', sqtp3zk);run;
data ta;
set secondt;
call  symput('secondorder', col1);run;
data hh1;
set h1;
second=&secondorder;
var1s=(1/4)*  (1/(&val*&val))* (g+&secondorder) ;
std1s=sqrt(var1s);
run;
data ft;
set hh1;
vstd1s =std1s;
 secondor =second;
drop std1s     second;
run;
 data varfinalp3;
set varfinalp3 ft;
run;
%end;
%end;
%mend varp3;
%varp3;
run;

/*unconditional partial R-indicator*/
data allw;data allb;  data  varu_level; run;
%macro partial;
%do i=1 %to  &variablenoint;


data radd;
set rindicatorall;
run;
data s;
length s $20;
 s=&&var&i;
  call symput('hvar',s);
run;
proc logistic data=risq.a outest= betasamp covout   outdesign=xdfull /*noprint*/    ;
      class      &hvar ;
      model responsesamp1=   &hvar
     / expb ;
           output out= predsamp  p=phatsamp;
      run;
 data rindicat  ;
set predsamp;
ns=piinv;
 os=1;
 mmm=1;
 rphatsamp=1- phatsamp;
run;
data rindicatora;
set rindicat;
run;
 %calcind;
 data varc&i;
 set finalfilepart;
 varc=  std_r/2;
 

run;
 proc means sum mean data= radd  nway noprint  ;
var ns rphatsamp ;
class   &hvar;
output out=ep&i sum(ns)=fbar&i  mean(rphatsamp)=mrphat&i ;
run;
 proc sort data=radd; by &hvar;
data rindicatorall;
 merge radd (in=v1) ep&i ;
 by &hvar;
 if v1;
 run;
%end;
%let variab=%eval(&variablenoint+1);
%let endr=%eval(&variablenoint+&variableinter);
%do j=&variab %to &endr;
data radd;
set rindicatorall;
run;
%let i=%eval(&j-&variablenoint);
data s;
length s $20;
  s=&&vvar&i;
 call symput('hvar',s);
 run;
proc logistic data=risq.a outest= betasamp covout   outdesign=xdfull /*noprint*/   ;
      class      &hvar ;
      model responsesamp1=   &hvar
     / expb ;* selection=b     ;
           output out= predsamp  p=phatsamp  ;
      run;
 data rindicat  ;
set predsamp;
ns=piinv;
 os=1;
 mmm=1;
 rphatsamp= 1-phatsamp;
run;
data rindicatora;
set rindicat;
run;
 %calcind;
  data varc&j;
 set finalfilepart;
 varc=  std_r/2;run;
 proc means sum mean data= radd  nway  noprint ;
var ns rphatsamp ;
class  &hvar;
output out=ep&j sum(ns)=fbar&j  mean(rphatsamp)=mrphat&j ;
run;
 proc sort data=radd; by &hvar;
data rindicatorall;
 merge radd (in=v1) ep&j ;
 by &hvar;
 if v1;
 run;
 %end;
%let variar=%eval(&variablenoint+1+&variableinter);
 %let endd=%eval(&variablenoint+&variableinter+&variablestrat);
%do k=&variar %to &endd;
data radd;
set rindicatorall;
run;
%let t=%eval(&k-&variablenoint-&variableinter);
data s;
length s $20;
  s=&&strat&t;
 call symput('hvar',s);
 run;
proc logistic data=risq.a outest= betasamp covout   outdesign=xdfull /*noprint*/  ;
      class      &hvar ;
      model responsesamp1=   &hvar
     / expb ;* selection=b     ;
           output out= predsamp  p=phatsamp  ;
      run;
 data rindicat  ;
set predsamp;
ns=piinv;
 os=1;
 mmm=1;
 rphatsamp= 1-phatsamp;
run;
data rindicatora;
set rindicat;
run;
 %calcind;
data varc&k;
 set finalfilepart;
 varc=  std_r/2;run;
 proc means sum mean data= radd  nway  noprint ;
var ns rphatsamp ;
class  &hvar;
output out=ep&k sum(ns)=fbar&k  mean(rphatsamp)=mrphat&k ;
run;
 proc sort data=radd; by &hvar;
data rindicatorall;
 merge radd (in=v1) ep&k ;
 by &hvar;
 if v1;
 run;
 %end;
proc means mean   data=rindicatorall nway noprint ;
var  rphatsamp ;
output out=epha   mean(rphatsamp)=mrphatall;
data tk;
set epha;
call symput('phall',mrphatall);
run;

 %do k=1 %to &endd;


data ephaa;
set epha;
_type_=1;
data a ;
merge ep&k (in=v1) ephaa;
by _type_;
if v1;
run;
 data partialp2&k;
set a ;
p2Zk&k =sqrt(((fbar&k )/&popsize))*(mrphat&k-mrphatall);
p1Zk&k =p2Zk&k*p2Zk&k;
 
  run;
  proc means sum nway noprint ;
 var p1Zk&k ;
 id mrphatall;
 output out=va  sum(p1Zk&k )=betweenvar;
 run;
  
  
data allb;
set allb va ;
%end;
%do kk=1 %to &endd;
data tall;
set rindicatorall;
 p3Zk&kk=(1/(&popsize-1))*piinv*(rphatsamp-mrphat&kk)**2 ;
  run;
proc means sum data=tall noprint nway ;
var p3Zk&kk;
output out=tz  sum(p3Zk&kk)=withinvar; run;
run;
data allw;
set allw tz ;
data varu_level;
set varu_level varc&kk;
run;
%end;
%mend partial;
%partial;
run;

proc means mean data=rindicatorall noprint;
var rphatsamp;
output out=jk mean(rphatsamp)=mall;
run;
data jjk;
set jk;
call symput('mall',mall);
run;
%let endd=%eval(&variablenoint+&variableinter+&variablestrat);


/************* variance of unconditional partial indicator *****/
data varfinalp2; run;
%macro varp2;
%do po=1 %to   &endd;
 %do qo=1 %to    &&nvar&po;
 data f;
set rindicatorall;
 if &&xvar&po=&qo  then   call symput ('mhat',mrphat&po);
 if &&xvar&po=&qo    then call symput ('fb',fbar&po);
run;
data g;
set f;
if  &&xvar&po=&qo then delta=1; else delta=0;
phi1=piinv*rphatsamp*delta/&fb;
phi2new=(piinv*rphatsamp*(1-delta))/(&popsize-&fb);
run;
proc means var sum data=g nway noprint  ;
var phi1   phi2new;
output out=h var(  phi1  phi2new)=
  vphi1   vphi2new
sum(  phi1   phi2new)=  sphi1  sphi2new;
run;
data hh;
set h;
v1=vphi1*&samsize;
v2new=vphi2new*&samsize;
run;
data ta;
set partialp2&po;
if   &&xvar&po=&qo;
call symput('val', p2zk&po);
run;
data vrl;
set hh;
varfinal =((1-(&fb/&popsize))**2)* v1+((1-(&fb/&popsize))**2)*v2new;
sefinal =  sqrt(&fb/&popsize)*sqrt(varfinal );
run;
data varfinalp2;
set varfinalp2 vrl;
%end;
%end;
%mend varp2;
%varp2;
run;
 /*********************** bias adjustments ******************/
 data tallw;
set allw;
if _type_ ne .;
data tallb;
set allb;
if _type_ ne .;
data tallwr;
set allwr;
if _type_ ne .;
data tallbr;
set allbr;
if _type_ ne .;
run;

 data partialbetween;
set tallw; set tallb; 
 t=&biasall;
  sqrtbetween=sqrt(betweenvar);
sams= &samsize;                        
ss1=t*withinvar/(withinvar+betweenvar);
ss2=t*betweenvar/(withinvar+betweenvar);
   betweenbiasadj =betweenvar-ss2;
sqtbetweenbiasadj  =sqrt( betweenbiasadj);
   run;
 data partialwithin;
set tallwr; set tallbr;  
t=&biasall;
 sqrtwithina =sqrt(withinvara);
sams= &samsize;
 ss1=t*withinvara/(withinvara+betweenvara);
 ss2=t*betweenvara/(withinvara+betweenvara);
 withinabiasadj=withinvara-ss1;
sqtwithinabiasadj =sqrt( withinabiasadj);
run;

 /************** outputs ***********/
data u1;
set varfinalp2;
if _type_ ne .;
SE_uncond_cat=sefinal;
keep SE_uncond_cat;
run;
data u2;
set varfinalp3;
if _type_ ne .;
SE_cond_cat =vstd1s;
keep  SE_cond_cat;
run;
data u3;
set varu_level;
if bia ne .;
SE_uncond_var=varc ;
srvar_uncond=srvar;
svarb_uncond=svarb;
*keep SE_uncond_var srvar_uncond svarb_uncond ;
keep SE_uncond_var;
run;
data u4;
set varc_level;
if bia ne .;
SE_cond_var =varx;
srvar_cond=srvar;
svarb_cond=svarb;
*keep SE_cond_var srvar_cond svarb_cond;
keep SE_cond_var ;
run;
data u5;
set finalfileR_ind;
LB_r=r_indicator-ci_r;
UB_r=r_indicator+ci_r;
variance_prop=svarb;
variance_prop_adj=srvar;
StdErr_r=std_r;
CV_prop_adj=cv ;  
StdErr_CV_adj=std_cv;
CV_prop_unadj=cv_unadjusted ;  
StdErr_CV_unadj=std_cv_unadjusted;
 keep R_indicator R_withbias StdErr_r  variance_prop variance_prop_adj lb_r ub_r CV_prop_adj StdErr_CV_adj CV_prop_unadj  StdErr_CV_unadj  
avgprop;
run;
 
data kkk;
set u5;
call symput('srvar',variance_prop_adj);
run;
%put &srvar;
data kkk;
set u5;
call symput('svarb',variance_prop);
run;
%put &svarb;
data f;
set listofpartialbs;
nnn=t;
drop t;
data u6;
set  partialbetween;set f;
uncond_var=betweenvar;
sqrt_uncond_var=sqrtbetween;
uncond_var_adj=betweenbiasadj;
sqrt_uncond_var_adj=sqtbetweenbiasadj;
namevaru=nnn;
keep uncond_var sqrt_uncond_var uncond_var_adj sqrt_uncond_var_adj namevaru ;
run;
data ff;
set listofpartialws;
nnn=t;
drop t;
data u7;
set partialwithin; set ff;
cond_var=withinvara;
sqrt_cond_var=sqrtwithina;
cond_var_adj=withinabiasadj;
sqrt_cond_var_adj=sqtwithinabiasadj;
namevaru=nnn;
keep  cond_var sqrt_cond_var  cond_var_adj sqrt_cond_var_adj namevaru;
run;
/*********************************************************/
  data g;run;
%macro pr;
%do i=1 %to &jj;
data tt;
set listofpartialws;
if row=&i;
run;
data uu;
set tt;
us=compress(t)||".";
call symput('hvar',t);
call symput('hhvar',us);
run;
data a;
set  partialp3&i; 
run;
data g;
set g a; 
%end;
%mend pr;
%pr;
run;
data u8;
set g;
if p3zk ne .;
cond_cat=p3zk;
sqrt_cond_cat=sqtp3zk;
sampsize=ns;
popsize=ps;
drop p3zk sqtp3zk ns ps ;
run;
data f;
%macro pr;
%do i=1 %to &allv;
 data tt;
set listofpartialbs;
if row=&i;
run;
data uu;
set tt;
us=compress(t)||".";
call symput('hvar',t);
call symput('hhvar',us);
run;
data aa;
set partialp2&i;
popsize=fbar&i;
avg_propensity_cat=mrphat&i;
avg_propensity=mrphatall;
uncond_cat=p1zk&i;
sqrt_uncond_cat=p2zk&i;
drop fbar&i mrphat&i p1zk&i p2zk&i mrphatall;
run;
data f;
set f aa;
%end;
%mend pr;
%pr;
run;
data u9;
set f;
if _type_ ne .;
drop _type_ _freq_;
run;
 data final1;
 set u6; set u3; 
 row+1;
 
data final3;
 set u7; set u4;
 row+1;
 data tem;
 merge final1 (in=v1) final3 (in=v2);
by row;
if v1;
run;
data ffinal1 ;
set tem;
cvu=sqrt_uncond_var/&htmean;
cvc=sqrt_cond_var/&htmean;
cvu_adj=sqrt_uncond_var_adj/&htmean;
cvc_adj=sqrt_cond_var_adj/&htmean;
avgpr=&htmean;
 variance_cvu=((se_uncond_var*se_uncond_var)/(avgpr*avgpr))+ ((sqrt_uncond_var*sqrt_uncond_var*&srvar*&srvar)/(&samsize*(avgpr**4)));
 std_cvu=sqrt(variance_cvu);
 variance_cvc=((se_cond_var*se_cond_var)/(avgpr*avgpr))+ ((sqrt_cond_var*sqrt_cond_var*&srvar*&srvar)/(&samsize*(avgpr**4)));
 std_cvc=sqrt(variance_cvc);
 
drop row avgpr variance_cvu variance_cvc;
run; 


 data final2;
 set u9; set u1;
 row+1;
 data final4;
 set u8; set u2;
 row+1;
data tem;
 merge final2 (in=v1) final4 (in=v2);
by row;
if v1;
run;
data ffinal2 ;
set tem;
cvu_category=sqrt_uncond_cat/&htmean;
cvc_category=sqrt_cond_cat/&htmean;
 avgpr=&htmean;
variance_cvu_category=((se_uncond_cat*se_uncond_cat)/(avgpr*avgpr))+ ((sqrt_uncond_cat*sqrt_uncond_cat*&srvar*&srvar)/(&samsize*(avgpr**4)));
 std_cvu_category=sqrt(variance_cvu_category);
 variance_cvc_category=((se_cond_cat*se_cond_cat)/(avgpr*avgpr))+ ((sqrt_cond_cat*sqrt_cond_cat*&srvar*&srvar)/(&samsize*(avgpr**4)));
 std_cvc_category=sqrt(variance_cvc_category);
drop row avgpr variance_cvu_category variance_cvc_category;
run; 
 
/***** final output file - names and directory of output can be changed here **********/
 data risq.final_output_ex2;
 set u5 ffinal1 ffinal2 ; 
 run;
 PROC EXPORT DATA= RISQ.FINAL_OUTPUT_EX2 
            OUTFILE= "&workloc.\RISQoutput_w11r_simopt&simopt._wave&wave._&period._&sample." 
            DBMS=XLSX REPLACE;
     PUTNAMES=YES;
RUN;

%logentry( fref = runtimes , msg = "Ending wave &wave., &sample. sample, period &period. ") ;

%mend risqprogram;

%macro callrisqprograms ;
	%do simopt = 1 %to 9 ;
		%do wave = &startwave %to &endwave ;
			%do s = 2 %to 3 ;
				%do p = 1 %to 3 ;
					%if &s = 1 %then %do ; %let sample=Overall ; %end ;
					%else %if &s = 2 %then %do ; %let sample=Main ; %end ;
					%else %if &s = 3 %then %do ; %let sample=TopUp ; %end ;
					%if &p = 1 %then %do ; %let period=P1 ; %end ;
					%else %if &p = 2 %then %do ; %let period=P2 ; %end ;
					%else %if &p = 3 %then %do ; %let period=P3 ; %end ;
					%risqprogram(&simopt,&wave,&sample,&period)
				%end ;
			%end ;
		%end ;
	%end ;


%mend callrisqprograms ;

%callrisqprograms 

* #times run = 9 * 5 * 3 * 3 ;
* revised #times run = 9 * 5 * 2 * 3 ;
* really only need #times run = 9 * 5 * 2 * 1 , but programs set up to look at P1 and P2 for each sample;

