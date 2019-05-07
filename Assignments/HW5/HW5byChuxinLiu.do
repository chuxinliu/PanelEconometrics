// Panel Econometrics     //
// Assignment #5          //
// Author: Chuxin Liu     //
// Last Updated: 05042019 //

set more off
clear
capture log close

global root="C:\Users\cliu4\Documents\GitHub\PanelEconometrics\Assignments\HW5"
use "$root\Cigar.dta", clear

********************************************************************************

tsset state yr

*Question 1

sort state yr 
by state: gen clag = sales[_n-1]

gen lnc=ln(sales)
gen lnclag=ln(clag)
gen lnp=ln(price)
gen lny=ln(ndi)
gen lnpn=ln(pimin)

eststo clear
eststo summstats: estpost summarize state yr price pop pop16 cpi ndi ///
	sales lnc clag lnclag lnp lny pimin lnpn
esttab summstats using $root\Table1.rtf, replace main(mean %6.2f) aux(sd) ///
	cell("mean sd") nomtitles title(Table 1: Summary Statistics)

*Question 2

*(1) OLS 
eststo clear
eststo OLS: reg lnc lnclag lnp lny lnpn

*(2) Within
eststo Within: xtreg lnc lnclag lnp lny lnpn, fe

*(3) 2SLS 
eststo 2SLS: xthtaylor lwage wks south smsa ms exp exp2 occ ind union fem blk ed, ///
	endog(exp exp2 wks ms union ed)
* Hausman test: within and Hausman-Taylor
hausman within ht, df(3)
local hausman_chi2_df=r(df)
local hausman_chi2=round(r(chi2),.01)
estadd local Hausman "chi2(`hausman_chi2_df')=`hausman_chi2'"
est store ht

*(4) AM
eststo am: xthtaylor lwage wks south smsa ms exp exp2 occ ind union fem blk ed, ///
	endog(exp exp2 wks ms union ed) amacurdy
* Hausman test: Hausman-Taylor and AM
hausman ht am, df(13)
local hausman_chi2_df=r(df)
local hausman_chi2=round(r(chi2),.01)
estadd local Hausman "chi2(`hausman_chi2_df')=`hausman_chi2'"
est store am

esttab gls within ht am using $root\Table2.rtf, replace se noobs compress nogaps label ///
	mtitle("GLS" "Within" "HT" "AM") title(Table 2: Replication of Table 7.4) ///
	order(_cons wks south smsa ms exp exp2 occ ind union fem blk ed) nonotes ///
	addnotes("*X2(exp exp2 wks ms union), Z1(fem blk)" "Source: Baltagi and Khanti-Akom (1990)") ///
	scalars(Hausman)

* Question 4

hausman within am, df(27)
hausman gls am

