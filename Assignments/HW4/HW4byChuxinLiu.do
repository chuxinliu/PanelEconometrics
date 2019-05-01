// Panel Econometrics     //
// Assignment #4          //
// Author: Chuxin Liu     //
// Last Updated: 05012019 //

set more off
clear
capture log close

global root="C:\Users\cliu4\Documents\GitHub\PanelEconometrics\Assignments\HW4"
use "$root\wages.dta", clear

********************************************************************************

gen i=_n
gen id=.
gen year=.
replace id=ceil(i/7-0.01)
gen diff = 7*(i/7-floor(i/7))+0.01
replace year=1976 if diff>1 & diff<2 
replace year=1977 if diff>2 & diff<3 
replace year=1978 if diff>3 & diff<4 
replace year=1979 if diff>4 & diff<5 
replace year=1980 if diff>5 & diff<6 
replace year=1981 if diff>6 & diff<7 
replace year=1982 if diff>0 & diff<1 
drop i diff
tsset id year

*Question 1

eststo clear
eststo summstats: estpost summarize exp wks occ ind south smsa ms fem union ed blk lwage
esttab summstats using $root\Table1.rtf, replace main(mean %6.2f) aux(sd) ///
	cell("mean sd") mtitle("Total sample") title(Table 1: Summary Statistics)

	
*Question 2

gen exp2=exp*exp

local X2 = "occ south smsa ind"
local Z1 = "fem blk"

*(1) GLS
set matsize 596
eststo clear
eststo gls: xtgls lwage wks south smsa ms exp exp2 occ ind union fem blk ed

*(2) Within
eststo within: xtreg lwage wks south smsa ms exp exp2 occ ind union fem blk ed, fe
hausman gls within
estadd scalar hausman_chi2_df=r(df)
estadd scalar hausman_chi2=r(chi2)
est store within

*(3) HT
eststo ht: xthtaylor lwage wks south smsa ms exp exp2 occ ind union fem blk ed, ///
	endog(exp exp2 wks ms union ed)
hausman gls ht
estadd scalar hausman_chi2_df=r(df)
estadd scalar hausman_chi2=r(chi2)
est store ht

*(4) AM
eststo am: xthtaylor lwage wks south smsa ms exp exp2 occ ind union fem blk ed, ///
	endog(exp exp2 wks ms union ed) amacurdy
hausman gls am
estadd scalar hausman_chi2_df=r(df)
estadd scalar hausman_chi2=r(chi2)
est store am

esttab gls within ht am using $root\Table2.rtf, replace noobs compress nogaps label ///
	mtitle("GLS" "Within" "HT" "AM") title(Table 2: Replication of Table 7.4) ///
	order(_cons wks south smsa ms exp exp2 occ ind union fem blk ed) ///
	nonotes addnotes("*X2(exp exp2 wks ms union), Z1(fem blk)" "Source: Baltagi and Khanti-Akom (1990)") ///
	scalars(hausman_chi2_df hausman_chi2)



