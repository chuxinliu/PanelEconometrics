// Panel Econometrics     //
// Assignment #5          //
// Author: Chuxin Liu     //
// Last Updated: 05082019 //

set more off
clear
capture log close

global root="C:\Users\cl3852\Documents\GitHub\PanelEconometrics\Assignments\HW5"
log using "$root\HW5.log",replace
use "$root\Cigar.dta", clear

sort state yr
xtset state yr

********************************************************************************
*Question 1
*Variables in the regressions are in real term: adjusted for inflation using CPI

g c=sales*100/cpi
sort state yr
by state: gen clag = c[_n-1]
g lnc=ln(c)
g lnclag=ln(clag)
g y=ndi*100/cpi
g lny=ln(y)
g p=price*100/cpi
g lnp=ln(p)
g pn=pimin*100/cpi
g lnpn=ln(pn)

eststo clear
eststo sumstats: estpost summarize price pop pop16 cpi ndi sales pimin ///
	lnc lnclag lnp lny lnpn
esttab sumstats using $root\Table1.rtf, replace main(mean %6.2f) aux(sd) ///
	cell("mean sd") nomtitles title(Table 1: Summary Statistics) ///
	addnotes("Created variables: lnc lnclag lnp lny lnpn")

********************************************************************************
*Question 2
*(1) OLS
eststo clear
eststo OLS: reg lnc lnclag lnp lny lnpn
*(2) Within
tab yr, g(yrd)
eststo Within: xtreg lnc lnclag lnp lny lnpn yrd*, fe
*(3) 2SLS
eststo TSLS: xtivreg lnc lnp lny lnpn (lnclag = L.lnp L.lny L.lnpn)
*(4) 2SLS-KR
ssc install xtkr
eststo TSLSKR: xtkr lnc (lnclag lnp lny lnpn = d.l.lnc d.l(0/1).lnp d.l(0/1).lny d.l(0/1).lnpn yrd*)
*(5) Within-2SLS
eststo Within2SLS: xtivreg lnc lnp lny lnpn yrd* (lnclag = L.lnp L.lny L.lnpn), fe
*(6) FD-2SLS
eststo FD2SLS: xtivreg lnc lnp lny lnpn yrd* (lnclag = L.lnp L.lny L.lnpn), fd
*(7) FD-2SLS-KR
eststo FD2SLSKR: xtkr d.lnc (d.l.lnc d.lnp d.lny d.lnpn = l2.lnc l(1/2).lnp l(1/2).lny l(1/2).lnpn yrd*)
*(8) GMM-1-Step
ssc install xtabond2
eststo GMM1Step: xtabond2 lnc lnclag lnp lny lnpn yrd*, gmm(lnclag) iv(lnp lny lnpn yrd*) noleveleq robust
*(9) GMM-2-Step
eststo GMM2Step: xtabond2 lnc lnclag lnp lny lnpn yrd*, gmm(lnclag) iv(lnp lny lnpn yrd*) noleveleq twostep

esttab OLS Within TSLS TSLSKR Within2SLS ///
	using $root\Table21.rtf, replace se noobs compress nogaps label ///
	mtitle("OLS" "Within" "2SLS" "2SLS-KR" "Within-2SLS") ///
	title(Table 2.1: Replication of Table 8.1 (1)) ///
	keep(lnclag lnp lny lnpn) order(lnclag lnp lny lnpn) nonotes ///

esttab FD2SLS FD2SLSKR GMM1Step GMM2Step ///
	using $root\Table22.rtf, replace se noobs compress nogaps label ///
	mtitle("FD-2SLS" "FD-2SLS-KR" "GMM-1-Step" "GMM-2-Step") ///
	title(Table 2.2: Replication of Table 8.1 (2)) ///
	order(lnclag lnp lny lnpn D.lnclag LD.lnc D.lnp D.lny D.lnpn) nonotes ///
	keep(lnclag lnp lny lnpn D.lnclag LD.lnc D.lnp D.lny D.lnpn)

********************************************************************************
*Question 3: OLS vs. xtabond2
*OLS
eststo clear
eststo OLS: reg lnc lnclag lnp lny lnpn yrd*
*GMM-1-Step
eststo GMM1Step: xtabond2 lnc lnclag lnp lny lnpn yrd*, gmm(lnclag) iv(lnp lny lnpn yrd*) noleveleq robust
*GMM-2-Step
eststo GMM2Step: xtabond2 lnc lnclag lnp lny lnpn yrd*, gmm(lnclag) iv(lnp lny lnpn yrd*) noleveleq twostep
esttab OLS GMM1Step GMM2Step ///
	using $root\Table3.rtf, replace se noobs compress nogaps label ///
	mtitle("OLS" "GMM1Step" "GMM2Step") title(Table 3: OLS vs. xtabond2) ///
	keep(lnclag lnp lny lnpn) order(lnclag lnp lny lnpn) nonotes

********************************************************************************
*Question 4: FE vs. RE
*FE
eststo FE: xtreg lnc lnclag lnp lny lnpn yrd*, fe
eststo RE: xtreg lnc lnclag lnp lny lnpn yrd*, re
esttab FE RE ///
	using $root\Table4.rtf, replace se noobs compress nogaps label ///
	mtitle("FE" "RE") title(Table 4: FE vs. RE) ///
	keep(lnclag lnp lny lnpn) order(lnclag lnp lny lnpn) nonotes

********************************************************************************
*Question 5: 2SLS (ivregress), FE-2SLS and RE-2SLS (xtivreg) vs. Q3 and Q4
*2SLS (ivregress)
eststo TSLS: ivregress 2sls lnc lnp lny lnpn yrd* (lnclag = L.lnp L.lny L.lnpn)
*FE-2SLS (xtivreg)
eststo FE2SLS: xtivreg lnc lnp lny lnpn yrd* (lnclag = L.lnp L.lny L.lnpn), fe
*RE-2SLS (xtivreg)
eststo RE2SLS: xtivreg lnc lnp lny lnpn yrd* (lnclag = L.lnp L.lny L.lnpn), re
esttab TSLS FE2SLS RE2SLS ///
	using $root\Table5.rtf, replace se noobs compress nogaps label ///
	mtitle("2SLS" "FE-2SLS" "RE-2SLS") ///
	title(Table 5: 2SLS (ivregress), FE-2SLS and RE-2SLS (xtivreg)) ///
	keep(lnclag lnp lny lnpn) order(lnclag lnp lny lnpn) nonotes

********************************************************************************
*Question 6: xtdpdsys vs. xtabond, comparing to FE and RE?
* Part 6.
eststo xtdpdsys: xtdpdsys lnc lnp lny lnpn yrd*, lags(1) twostep vce(robust)
esttab xtdpdsys GMM1Step GMM2Step FE RE ///
	using $root\Table6.rtf, replace se noobs compress nogaps label ///
	mtitle("xtdpdsys" "GMM-1-Step" "GMM-2-Step" "FE" "RE") ///
	title(Table 6: xtdpdsys vs. xtabond, comparing to FE and RE) ///
	keep(lnclag L.lnc lnp lny lnpn) order(lnclag L.lnc lnp lny lnpn) nonotes

********************************************************************************
*Question 7: use xtdpd to replicate xtabond
eststo xtdpd: xtdpd l(0/1).lnc lnp lny lnpn yrd*, dgmmiv(lnc) div(lnp lny lnpn yrd*) twostep vce(robust)
esttab xtdpd GMM2Step ///
	using $root\Table7.rtf, replace se noobs compress nogaps label ///
	mtitle("xtdpd" "GMM2Step") title(Table 7: Use xtdpd to replicate xtabond) ///
	keep(lnclag L.lnc lnp lny lnpn) order(lnclag L.lnc lnp lny lnpn) nonotes

********************************************************************************
*Question 8: dgmmiv(lnp lny lnpn, lag(1.)) and dgmmiv(lnp lny lnpn, lag(1.1))
eststo ABOption1: xtdpd l(0/1).lnc lnp lny lnpn yrd*, dgmmiv(lnp lny lnpn,lag(1 .)) div(yrd*) twostep vce(robust)
eststo ABOption2: xtdpd l(0/1).lnc lnp lny lnpn yrd*, dgmmiv(lnp lny lnpn,lag(1 1)) div(yrd*) twostep vce(robust)
esttab ABOption1 ABOption2 ///
	using $root\Table8.rtf, replace se noobs compress nogaps label ///
	mtitle("AB-Option1" "AB-Option2") ///
	title(Table 8: dgmmiv(lnp lny lnpn,lag(1 .)) and dgmmiv(lnp lny lnpn,lag(1 1))) ///
	keep(L.lnc lnp lny lnpn) order(L.lnc lnp lny lnpn) nonotes

********************************************************************************
*Question 9: lgmmiv(lnc) and lgmmiv(lnc lnp lny lnpn)
eststo ABOption3:  xtdpd l(0/1).lnc lnp lny lnpn yrd*, dgmmiv(lnc lnp lny lnpn,lag(1 .)) div(yrd*) twostep vce(robust)
eststo ABOption4:  xtdpd l(0/1).lnc lnp lny lnpn yrd*, dgmmiv(lnc lnp lny lnpn,lag(1 1)) div(yrd*) twostep vce(robust)
esttab ABOption3 ABOption4 ///
	using $root\Table9.rtf, replace se noobs compress nogaps label ///
	mtitle("AB-Option3" "AB-Option4") ///
	title(Table 9: lgmmiv(lnc) and lgmmiv(lnc lnp lny lnpn)) ///
	keep(L.lnc lnp lny lnpn) order(L.lnc lnp lny lnpn) nonotes
