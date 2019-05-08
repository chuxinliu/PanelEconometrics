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

********************************************************************************
sort state yr 
xtset state yr

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

ssc install sutex
*help sutex
sutex price pop pop16 cpi ndi sales pimin lnc lnclag lnp lny lnpn, minmax ///
	file("$root\Table1.tex") replace


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
eststo GMM1Step: xtabond2 lnc lnclag lnp lny lnpn yrd*, gmm(l.lnc) iv(lnp lny lnpn yrd*) noleveleq robust 

*(9) GMM-2-Step
eststo GMM2Step: xtabond2 lnc lnclag lnp lny lnpn yrd*, gmm(l.lnc) iv(lnp lny lnpn yrd*) noleveleq twostep 

esttab OLS Within TSLS TSLSKR Within2SLS FD2SLS FD2SLSKR GMM1Step GMM2Step ///
	using $root\Table2.rtf, replace se noobs compress nogaps label ///
	mtitle("OLS" "Within" "TSLS" "TSLSKR" "Within2SLS" "FD2SLS" "FD2SLSKR" "GMM1Step" "GMM2Step") ///
	title(Table 2: Replication of Table 8.1) ///
	keep(lnclag lnp lny lnpn) order(lnclag lnp lny lnpn) nonotes ///



/* Part 3.

qui: reg lnc lnclag lnpc lny lnpn
matrix b1 = e(b)
matrix v1 = e(V)
matrix se1 = (v1[1,1]^.5, v1[2,2]^.5, v1[3,3]^.5, v1[4,4]^.5)
qui: xtreg lnc lnclag lnpc lny lnpn y2-y30, fe
matrix b2 = e(b)
matrix v2 = e(V)
matrix se2 = (v2[1,1]^.5, v2[2,2]^.5, v2[3,3]^.5, v2[4,4]^.5)
qui: ivregress 2sls lnc lnpc lny lnpn (lnclag = L.lnpc L.lny L.lnpn)
matrix b3 = e(b)
matrix v3 = e(V)
matrix se3 = (v3[1,1]^.5, v3[2,2]^.5, v3[3,3]^.5, v3[4,4]^.5)
qui: xtkr lnc (L.lnc lnpc lny lnpn = d.l.lnc d.l(0/1).lnpc d.l(0/1).lny d.l(0/1).lnpn y2-y30)
matrix b4 = e(b)
matrix v4 = e(V)
matrix se4 = (v4[1,1]^.5, v4[2,2]^.5, v4[3,3]^.5, v4[4,4]^.5)
qui: xtivreg lnc lnpc lny lnpn y2-y30 (lnclag = L.lnpc L.lny L.lnpn), fe
matrix b5 = e(b)
matrix v5 = e(V)
matrix se5 = (v5[1,1]^.5, v5[2,2]^.5, v5[3,3]^.5, v5[4,4]^.5)
qui: xtivreg lnc lnpc lny lnpn y2-y30 (lnclag = L.lnpc L.lny L.lnpn), fd
matrix b6 = e(b)
matrix v6 = e(V)
matrix se6 = (v6[1,1]^.5, v6[2,2]^.5, v6[3,3]^.5, v6[4,4]^.5)
qui: xtkr d.lnc (d.l.lnc d.lnpc d.lny d.lnpn = l2.lnc l(1/2).lnpc l(1/2).lny l(1/2).lnpn y2-y30)
matrix b7 = e(b)
matrix v7 = e(V)
matrix se7 = (v7[1,1]^.5, v7[2,2]^.5, v7[3,3]^.5, v7[4,4]^.5)
qui: xtabond2 lnc l.lnc lnpc lny lnpn y2-y30 , gmm(l.lnc) iv(lnpc lny lnpn y2-y30) noleveleq robust /* iv() has the exog var. gmm() has the endog.*/
matrix b8 = e(b)
matrix v8 = e(V)
matrix se8 = (v8[1,1]^.5, v8[2,2]^.5, v8[3,3]^.5, v8[4,4]^.5)
qui: xtabond2 lnc l.lnc lnpc lny lnpn y2-y30 , gmm(l.lnc) iv(lnpc lny lnpn y2-y30) noleveleq twostep /* noleveleq indicates the use of dif. instead of system*/
matrix b9 = e(b)
matrix v9 = e(V)
matrix se9 = (v9[1,1]^.5, v9[2,2]^.5, v9[3,3]^.5, v9[4,4]^.5)

matrix part3 = (b1[1,1..4]\se1\b8[1,1..4]\se8\b9[1,1..4]\se9)
matrix colnames part3 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part3 = OLS "Std._Err." AB-1-step "Std._Err." AB-2-step "Std._Err."
outtable using hw4_part3, mat(part3) replace f(%15.2f) nobox caption("OLS and xtabond2 with all the years") clabel("hw4_part3")

* Part 4.
qui: xtreg lnc lnclag lnpc lny lnpn y2-y30, re
matrix b10 = e(b)
matrix v10 = e(V)
matrix se10 = (v10[1,1]^.5, v10[2,2]^.5, v10[3,3]^.5, v10[4,4]^.5)

matrix part4 = (b2[1,1..4]\se2\b10[1,1..4]\se10)
matrix colnames part4 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part4 = FE "Std._Err." RE "Std._Err." 
outtable using hw4_part4, mat(part4) replace f(%15.2f) nobox caption("FE vs RE with all the years") clabel("hw4_part4")

* Part 5.
qui: xtivreg lnc lnpc lny lnpn y2-y30 (lnclag = L.lnpc L.lny L.lnpn), re
matrix b11 = e(b)
matrix v11 = e(V)
matrix se11 = (v11[1,1]^.5, v11[2,2]^.5, v11[3,3]^.5, v11[4,4]^.5)

matrix part5 = (b3[1,1..4]\se3\b5[1,1..4]\se5\b11[1,1..4]\se11)
matrix colnames part5 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part5 = 2SLS "Std._Err." FE-2SLS "Std._Err." RE-2SLS "Std._Err."
outtable using hw4_part5, mat(part5) replace f(%15.2f) nobox caption("2SLS, FE-SLS and RE-2SLS with all the years") clabel("hw4_part5")

* Part 6.
qui: xtabond lnc lnpc lny lnpn y2-y30, vce(robust) twostep
matrix b12 = e(b)
matrix v12 = e(V)
matrix se12 = (v12[1,1]^.5, v12[2,2]^.5, v12[3,3]^.5, v12[4,4]^.5)
qui: xtdpdsys lnc lnpc lny lnpn y2-y30, lags(1) twostep vce(robust)
matrix b13 = e(b)
matrix v13 = e(V)
matrix se13 = (v13[1,1]^.5, v13[2,2]^.5, v13[3,3]^.5, v13[4,4]^.5)
matrix part6 = (b12[1,1..4]\se12\b13[1,1..4]\se13\b2[1,1..4]\se2\b10[1,1..4]\se10)
matrix colnames part6 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part6 = xtabond "Std._Err." xtdpdsys "Std._Err." FE "Std._Err." RE "Std._Err."
outtable using hw4_part6, mat(part6) replace f(%15.2f) nobox caption("xtabond2, xtdpdsys and FE/RE with all the years") clabel("hw4_part6")

* Part 7.
qui: xtdpd l(0/1).lnc lnpc lny lnpn y2-y30, dgmmiv(lnc) div(lnpc lny lnpn y2-y30) twostep vce(robust)
matrix b14 = e(b)
matrix v14 = e(V)
matrix se14 = (v14[1,1]^.5, v14[2,2]^.5, v14[3,3]^.5, v14[4,4]^.5)

matrix part7 = (b12[1,1..4]\se12\b14[1,1..4]\se14)
matrix colnames part7 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part7 = xtabond "Std._Err." xtdpd "Std._Err." 
outtable using hw4_part7, mat(part7) replace f(%15.2f) nobox caption("xtabond vs xtdpd with all the years") clabel("hw4_part7")

* Part 8.
qui: xtdpd l(0/1).lnc lnpc lny lnpn y2-y30, dgmmiv(lnpc lny lnpn,lag(1 .)) div(y2-y30) twostep vce(robust)
matrix b15 = e(b)
matrix v15 = e(V)
matrix se15 = (v15[1,1]^.5, v15[2,2]^.5, v15[3,3]^.5, v15[4,4]^.5)
qui: xtdpd l(0/1).lnc lnpc lny lnpn y2-y30, dgmmiv(lnpc lny lnpn,lag(1 1)) div(y2-y30) twostep vce(robust)
matrix b16 = e(b)
matrix v16 = e(V)
matrix se16 = (v16[1,1]^.5, v16[2,2]^.5, v16[3,3]^.5, v16[4,4]^.5)

matrix part8 = (b15[1,1..4]\se15\b16[1,1..4]\se16)
matrix colnames part8 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part8 = AB-option1 "Std._Err." AB-option2 "Std._Err." 
outtable using hw4_part8, mat(part8) replace f(%15.2f) nobox caption("xtdpd using two different options with all the years") clabel("hw4_part8")

* Part 9.
qui: xtdpd l(0/1).lnc lnpc lny lnpn y2-y30, dgmmiv(lnc lnpc lny lnpn,lag(1 .)) div(y2-y30) twostep vce(robust)
matrix b17 = e(b)
matrix v17 = e(V)
matrix se17 = (v17[1,1]^.5, v17[2,2]^.5, v17[3,3]^.5, v17[4,4]^.5)
qui: xtdpd l(0/1).lnc lnpc lny lnpn y2-y30, dgmmiv(lnc lnpc lny lnpn,lag(1 1)) div(y2-y30) twostep vce(robust)
matrix b18 = e(b)
matrix v18 = e(V)
matrix se18 = (v18[1,1]^.5, v18[2,2]^.5, v18[3,3]^.5, v18[4,4]^.5)

matrix part9 = (b17[1,1..4]\se17\b18[1,1..4]\se18)
matrix colnames part9 = "lnC_{i,t-1}" "lnP_{it}" "lnPn_{it}" "lnY_{it}"
matrix rownames part9 = AB-option1 "Std._Err." AB-option2 "Std._Err." 
outtable using hw4_part9, mat(part9) replace f(%15.2f) nobox caption("xtdpd using two different options with all the years") clabel("hw4_part9")



