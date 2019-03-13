** Panel Econometrics       **
** Assignment 2             **
** Author: Chuxin Liu       **
** Last Updated: 03/13/2019 **

clear
set more off
capture: log close
cd "C:\Users\cliu\Documents\GitHub\PanelEconometrics\HW2"
log using "HW2_log", text replace
use "gasoline.dta", clear


******************************** Data Cleaning *********************************
encode country, generate(ncountry)
tsset ncountry year
gen iota = 1

*******************************************************************************
* Question 1
* (a) FE, country effects
xtreg lgaspcar lincomep lrpmg lcarpcap, fe
reg lgaspcar lincomep lrpmg lcarpcap  i.ncountry

matrix b = e(b)'
matrix list b
matrix bhat = b[5..21,1..1]
matrix list bhat
matrix V = e(V)
matrix list V
matrix Vhat = V[5..21,5..21]
matrix list Vhat

mata: I_N = I(17)
mata: st_matrix("r(I_N)",I_N)

matrix R = r(I_N)
matrix list R
matrix q = J(17,1,0)
matrix list q

matrix mhat = R*bhat-q
matrix list mhat
matrix covmatrixm = R*Vhat*R'
matrix list covmatrixm

local J = 17
matrix F = mhat'*invsym(covmatrixm)*mhat/`J' 
matrix list F

* b) FE, time effects
xtreg lgaspcar lincomep lrpmg lcarpcap i.year, fe
testparm i.year
xtreg lgaspcar lincomep lrpmg lcarpcap i.year, fe

matrix b = e(b)'
matrix list b
matrix V = e(V)
matrix list V
matrix bhat = b[5..22,1..1]
matrix list bhat
matrix Vhat = V[5..22,5..22]
matrix list Vhat

mata: I_N = I(18)
mata: st_matrix("r(I_N)",I_N)

matrix R = r(I_N)
matrix list R
matrix q = J(18,1,0)
matrix list q
matrix mhat = R*bhat-q
matrix list mhat
matrix covmatrixm = R*Vhat*R'
matrix list covmatrixm

local J = 18
matrix F = mhat'*invsym(covmatrixm)*mhat/`J'
matrix list F

* c) RE, country effects
xtreg lgaspcar lincomep lrpmg lcarpcap, mle
ereturn list
local ll_u = e(ll)
reg lgaspcar lincomep lrpmg lcarpcap
ereturn list
local ll_r = e(ll)
local LR = -2*(`ll_r'-`ll_u')
di `LR'

xtreg lgaspcar lincomep lrpmg lcarpcap, re
ereturn list
local n = e(N)
local N = e(N_g)
local T = e(g_max)
local K = e(df_m)
local p = `n'-(`K'+1)
* Breusch and Pagan LM test for random effects
xttest0

* Alternatively:
reg lgaspcar lincomep lrpmg lcarpcap
predict u_tilda, residuals
mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
J_T = J(`T',`T',1)
LM_1 = (`N'*`T')/(2*(`T'-1))*(1-(u'*(I_N#J_T)*u)/uu)^2
st_numscalar("r(LM_1)",LM_1)
end
local LM_1 = r(LM_1)
display "LM test of sigma^2_mu=0: `LM_1'"

mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
J_T = J(`T',`T',1)
HO = (((`N'*`T')/(2*(`T'-1)))^.5)*((u'*(I_N#J_T)*u)/uu-1)
st_numscalar("r(HO)",HO)
end
local HO = r(HO)
display "Honda test of sigma^2_mu=0: `HO'"

mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
I_n = I(`n')
J_T = J(`T',`T',1)
D = I_N#J_T
d = (u'*D*u)/uu
Z = st_data(.,("iota","lincomep","lrpmg","lcarpcap"))
ZZ = cross(Z,Z)
inv_ZZ = invsym(ZZ)
P_z = I_n-Z*inv_ZZ*Z'
E_d = trace(D*P_z)/`p'
var_d = 2*(`p'*(trace(D*P_z))^2-(trace(D*P_z))^2)/(`p'^2*(`p'+2))
SLM = (d-E_d)/(var_d^.5)
st_numscalar("r(SLM)",SLM)
end
local SLM = r(SLM)
display "SLM test of sigma^2_mu=0: `SLM'"

mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
J_T = J(`T',`T',1)
A = (((`N'*`T')/(2*(`T'-1)))^.5)*((u'*(I_N#J_T)*u)/uu-1)
if (A>0){ 
X_m = A^2
}
else {
X_m = 0
}
st_numscalar("r(X_m)",X_m)
end
local X_m = r(X_m)
display "GHM test of sigma^2_mu=0: `HO'"

* d) RE, country or time effects
mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
J_T = J(`T',`T',1)
I_T = I(`T')
J_N = J(`N',`N',1)
LM_1 = (`N'*`T')/(2*(`T'-1))*(1-(u'*(I_N#J_T)*u)/uu)^2
LM_2 = (`N'*`T')/(2*(`N'-1))*(1-(u'*(J_N#I_T)*u)/uu)^2
LM = LM_1+LM_2
st_numscalar("r(LM)",LM)
end
local LM = r(LM)
display "LM test of sigma^2_mu=0 & sigma^2_lambda=0: `LM'"

mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
J_T = J(`T',`T',1)
A = (((`N'*`T')/(2*(`T'-1)))^.5)*((u'*(I_N#J_T)*u)/uu-1)
I_T = I(`T')
J_N = J(`N',`N',1)
B = (((`N'*`T')/(2*(`N'-1)))^.5)*((u'*(J_N#I_T)*u)/uu-1)
H0 = (A+B)/(2^.5)
st_numscalar("r(HO)",HO)
end
local HO = r(HO)
display "Honda ``handy'' one-sided test of sigma^2_mu=0 & sigma^2_lambda=0: `HO'"

mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
I_n = I(`n')
J_T = J(`T',`T',1)
J_N = J(`N',`N',1)
I_T = I(`T')
D = 0.5*((`N'*`T'/(`T'-1))^.5)*(I_N#J_T)+.5*((`N'*`T'/(`N'-1))^.5)*(J_N#I_T)
d = (u'*D*u)/uu
Z = st_data(.,("iota","lincomep","lrpmg","lcarpcap"))
ZZ = cross(Z,Z)
inv_ZZ = invsym(ZZ)
P_z = I_n-Z*inv_ZZ*Z'
E_d = trace(D*P_z)/`p'
var_d = 2*(`p'*trace(D*P_z)^2-(trace(D*P_z))^2)/(`p'^2*(`p'+2))
SLM = (d-E_d)/(var_d^.5)
st_numscalar("r(SLM)",SLM)
end
local SLM = r(SLM)
display "SLM test of sigma^2_mu=0 & sigma^2_lambda=0: `SLM'"

mata
u = st_data(.,"u_tilda")
uu = cross(u,u)
I_N = I(`N')
J_T = J(`T',`T',1)
A = ((`N'*`T')/(2*(`T'-1)))^.5*((u'*(I_N#J_T)*u)/uu-1)
I_T = I(`T')
J_N = J(`N',`N',1)
B = ((`N'*`T')/(2*(`N'-1)))^.5*((u'*(J_N#I_T)*u)/uu-1)
if (A>0 & B>0){ 
X_m = A^2+B^2
}
else if (A>0 & B<=0) {
X_m = A^2
}
else if (A<=0 & B>0) {
X_m = B^2
}
else {
X_m = 0
}
st_numscalar("r(X_m)",X_m)
end
local X_m = r(X_m)
display "GHM test of sigma^2_mu=0 & sigma^2_lambda=0: `HO'"



********************************************************************************
* Question 2 
* a) Hausman Test: RE vs FE (1st version of Hausman Test)
xtreg lgaspcar lincomep lrpmg lcarpcap, fe 
matrix b = e(b)'
matrix V_b = e(V)
estimates store FE
xtreg lgaspcar lincomep lrpmg lcarpcap, re 
matrix B = e(b)'
matrix V_B = e(V)
estimates store RE
hausman FE RE

* b) Hausman Test: GLS vs OLS (4th version of Hausman Test)
xtreg lgaspcar lincomep lrpmg lcarpcap, re 
matrix b = e(b)'
matrix list b
matrix V_b = e(V)
matrix list V_b

reg lgaspcar lincomep lrpmg lcarpcap
matrix B = e(b)'
matrix list B
matrix V_B = e(V)
matrix list V_B

matrix bhat = b[1..3,1..1]
matrix list bhat
matrix V_bhat = V_b[1..3,1..3]
matrix list V_bhat
matrix Bhat = B[1..3,1..1]
matrix list Bhat
matrix V_Bhat = V_B[1..3,1..3]
matrix list V_Bhat

matrix inv_V_b_V_B = invsym(V_bhat-V_Bhat)
matrix list inv_V_b_V_B
matrix q4 = bhat-Bhat
matrix list q4
matrix hausman = q4'*inv_V_b_V_B*q4
matrix list hausman


********************************************************************************
* Question 3
* a) Likelihood Ratio Test: RE vs FE 
xtreg lgaspcar lincomep lrpmg lcarpcap, re
xtoverid

* b) Arellano (1993) Test: RE vs FE 
foreach x in lgaspcar lincomep lrpmg lcarpcap {
bysort ncountry: egen `x'_sum = sum(`x')
bysort ncountry: egen `x'_bar = mean(`x')
bysort ncountry: gen `x'_sumbackward = sum(`x')
bysort ncountry: gen `x'_sumforward = `x'_sum-`x'_sumbackward
bysort ncountry: gen `x'_plus = (((_N-_n)/(_N-_n+1))^.5)*(`x'-`x'_sumforward/(_N-_n))
bysort ncountry: replace `x'_plus = `x'_bar if _n==_N
gen `x'_add = 0
bysort ncountry: replace `x'_add = `x'_bar if _n==_N
}
reg lgaspcar_plus lincomep_plus lrpmg_plus lcarpcap_plus lincomep_add lrpmg_add lcarpcap_add, vce(robust)
matrix b = e(b)'
matrix V = e(V)
matrix bhat = b[4..6,1..1]
matrix Vhat = V[4..6,4..6]
mata: I_N = I(3)
mata: st_matrix("r(I_N)",I_N)
matrix R = r(I_N)
matrix q = J(3,1,0)
matrix mhat = R*bhat-q
matrix covmatrixm = R*Vhat*R'
local J = 3
matrix F = mhat'*invsym(covmatrixm)*mhat/`J'
matrix list F
