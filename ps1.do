** Panel Econometrics       **
** Assignment 1             **
** Author: Chuxin Liu       **
** Last Updated: 02/28/2019 **

clear
set more off
capture: log close
cd "C:\Users\chuxi\Documents\GitHub\PanelEconometrics"
use "gasoline.dta", clear

*******************************************************************************
* Question 1
encode country, generate(ncountry)
* a) Gasoline Demand Data. One-way Error Component Results
tsset ncountry year /* declare panel data*/
matrix results = J(14,3,.)

* OLS
eststo clear
eststo OLS: quietly reg lgaspcar lincomep lrpmg lcarpcap
* Between
eststo Between: quietly xtreg lgaspcar lincomep lrpmg lcarpcap, be
* Within
eststo Within: quietly xtreg lgaspcar lincomep lrpmg lcarpcap, fe
* WALHUS
eststo Walhus: quietly spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtwh)
* AMEMIYA
eststo Amemiya: quietly spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtam)
* SWAR (Swamy-Arora)
eststo Swar: quietly spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtsa)
* IMLE
eststo IMLE: quietly spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtmlem)

esttab using table1.csv, label title(Replicating Table 2.5) mtitles("OLS" "Between" "Within" "WALHUS" "AMEMIYA" "SWAR" "IMLE") replace 

* Question 2)
preserve
tsset ncountry year
reg lgaspcar lincomep lrpmg lcarpcap i.ncountry
collapse lgaspcar lincomep lrpmg lcarpcap, by(ncountry)
gen mu_i = lgaspcar-_b[lincomep]-_b[lrpmg]-_b[lcarpcap]
graph twoway (lfit mu_i lincomep) (scatter mu_i lincomep)
graph export scatter_graph.png, replace
corr mu_i lincomep lrpmg lcarpcap
pwcorr mu_i lincomep lrpmg lcarpcap
esttab using corr.csv
restore

/*
matrix fe = e(b)'
matrix fixedeffects = fe[4..21,1]
matrix cons = J(18,1,_b[_cons])
matrix ffs = fixedeffects + cons
*/
