** Panel Econometrics       **
** Assignment 1             **
** Author: Chuxin Liu       **
** Last Updated: 03/06/2019 **

clear
set more off
capture: log close
cd "C:\Users\cliu\Documents\GitHub\PanelEconometrics\HW1"
use "gasoline.dta", clear

*******************************************************************************
* Question 1
encode country, generate(ncountry)
* a) Gasoline Demand Data. One-way Error Component Results
tsset ncountry year /* declare panel data*/
matrix results = J(14,3,.)

eststo clear
* OLS
eststo OLS: reg lgaspcar lincomep lrpmg lcarpcap
* Between
eststo Between: xtreg lgaspcar lincomep lrpmg lcarpcap, be
* Within
eststo Within: xtreg lgaspcar lincomep lrpmg lcarpcap, fe
* WALHUS
/* ssc install spregxt */
/* check if spregxt is installed */
/* nc(#): Number of Cross Sections Units */
/* model(ols): Linear Panel Models (Non Spatial) */
/* run(xtwh): [NEW] Wallace-Hussain Random-Effects Panel Regression */
eststo Walhus: spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtwh)
* AMEMIYA
/* run(xtam): [NEW] Amemiya Random-Effects Panel Regression */
eststo Amemiya: spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtam)
* SWAR (Swamy-Arora)
/* run(xtsa): [NEW] Swamy-Arora Random-Effects Panel Regression */
eststo Swar: spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtsa)
* IMLE
/* run(xtmlem): [NEW] Trevor Breusch MLE Random-Effects Panel Regression */
eststo IMLE: spregxt lgaspcar lincomep lrpmg lcarpcap, nc(18) model(ols) run(xtmlem)

esttab using Table1.csv, label se noobs nocons title(Replicating Table 2.5) mtitles("OLS" "Between" "Within" "WALHUS" "AMEMIYA" "SWAR" "IMLE") replace 

* Question 2
tsset ncountry year
xtreg lgaspcar lincomep lrpmg lcarpcap, fe
collapse lgaspcar lincomep lrpmg lcarpcap, by(ncountry)
*(a)
gen mu_i = lgaspcar-_b[lincomep]*lincomep-_b[lrpmg]*lrpmg-_b[lcarpcap]*lcarpcap-_b[_cons]
*(b)
twoway (lfit mu_i lincomep) (scatter mu_i lincomep, mlabel(ncountry) mlabsize(vsmall) mlabposition(5)), ytitle(Fixed Effect) xtitle(Average Per Capita Income) title(Figure 1: Fixed Effect and Average Per Capita Income) legend(off)
graph export Figure1.png, replace
*(c)
corr mu_i lincomep lrpmg lcarpcap

