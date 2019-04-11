// Panel Econometrics     //
// Assignment #3          //
// Author: Chuxin Liu     //
// Last Updated: 04102019 //

set more off
clear
capture log close

global root="C:\Users\cliu4\Documents\GitHub\PanelEconometrics\Assignments\HW3"
use "$root\gasoline.dta", clear

encode country, g(ncountry)
tsset ncountry year

*Question 1
eststo clear
eststo: quietly xtreg lgaspcar lincomep lrpmg lcarpcap, re
*Auto-Relation Model: exregar
eststo: quietly xtregar lgaspcar lincomep lrpmg lcarpcap, re rhotype(tscorr)
eststo: quietly xtregar lgaspcar lincomep lrpmg lcarpcap, re rhotype(freg)
esttab using "$root\table1.csv", label replace ///
	title(Table 1: Question 1 Regression Results) ///
	mtitles("RE" "RE TSCORR" "RE FREG") sca(sigma_u sigma_e rho_ar rho_fov)


*Question 2
*(a): Linear models with interactive fixed effects
eststo clear
eststo: regife lgaspcar lincomep lrpmg lcarpcap, ///
	factors(timefactor=year idfactor=ncountry, 2)
esttab using "$root\table2.csv", label replace ///
	title(Table 2: Bai's Estimate) mtitles("Bai")
*(b)
matrix var=e(V)
matrix list var
*(c)
preserve
collapse timefactor1 timefactor2, by(year)
list year timefactor1 timefactor2
restore
*(d)
preserve
collapse idfactor1 idfactor2, by(country)
list country idfactor1 idfactor2
restore


*Question 3
use "$root\gasoline.dta", clear
encode country, g(ncountry)
tsset ncountry year
regife lgaspcar lincomep lrpmg lcarpcap, factors(timefactor=year idfactor=ncountry, 2)
est sto m1
drop *factor*
regife lgaspcar lincomep lrpmg lcarpcap, ///
	a(id = ncountry time = year) ///
	f(timefactor = year idfactor = ncountry , 2)
est sto m2
hausman m2 m1

*Question 4
eststo clear
eststo: xtmg lgaspcar lincomep lrpmg lcarpcap
ereturn list
esttab using "$root\table4.csv", label replace ///
	title(Table 4: Question 4 MG Esimator) ///
	mtitles("MG")
matrix var=e(V)
matrix list var








