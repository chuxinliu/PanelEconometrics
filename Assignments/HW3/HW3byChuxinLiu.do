// Panel Econometrics     //
// Assignment #3          //
// Author: Chuxin Liu     //
// Last Updated: 04102019 //

set more off
clear
capture log close

global root="C:\Users\cl3852\Documents\GitHub\PanelEconometrics\Assignments\HW3"
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
regife lngascar lnyn lnpmgpgdp lncarn, f(fyear=year1 fid=country1, 2)
*(b)
matrix var=e(V)
matrix list var
*(c)
preserve
collapse fyear1 fyear2, by(YR1)
list YR1 fyear1 fyear2
scatter fyear1 fyear2 YR1
restore
*(d)
preserve
regife lngascar lnyn lnpmgpgdp lncarn, f(fyear=year1 fid=country1, 2)
collapse fid1 fid2, by(country1)
list country1 fid1 fid2
scatter fid1 fid2 country1
restore
use "$root\fid.dta", clear
twoway connected fid11-fid118 country1
twoway connected fid21-fid218 country1

*Question 3
*(a)
use "$root\gasoline.dta", clear
set more off
regife lngascar lnyn lnpmgpgdp lncarn, f(fyear=year1 fid=country1, 2)
est sto m1
drop fid1 fid2 fyear1 fyear2
regife lngascar lnyn lnpmgpgdp lncarn, a(feid = country1 feyear = year1) f(fyear=year1 fid=country1, 2)
est sto m2
*(c)
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








