set more off
capture log close
clear

log using Var_Log, text replace

* import delimited C:\Data\CRSP\20180330_CRSP_Daily_19620701_20171231.csv

* destring ret, replace force
* tostring date, replace
* gen Date = date(date, "YMD")
* drop date
* format Date %td

* drop shrcd

* save C:/Data/Thesis/Var.dta, replace

use C:/Data/Thesis/Var

gen yr = year(Date)
gen mnth = month(Date)

gen good_ret = 1 if ret!=.
replace good_ret = 0 if good_ret==.

collapse (sd) SD_ret=ret (count) N_good=good_ret, by(permno yr mnth)

gen Var_ret = SD_ret^2
drop SD_ret

save C:/Data/Thesis/Var_monthly.dta, replace

log close
