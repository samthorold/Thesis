clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/VALUE, replace text

*******************************************************************************
* Value     												                  *
*******************************************************************************

* Following Asness and Fazinni (2013), we will have three measures of value
* BM annual, lagged
* BM annual, current
* BM monthly, current

* Return data December holding period is the same as the year
* Book Variables data holding period is fiscal year plus one

use C:/Data/Thesis/BookVars

merge 1:1 permno hp using C:/Data/Thesis/Dec_ME

drop if _merge==2
drop _merge

gen bmal = be / dec_me
gen bmac = be / jun_me

describe
summarize

save C:/Data/Thesis/BookVars, replace



use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars

drop if _merge==2
drop _merge

gen bmmc = be / me

describe
summarize

save C:/Data/Thesis/Returns, replace

log close

