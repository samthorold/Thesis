clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/VALUE, replace text

*******************************************************************************
* Value                                                                       *
*******************************************************************************

* Following Asness and Fazinni (2013), we will have three measures of value
* BM annual, lagged
* BM annual, current
* BM monthly, current

* Return data December holding period is the same as the year
* Book Variables data holding period is fiscal year plus one

use C:/Data/Thesis/BookVars

merge 1:1 permno hp using C:/Data/Thesis/Dec_ME, nogen keep(match master)

gen bmal = be / dec_me
label var bmal "Book-to-Market, Annual Formation with Lagged Market Equity"

gen bmac = be / jun_me
label var bmac "Book-to-Market, Annual Formation with Current Market Equity"

describe
summarize

save C:/Data/Thesis/BookVars, replace



use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match master)

gen bmmc = be / me
label var bmmc "Book-to-Market, Monthly Formation with Current Market Equity"

drop jun_me dec_me be op op_ok opr gp cp inv bmal bmac

describe
summarize

save C:/Data/Thesis/Returns, replace

log close

