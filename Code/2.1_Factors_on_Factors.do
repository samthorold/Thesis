clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Spanning_Regs, replace text

*******************************************************************************
* Spanning Regs                                                               *
*******************************************************************************

import delim C:/Data/FrenchDartmouth/25_Portfolios_5x5.CSV

gen i = _n

drop if date<196307 | date>201612

merge 1:1 i using C:/Data/Thesis/Factors, nogen keep(match)

