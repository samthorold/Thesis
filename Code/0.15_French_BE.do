clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/FrenchBE, replace text

*******************************************************************************
* Ken French BE Munging                                                       *
*******************************************************************************

import delim C:/Data/FrenchDartmouth/BE.csv

reshape long v, i(lpermno) j(fyear)

replace fyear = fyear - 4+1926

keep if fyear>=yeari & fyear<=yearf

rename v hand_be

drop yeari yearf

save C:/Data/Thesis/Hand_BE, replace 
