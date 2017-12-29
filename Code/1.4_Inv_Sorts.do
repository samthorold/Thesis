clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Inv_Sorts, replace text

*******************************************************************************
* Profit Sorts                                                                *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/INV_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(inv_b date)
	save C:/Data/Thesis/INV_10B_Size, replace
restore

merge m:1 inv_b date using C:/Data/Thesis/INV_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date inv_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(inv_b)
	export delim using C:/Data/Thesis/INV_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(inv_b)
	list, sep(10)
restore
