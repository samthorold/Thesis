clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_INV_Sorts, replace text

*******************************************************************************
* Size-Asset Growth Sorts                                                     *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/INV_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b inv_b date)
	save C:/Data/Thesis/ME_INV_6B_Size, replace
restore

merge m:1 jun_me_b inv_b date using C:/Data/Thesis/ME_INV_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b inv_b)

egen bkt = concat(jun_me_b inv_b)

replace ret = ret * 100

preserve
	drop jun_me_b inv_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_INV_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b inv_b)
	list, sep(10)
restore




