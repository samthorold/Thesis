clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_STR_Sorts, replace text

*******************************************************************************
* Size-STR Sorts                                                              *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/l1_ret_3B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b!=. & l1_ret_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_me_b l1_ret_b date)
	save C:/Data/Thesis/ME_STR_6B_Size, replace
restore

merge m:1 l1_me_b l1_ret_b date using C:/Data/Thesis/ME_STR_6B_Size, nogen keep(match)

gen wt_ret = ret * l1_me / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date l1_me_b l1_ret_b)

egen bkt = concat(l1_me_b l1_ret_b)

replace ret = ret * 100

preserve
	drop l1_me_b l1_ret_b
	reshape wide ret, i(date) j(bkt) string
	gen STRs = ret11 - ret13
	gen STRb = ret21 - ret23
	gen STR = (STRs + STRb) / 2
	gen SMB_STR = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/ME_STR_6_Returns, replace
	export delim using C:/Data/Thesis/ME_STR_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(l1_me_b l1_ret_b)
	list, sep(10)
restore




