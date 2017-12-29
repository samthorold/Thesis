clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Prior_Sorts, replace text

*******************************************************************************
* Prior Sorts                                                                 *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno date using C:/Data/Thesis/PR_10B, nogen keep(match)

sort permno date
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(pr_12_2_b date)
	save C:/Data/Thesis/PR_10B_Size, replace
restore

merge m:1 pr_12_2_b date using C:/Data/Thesis/PR_10B_Size, nogen keep(match)

gen wt_ret = ret * l1_me / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date pr_12_2_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(pr_12_2_b)
	export delim using C:/Data/Thesis/PR_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(pr_12_2_b)
	list, sep(10)
restore
