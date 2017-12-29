clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Prior_Sorts, replace text

*******************************************************************************
* Size-Prior Sorts                                                            *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_3B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_me_b pr_12_2_b date)
	save C:/Data/Thesis/ME_PR_6B_Size, replace
restore

merge m:1 l1_me_b pr_12_2_b date using C:/Data/Thesis/ME_PR_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date l1_me_b pr_12_2_b)

egen bkt = concat(l1_me_b pr_12_2_b)

replace ret = ret * 100

preserve
	drop l1_me_b pr_12_2_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_PR_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(l1_me_b pr_12_2_b)
	list, sep(10)
restore




