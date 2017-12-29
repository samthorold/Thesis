clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Value_Sorts, replace text

*******************************************************************************
* Size-Value Sorts                                                            *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/BM_al_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b bmal_b date)
	save C:/Data/Thesis/ME_BM_al_6B_Size, replace
restore

merge m:1 jun_me_b bmal_b date using C:/Data/Thesis/ME_BM_al_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b bmal_b)

egen bkt = concat(jun_me_b bmal_b)

replace ret = ret * 100

preserve
	drop jun_me_b bmal_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_BM_al_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b bmal_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/BM_ac_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b bmac_b date)
	save C:/Data/Thesis/ME_BM_ac_6B_Size, replace
restore

merge m:1 jun_me_b bmac_b date using C:/Data/Thesis/ME_BM_ac_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b bmac_b)

egen bkt = concat(jun_me_b bmac_b)

replace ret = ret * 100

preserve
	drop jun_me_b bmac_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_BM_ac_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b bmac_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_3B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b!=. & l1_bmmc_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_me_b l1_bmmc_b date)
	save C:/Data/Thesis/ME_BM_mc_6B_Size, replace
restore

merge m:1 l1_me_b l1_bmmc_b date using C:/Data/Thesis/ME_BM_mc_6B_Size, nogen keep(match)

gen wt_ret = ret * l1_me / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date l1_me_b l1_bmmc_b)

egen bkt = concat(l1_me_b l1_bmmc_b)

replace ret = ret * 100

preserve
	drop l1_me_b l1_bmmc_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_BM_mc_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(l1_me_b l1_bmmc_b)
	list, sep(10)
restore
