clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Value_Sorts, replace text

*******************************************************************************
* Value Sorts                                                                 *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/BM_al_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(bmal_b date)
	save C:/Data/Thesis/BM_al_10B_Size, replace
restore

merge m:1 bmal_b date using C:/Data/Thesis/BM_al_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date bmal_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(bmal_b)
	export delim using C:/Data/Thesis/BM_al_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(bmal_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BM_ac_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(bmac_b date)
	save C:/Data/Thesis/BM_ac_10B_Size, replace
restore

merge m:1 bmac_b date using C:/Data/Thesis/BM_ac_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date bmac_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(bmac_b)
	export delim using C:/Data/Thesis/BM_ac_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(bmac_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/BM_mc_10B, nogen keep(match)

sort permno date
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_bmmc_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b date)
	save C:/Data/Thesis/BM_mc_10B_Size, replace
restore

merge m:1 l1_bmmc_b date using C:/Data/Thesis/BM_mc_10B_Size, nogen keep(match)

gen wt_ret = ret * l1_me / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date l1_bmmc_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(l1_bmmc_b)
	export delim using C:/Data/Thesis/BM_mc_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(l1_bmmc_b)
	list, sep(10)
restore
