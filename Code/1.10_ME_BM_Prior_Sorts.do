clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Value_Prior_Sorts, replace text

*******************************************************************************
* Size-Value-Prior Sorts                                                      *
*******************************************************************************

*****************
* Monthly Value *
*****************

*** Small

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2 = pr_11_1[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep permno date hp l1_me_b l1_bmmc_b pr_12_2_b ret me l1_me bmmc bmal pr_12_2 d_be d1_be inv

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_mc_PR_16B_Size, replace
restore

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_mc_PR_16B_Size, nogen keep(match)

* Returns

preserve

	gen wt_x = ret * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)

	replace x = x * 100

	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_Returns.csv, replace
restore

* Past Change in Book Equity

preserve

	gen wt_x = d_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_D_BE.csv, replace

restore

* Future Change in Book Equity

preserve

	gen wt_x = d1_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_D1_BE.csv, replace

restore

* Monthly Value

preserve

	gen wt_x = bmmc * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_BM_mc.csv, replace

restore

* Annual Value

preserve

	gen wt_x = bmal * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_BM_al.csv, replace

restore

* Prior

preserve

	gen wt_x = pr_12_2 * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_PR.csv, replace

restore

* Inv

preserve

	gen wt_x = inv * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_mc_PR_16_Inv.csv, replace

restore

*** Big

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2 = pr_11_1[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep permno date hp l1_me_b l1_bmmc_b pr_12_2_b ret me l1_me bmmc bmal pr_12_2 d_be d1_be inv

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Big_BM_mc_PR_16B_Size, replace
restore

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_mc_PR_16B_Size, nogen keep(match)

* Returns

preserve

	gen wt_x = ret * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)

	replace x = x * 100

	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_Returns.csv, replace
restore

* Past Change in Book Equity

preserve

	gen wt_x = d_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_D_BE.csv, replace

restore

* Future Change in Book Equity

preserve

	gen wt_x = d1_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_D1_BE.csv, replace

restore

* Monthly Value

preserve

	gen wt_x = bmmc * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_BM_mc.csv, replace

restore

* Annual Value

preserve

	gen wt_x = bmal * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_BM_al.csv, replace

restore

* Prior

preserve

	gen wt_x = pr_12_2 * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_PR.csv, replace

restore

* Inv

preserve

	gen wt_x = inv * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date l1_bmmc_b pr_12_2_b)
	egen bkt = concat(l1_bmmc_b pr_12_2_b)
	drop l1_bmmc_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_mc_PR_16_Inv.csv, replace

restore

****************
* Annual Value *
****************

*** Small

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno hp   using C:/Data/Thesis/BM_al_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen pr_12_2 = pr_11_1[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep permno date hp l1_me_b bmal_b pr_12_2_b ret me l1_me bmmc bmal pr_12_2 d_be d1_be inv

keep if l1_me_b==1 & bmal_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(bmal_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_al_PR_16B_Size, replace
restore

merge m:1 bmal_b pr_12_2_b date using C:/Data/Thesis/Small_BM_al_PR_16B_Size, nogen keep(match)

* Returns

preserve

	gen wt_x = ret * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)

	replace x = x * 100

	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_Returns.csv, replace
restore

* Past Change in Book Equity

preserve

	gen wt_x = d_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_D_BE.csv, replace

restore

* Future Change in Book Equity

preserve

	gen wt_x = d1_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_D1_BE.csv, replace

restore

* Monthly Value

preserve

	gen wt_x = bmmc * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_BM_mc.csv, replace

restore

* Annual Value

preserve

	gen wt_x = bmal * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_BM_al.csv, replace

restore

* Prior

preserve

	gen wt_x = pr_12_2 * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_PR.csv, replace

restore

* Inv

preserve

	gen wt_x = inv * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_PR_16_Inv.csv, replace

restore

*** Big

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno hp   using C:/Data/Thesis/BM_al_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen pr_12_2 = pr_11_1[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep permno date hp l1_me_b bmal_b pr_12_2_b ret me l1_me bmmc bmal pr_12_2 d_be d1_be inv

keep if l1_me_b==2 & bmal_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(bmal_b pr_12_2_b date)
	save C:/Data/Thesis/Big_BM_al_PR_16B_Size, replace
restore

merge m:1 bmal_b pr_12_2_b date using C:/Data/Thesis/Big_BM_al_PR_16B_Size, nogen keep(match)

* Returns

preserve

	gen wt_x = ret * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)

	replace x = x * 100

	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_Returns.csv, replace
restore

* Past Change in Book Equity

preserve

	gen wt_x = d_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_D_BE.csv, replace

restore

* Future Change in Book Equity

preserve

	gen wt_x = d1_be * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_D1_BE.csv, replace

restore

* Monthly Value

preserve

	gen wt_x = bmmc * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_BM_mc.csv, replace

restore

* Annual Value

preserve

	gen wt_x = bmal * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_BM_al.csv, replace

restore

* Prior

preserve

	gen wt_x = pr_12_2 * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_PR.csv, replace

restore

* Inv

preserve

	gen wt_x = inv * l1_me / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b pr_12_2_b)
	egen bkt = concat(bmal_b pr_12_2_b)
	drop bmal_b pr_12_2_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_PR_16_Inv.csv, replace

restore
