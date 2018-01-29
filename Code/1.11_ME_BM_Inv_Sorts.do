clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Value_Prior_Sorts, replace text

*******************************************************************************
* Size-Value-Investment Sorts                                                 *
*******************************************************************************

****************
* Annual Value *
****************

*** Small

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/BM_al_4B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/Inv_4B, nogen keep(match)

sort permno date
by permno: gen pr_12_2 = pr_11_1[_n-1]

keep permno date hp jun_me_b bmal_b inv_b ret size bmmc bmal pr_12_2 d_be d1_be inv

keep if jun_me_b==1 & bmal_b!=. & inv_b!=.

preserve
	collapse (sum) bkt_size=size, by(bmal_b inv_b date)
	save C:/Data/Thesis/Small_BM_al_Inv_16B_Size, replace
restore

merge m:1 bmal_b inv_b date using C:/Data/Thesis/Small_BM_al_Inv_16B_Size, nogen keep(match)

* Returns

preserve

	gen wt_x = ret * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)

	replace x = x * 100

	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_Returns.csv, replace
restore

* Past Change in Book Equity

preserve

	gen wt_x = d_be * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_D_BE.csv, replace

restore

* Future Change in Book Equity

preserve

	gen wt_x = d1_be * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_D1_BE.csv, replace

restore

* Monthly Value

preserve

	gen wt_x = bmmc * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_BM_mc.csv, replace

restore

* Annual Value

preserve

	gen wt_x = bmal * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_BM_al.csv, replace

restore

* Prior

preserve

	gen wt_x = pr_12_2 * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b size_b)
	egen bkt = concat(bmal_b size_b)
	drop bmal_b size_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_PR.csv, replace

restore

* Inv

preserve

	gen wt_x = inv * inv / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Small_BM_al_Inv_16_Inv.csv, replace

restore

*** Big

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/BM_al_4B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/Inv_4B, nogen keep(match)

sort permno date
by permno: gen pr_12_2 = pr_11_1[_n-1]

keep permno date hp jun_me_b bmal_b inv_b ret size bmmc bmal pr_12_2 d_be d1_be inv

keep if jun_me_b==2 & bmal_b!=. & inv_b!=.

preserve
	collapse (sum) bkt_size=size, by(bmal_b inv_b date)
	save C:/Data/Thesis/Big_BM_al_Inv_16B_Size, replace
restore

merge m:1 bmal_b inv_b date using C:/Data/Thesis/Big_BM_al_Inv_16B_Size, nogen keep(match)

* Returns

preserve

	gen wt_x = ret * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)

	replace x = x * 100

	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_Returns.csv, replace
restore

* Past Change in Book Equity

preserve

	gen wt_x = d_be * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_D_BE.csv, replace

restore

* Future Change in Book Equity

preserve

	gen wt_x = d1_be * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_D1_BE.csv, replace

restore

* Monthly Value

preserve

	gen wt_x = bmmc * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_BM_mc.csv, replace

restore

* Annual Value

preserve

	gen wt_x = bmal * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_BM_al.csv, replace

restore

* Prior

preserve

	gen wt_x = pr_12_2 * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_PR.csv, replace

restore

* Inv

preserve

	gen wt_x = inv * size / bkt_size

	keep if hp>=1963 & year(date)<=2016
	collapse (sum) x=wt_x, by(date bmal_b inv_b)
	egen bkt = concat(bmal_b inv_b)
	drop bmal_b inv_b
	reshape wide x, i(date) j(bkt) string

	export delim using C:/Data/Thesis/Big_BM_al_Inv_16_Inv.csv, replace

restore
