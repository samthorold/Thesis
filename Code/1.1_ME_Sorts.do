clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Sorts, replace text

*******************************************************************************
* Market Equity Sorts                                                         *
*******************************************************************************

use C:/Data/Thesis/Returns

* We only want firms with a bucket based on June Market Equity
merge m:1 permno hp using C:/Data/Thesis/Jun_ME_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b date)
	save C:/Data/Thesis/Jun_ME_10B_Size, replace
restore

merge m:1 jun_me_b date using C:/Data/Thesis/Jun_ME_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(jun_me_b)
	export delim using C:/Data/Thesis/ME_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

* We only want firms with a bucket based on June Market Equity
merge m:1 permno hp using C:/Data/Thesis/Jun_ME_5B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b date)
	save C:/Data/Thesis/Jun_ME_5B_Size, replace
restore

merge m:1 jun_me_b date using C:/Data/Thesis/Jun_ME_5B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(jun_me_b)
	export delim using C:/Data/Thesis/ME_5_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b)
	list, sep(10)
restore










use C:/Data/Thesis/Returns, clear

* We only want firms with a bucket based on June Market Equity
merge m:1 permno hp using C:/Data/Thesis/Jun_ME_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b date)
	save C:/Data/Thesis/Jun_ME_3B_Size, replace
restore

merge m:1 jun_me_b date using C:/Data/Thesis/Jun_ME_3B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(jun_me_b)
	export delim using C:/Data/Thesis/ME_3_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b)
	list, sep(10)
restore
