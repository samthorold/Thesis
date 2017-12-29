clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Profit_Sorts, replace text

*******************************************************************************
* Profit Sorts                                                                *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/OP_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(op_b date)
	save C:/Data/Thesis/OP_10B_Size, replace
restore

merge m:1 op_b date using C:/Data/Thesis/OP_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date op_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(op_b)
	export delim using C:/Data/Thesis/OP_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(op_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/OPr_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(opr_b date)
	save C:/Data/Thesis/OPr_10B_Size, replace
restore

merge m:1 opr_b date using C:/Data/Thesis/OPr_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date opr_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(opr_b)
	export delim using C:/Data/Thesis/OPr_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(opr_b)
	list, sep(10)
restore



use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/GP_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(gp_b date)
	save C:/Data/Thesis/GP_10B_Size, replace
restore

merge m:1 gp_b date using C:/Data/Thesis/GP_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date gp_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(gp_b)
	export delim using C:/Data/Thesis/GP_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(gp_b)
	list, sep(10)
restore






use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/CP_10B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(cp_b date)
	save C:/Data/Thesis/CP_10B_Size, replace
restore

merge m:1 cp_b date using C:/Data/Thesis/CP_10B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date cp_b)

replace ret = ret * 100

preserve
	reshape wide ret, i(date) j(cp_b)
	export delim using C:/Data/Thesis/CP_10_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(cp_b)
	list, sep(10)
restore
