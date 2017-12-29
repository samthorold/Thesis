clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Prof_Sorts, replace text

*******************************************************************************
* Size-Profit Sorts                                                           *
*******************************************************************************

use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/OP_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b op_b date)
	save C:/Data/Thesis/ME_OP_6B_Size, replace
restore

merge m:1 jun_me_b op_b date using C:/Data/Thesis/ME_OP_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b op_b)

egen bkt = concat(jun_me_b op_b)

replace ret = ret * 100

preserve
	drop jun_me_b op_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_OP_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b op_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/OPr_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b opr_b date)
	save C:/Data/Thesis/ME_OPr_6B_Size, replace
restore

merge m:1 jun_me_b opr_b date using C:/Data/Thesis/ME_OPr_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b opr_b)

egen bkt = concat(jun_me_b opr_b)

replace ret = ret * 100

preserve
	drop jun_me_b opr_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_OPr_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b opr_b)
	list, sep(10)
restore







use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/GP_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b gp_b date)
	save C:/Data/Thesis/ME_GP_6B_Size, replace
restore

merge m:1 jun_me_b gp_b date using C:/Data/Thesis/ME_GP_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b gp_b)

egen bkt = concat(jun_me_b gp_b)

replace ret = ret * 100

preserve
	drop jun_me_b gp_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_GP_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b gp_b)
	list, sep(10)
restore






use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/Jun_ME_2B, nogen keep(match)
merge m:1 permno hp using C:/Data/Thesis/CP_3B, nogen keep(match)

preserve
	collapse (sum) bkt_size=size, by(jun_me_b cp_b date)
	save C:/Data/Thesis/ME_CP_6B_Size, replace
restore

merge m:1 jun_me_b cp_b date using C:/Data/Thesis/ME_CP_6B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date jun_me_b cp_b)

egen bkt = concat(jun_me_b cp_b)

replace ret = ret * 100

preserve
	drop jun_me_b cp_b
	reshape wide ret, i(date) j(bkt) string
	export delim using C:/Data/Thesis/ME_CP_6_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(jun_me_b cp_b)
	list, sep(10)
restore




