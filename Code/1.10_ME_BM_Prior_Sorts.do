clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/ME_Value_Prior_Sorts, replace text

*******************************************************************************
* Size-Value-Prior Sorts                                                      *
*******************************************************************************

*** Small

* N

use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

keep if hp>=1963 & year(date)<=2016

collapse (count) N=permno, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide N, i(date) j(bkt) string
	save C:/Data/Thesis/Small_BM_PR_16_N, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_N.csv, replace
restore

preserve
	collapse (mean) N=N, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* ME

use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

keep if hp>=1963 & year(date)<=2016

collapse (sum) me=me, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

replace me = me / 1000

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide me, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_ME, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_ME.csv, replace
restore

preserve
	collapse (mean) me=me, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Returns

use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide ret, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_Returns, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Change in Book Equity

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Big_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_d_be = d_be * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) d_be=wt_d_be, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide d_be, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_D_BE, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_D_BE.csv, replace
restore

preserve
	collapse (mean) d_be=d_be, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore


* Future Change in Book Equity

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_d1_be = d1_be * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) d1_be=wt_d1_be, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide d1_be, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_D1_BE, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_D1_BE.csv, replace
restore

preserve
	collapse (mean) d1_be=d1_be, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Monthly Value

use C:/Data/Thesis/Returns, clear

*merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_bmmc = bmmc * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) bmmc=wt_bmmc, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide bmmc, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_BM_mc, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_BM_mc.csv, replace
restore

preserve
	collapse (mean) bmmc=bmmc, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Annual Value

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_bmal = bmal * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) bmal=wt_bmal, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide bmal, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_BM_al, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_BM_al.csv, replace
restore

preserve
	collapse (mean) bmal=bmal, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Prior

use C:/Data/Thesis/Returns, clear

*merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_pr_12_2 = pr_12_2 * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) pr_12_2=wt_pr_12_2, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide pr_12_2, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_PR, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_PR.csv, replace
restore

preserve
	collapse (mean) pr_12_2=pr_12_2, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Inv

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==1 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Small_BM_PR_16B_Size, nogen keep(match)

gen wt_inv = inv * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) inv=wt_inv, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide inv, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Small_BM_PR_16_Inv, replace
	export delim using C:/Data/Thesis/Small_BM_PR_16_Inv.csv, replace
restore

preserve
	collapse (mean) inv=inv, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore


*** Big

* N

use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

keep if hp>=1963 & year(date)<=2016

collapse (count) N=permno, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide N, i(date) j(bkt) string
	save C:/Data/Thesis/Big_BM_PR_16_N, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_N.csv, replace
restore

preserve
	collapse (mean) N=N, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* ME

use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

keep if hp>=1963 & year(date)<=2016

collapse (sum) me=me, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

replace me = me / 1000

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide me, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_ME, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_ME.csv, replace
restore

preserve
	collapse (mean) me=me, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Returns

use C:/Data/Thesis/Returns, clear

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Big_BM_PR_16B_Size, replace
restore

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_ret = ret * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) ret=wt_ret, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide ret, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_Returns, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_Returns.csv, replace
restore

preserve
	collapse (mean) ret=ret, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Change in Book Equity

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Big_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_d_be = d_be * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) d_be=wt_d_be, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide d_be, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_D_BE, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_D_BE.csv, replace
restore

preserve
	collapse (mean) d_be=d_be, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Future Change in Book Equity

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Big_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_d1_be = d1_be * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) d1_be=wt_d1_be, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide d1_be, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_D1_BE, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_D1_BE.csv, replace
restore

preserve
	collapse (mean) d1_be=d1_be, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Monthly Value

use C:/Data/Thesis/Returns, clear

*merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_bmmc = bmmc * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) bmmc=wt_bmmc, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide bmmc, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_BM_mc, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_BM_mc.csv, replace
restore

preserve
	collapse (mean) bmmc=bmmc, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Annual Value

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_bmal = bmal * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) bmal=wt_bmal, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide bmal, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_BM_al, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_BM_al.csv, replace
restore

preserve
	collapse (mean) bmal=bmal, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Prior

use C:/Data/Thesis/Returns, clear

*merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_pr_12_2 = pr_12_2 * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) pr_12_2=wt_pr_12_2, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide pr_12_2, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_PR, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_PR.csv, replace
restore

preserve
	collapse (mean) pr_12_2=pr_12_2, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore

* Inv

use C:/Data/Thesis/Returns, clear

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match)

merge m:1 permno date using C:/Data/Thesis/ME_2B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/BM_mc_4B, nogen keep(match)
merge m:1 permno date using C:/Data/Thesis/PR_4B, nogen keep(match)

sort permno date
by permno: gen l1_me_b = me_b[_n-1]
by permno: gen l1_bmmc_b = bmmc_b[_n-1]
by permno: gen pr_12_2_b = pr_11_1_b[_n-1]
by permno: gen l1_me = me[_n-1]

keep if l1_me_b==2 & l1_bmmc_b!=. & pr_12_2_b!=.

 /*
preserve
	collapse (sum) bkt_size=l1_me, by(l1_bmmc_b pr_12_2_b date)
	save C:/Data/Thesis/Small_BM_PR_16B_Size, replace
restore
*/

merge m:1 l1_bmmc_b pr_12_2_b date using C:/Data/Thesis/Big_BM_PR_16B_Size, nogen keep(match)

gen wt_inv = inv * size / bkt_size

keep if hp>=1963 & year(date)<=2016

collapse (sum) inv=wt_inv, by(date l1_bmmc_b pr_12_2_b)

egen bkt = concat(l1_bmmc_b pr_12_2_b)

*replace ret = ret * 100

preserve
	drop l1_bmmc_b pr_12_2_b
	reshape wide inv, i(date) j(bkt) string
	*gen WMLs = ret13 - ret11
	*gen WMLb = ret23 - ret21
	*gen WML = (WMLs + WMLb) / 2
	*gen SMB_WML = (ret11 + ret12 + ret13) / 3 - (ret21 + ret22 + ret23) / 3
	save C:/Data/Thesis/Big_BM_PR_16_Inv, replace
	export delim using C:/Data/Thesis/Big_BM_PR_16_Inv.csv, replace
restore

preserve
	collapse (mean) inv=inv, by(l1_bmmc_b pr_12_2_b)
	list, sep(16)
restore
