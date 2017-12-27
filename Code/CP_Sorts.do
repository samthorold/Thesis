clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/CP_Sorts, replace text



use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/BookVars

keep if _merge==3
drop _merge

merge m:1 permno hp using C:/Data/Thesis/CP_10B

keep if _merge==3
drop _merge

preserve
  collapse (sum) bkt_size = size, by(cp_b date)
  save C:/Data/Thesis/cp_bkt_size, replace
restore

merge m:1 cp_b date using C:/Data/Thesis/cp_bkt_size

drop _merge

keep if op_ok & hp>=1963

gen wt_ret = ret * size / bkt_size

preserve
  collapse (sum)  ret = wt_ret, by(cp_b date)
  collapse (mean) ret = ret,    by(cp_b)
  replace ret = ret * 100

  list, sep(10)
restore






log close
