clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/GP_Sorts, replace text



use C:/Data/Thesis/Returns

merge m:1 permno hp using C:/Data/Thesis/BookVars

keep if _merge==3
drop _merge

merge m:1 permno hp using C:/Data/Thesis/GP_10B

keep if _merge==3
drop _merge

preserve
  collapse (sum) bkt_size = size, by(gp_b date)
  save C:/Data/Thesis/gp_bkt_size, replace
restore

merge m:1 gp_b date using C:/Data/Thesis/gp_bkt_size

drop _merge

keep if gp!=. & hp>=1963

gen wt_ret = ret * size / bkt_size

preserve
  collapse (sum)  ret = wt_ret, by(gp_b date)
  collapse (mean) ret = ret,    by(gp_b)
  replace ret = ret * 100

  list, sep(10)
restore






log close
