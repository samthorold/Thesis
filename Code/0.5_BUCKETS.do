clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/BUCKETS, replace text

*******************************************************************************
* Buckets														              *
*******************************************************************************

use C:/Data/Thesis/BookVars

local vars permno hp op op_ok opr gp cp inv

**********
* Profit *
**********

merge m:1 hp using C:/Data/Thesis/OP_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen op_b = .

replace op_b = 1 if op <= op30
replace op_b = 2 if op >  op30 & op <= op70
replace op_b = 3 if op >  op70

preserve
  keep permno hp op_b
  save C:/Data/Thesis/OP_3B, replace
restore

* Quintiles
replace op_b = .

replace op_b = 1 if op <= op20
replace op_b = 2 if op >  op20 & op <= op40
replace op_b = 3 if op >  op40 & op <= op60
replace op_b = 4 if op >  op60 & op <= op80
replace op_b = 5 if op >  op80

preserve
  keep permno hp op_b
  save C:/Data/Thesis/OP_5B, replace
restore

* Deciles
replace op_b = .

replace op_b = 1 if op <= op10
replace op_b = 2 if op >  op10 & op <= op20
replace op_b = 3 if op >  op20 & op <= op30
replace op_b = 4 if op >  op30 & op <= op40
replace op_b = 5 if op >  op40 & op <= op50
replace op_b = 6 if op >  op50 & op <= op60
replace op_b = 7 if op >  op60 & op <= op70
replace op_b = 8 if op >  op70 & op <= op80
replace op_b = 9 if op >  op80 & op <= op90
replace op_b =10 if op >  op90

preserve
  keep permno hp op_b
  save C:/Data/Thesis/OP_10B, replace
restore




keep `vars'

merge m:1 hp using C:/Data/Thesis/OPr_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen opr_b = .

replace opr_b = 1 if opr <= opr30
replace opr_b = 2 if opr >  opr30 & opr <= opr70
replace opr_b = 3 if opr >  opr70

preserve
  keep permno hp opr_b
  save C:/Data/Thesis/OPr_3B, replace
restore

* Quintiles
replace opr_b = .

replace opr_b = 1 if opr <= opr20
replace opr_b = 2 if opr >  opr20 & opr <= opr40
replace opr_b = 3 if opr >  opr40 & opr <= opr60
replace opr_b = 4 if opr >  opr60 & opr <= opr80
replace opr_b = 5 if opr >  opr80

preserve
  keep permno hp opr_b
  save C:/Data/Thesis/OPr_5B, replace
restore

* Deciles
replace opr_b = .

replace opr_b = 1 if opr <= opr10
replace opr_b = 2 if opr >  opr10 & opr <= opr20
replace opr_b = 3 if opr >  opr20 & opr <= opr30
replace opr_b = 4 if opr >  opr30 & opr <= opr40
replace opr_b = 5 if opr >  opr40 & opr <= opr50
replace opr_b = 6 if opr >  opr50 & opr <= opr60
replace opr_b = 7 if opr >  opr60 & opr <= opr70
replace opr_b = 8 if opr >  opr70 & opr <= opr80
replace opr_b = 9 if opr >  opr80 & opr <= opr90
replace opr_b =10 if opr >  opr90

preserve
  keep permno hp opr_b
  save C:/Data/Thesis/OPr_10B, replace
restore


keep `vars'

merge m:1 hp using C:/Data/Thesis/GP_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen gp_b = .

replace gp_b = 1 if gp <= gp30
replace gp_b = 2 if gp >  gp30 & gp <= gp70
replace gp_b = 3 if gp >  gp70

preserve
  keep permno hp gp_b
  save C:/Data/Thesis/GP_3B, replace
restore

* Quintiles
replace gp_b = .

replace gp_b = 1 if gp <= gp20
replace gp_b = 2 if gp >  gp20 & gp <= gp40
replace gp_b = 3 if gp >  gp40 & gp <= gp60
replace gp_b = 4 if gp >  gp60 & gp <= gp80
replace gp_b = 5 if gp >  gp80

preserve
  keep permno hp gp_b
  save C:/Data/Thesis/GP_5B, replace
restore

* Deciles
replace gp_b = .

replace gp_b = 1 if gp <= gp10
replace gp_b = 2 if gp >  gp10 & gp <= gp20
replace gp_b = 3 if gp >  gp20 & gp <= gp30
replace gp_b = 4 if gp >  gp30 & gp <= gp40
replace gp_b = 5 if gp >  gp40 & gp <= gp50
replace gp_b = 6 if gp >  gp50 & gp <= gp60
replace gp_b = 7 if gp >  gp60 & gp <= gp70
replace gp_b = 8 if gp >  gp70 & gp <= gp80
replace gp_b = 9 if gp >  gp80 & gp <= gp90
replace gp_b =10 if gp >  gp90

preserve
  keep permno hp gp_b
  save C:/Data/Thesis/GP_10B, replace
restore



keep `vars'

merge m:1 hp using C:/Data/Thesis/CP_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen cp_b = .

replace cp_b = 1 if cp <= cp30
replace cp_b = 2 if cp >  cp30 & cp <= cp70
replace cp_b = 3 if cp >  cp70

preserve
  keep permno hp cp_b
  save C:/Data/Thesis/CP_3B, replace
restore

* Quintiles
replace cp_b = .

replace cp_b = 1 if cp <= cp20
replace cp_b = 2 if cp >  cp20 & cp <= cp40
replace cp_b = 3 if cp >  cp40 & cp <= cp60
replace cp_b = 4 if cp >  cp60 & cp <= cp80
replace cp_b = 5 if cp >  cp80

preserve
  keep permno hp cp_b
  save C:/Data/Thesis/CP_5B, replace
restore

* Deciles
replace cp_b = .

replace cp_b = 1 if cp <= cp10
replace cp_b = 2 if cp >  cp10 & cp <= cp20
replace cp_b = 3 if cp >  cp20 & cp <= cp30
replace cp_b = 4 if cp >  cp30 & cp <= cp40
replace cp_b = 5 if cp >  cp40 & cp <= cp50
replace cp_b = 6 if cp >  cp50 & cp <= cp60
replace cp_b = 7 if cp >  cp60 & cp <= cp70
replace cp_b = 8 if cp >  cp70 & cp <= cp80
replace cp_b = 9 if cp >  cp80 & cp <= cp90
replace cp_b =10 if cp >  cp90

preserve
  keep permno hp cp_b
  save C:/Data/Thesis/CP_10B, replace
restore



****************
* Asset Growth *
****************

keep `vars'

merge m:1 hp using C:/Data/Thesis/INV_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen inv_b = .

replace inv_b = 1 if inv <= inv30
replace inv_b = 2 if inv >  inv30 & inv <= inv70
replace inv_b = 3 if inv >  inv70

preserve
  keep permno hp inv_b
  save C:/Data/Thesis/INV_3B, replace
restore

* Quintiles
replace inv_b = .

replace inv_b = 1 if inv <= inv20
replace inv_b = 2 if inv >  inv20 & inv <= inv40
replace inv_b = 3 if inv >  inv40 & inv <= inv60
replace inv_b = 4 if inv >  inv60 & inv <= inv80
replace inv_b = 5 if inv >  inv80

preserve
  keep permno hp inv_b
  save C:/Data/Thesis/INV_5B, replace
restore

* Deciles
replace inv_b = .

replace inv_b = 1 if inv <= inv10
replace inv_b = 2 if inv >  inv10 & inv <= inv20
replace inv_b = 3 if inv >  inv20 & inv <= inv30
replace inv_b = 4 if inv >  inv30 & inv <= inv40
replace inv_b = 5 if inv >  inv40 & inv <= inv50
replace inv_b = 6 if inv >  inv50 & inv <= inv60
replace inv_b = 7 if inv >  inv60 & inv <= inv70
replace inv_b = 8 if inv >  inv70 & inv <= inv80
replace inv_b = 9 if inv >  inv80 & inv <= inv90
replace inv_b =10 if inv >  inv90

preserve
  keep permno hp inv_b
  save C:/Data/Thesis/INV_10B, replace
restore




*****************
* Market Equity *
*****************

keep `vars'

merge m:1 hp using C:/Data/Thesis/Jun_ME_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen size_b = .

replace size_b = 1 if size <= size30
replace size_b = 2 if size >  size30 & size <= size70
replace size_b = 3 if size >  size70

preserve
  keep permno hp size_b
  save C:/Data/Thesis/Size_3B, replace
restore

* Quintiles
replace size_b = .

replace size_b = 1 if size <= size20
replace size_b = 2 if size >  size20 & size <= size40
replace size_b = 3 if size >  size40 & size <= size60
replace size_b = 4 if size >  size60 & size <= size80
replace size_b = 5 if size >  size80

preserve
  keep permno hp size_b
  save C:/Data/Thesis/Size_5B, replace
restore

* Deciles
replace size_b = .

replace size_b = 1 if size <= size10
replace size_b = 2 if size >  size10 & size <= size20
replace size_b = 3 if size >  size20 & size <= size30
replace size_b = 4 if size >  size30 & size <= size40
replace size_b = 5 if size >  size40 & size <= size50
replace size_b = 6 if size >  size50 & size <= size60
replace size_b = 7 if size >  size60 & size <= size70
replace size_b = 8 if size >  size70 & size <= size80
replace size_b = 9 if size >  size80 & size <= size90
replace size_b =10 if size >  size90

preserve
  keep permno hp size_b
  save C:/Data/Thesis/Size_10B, replace
restore




keep `vars'

merge m:1 hp using C:/Data/Thesis/ME_20Q

drop if _merge==2
drop _merge

* 30th and 70th percentile
gen me_b = .

replace me_b = 1 if me <= me30
replace me_b = 2 if me >  me30 & me <= me70
replace me_b = 3 if me >  me70

preserve
  keep permno hp me_b
  save C:/Data/Thesis/ME_3B, replace
restore

* Quintiles
replace me_b = .

replace me_b = 1 if me <= me20
replace me_b = 2 if me >  me20 & me <= me40
replace me_b = 3 if me >  me40 & me <= me60
replace me_b = 4 if me >  me60 & me <= me80
replace me_b = 5 if me >  me80

preserve
  keep permno hp me_b
  save C:/Data/Thesis/ME_5B, replace
restore

* Deciles
replace me_b = .

replace me_b = 1 if me <= me10
replace me_b = 2 if me >  me10 & me <= me20
replace me_b = 3 if me >  me20 & me <= me30
replace me_b = 4 if me >  me30 & me <= me40
replace me_b = 5 if me >  me40 & me <= me50
replace me_b = 6 if me >  me50 & me <= me60
replace me_b = 7 if me >  me60 & me <= me70
replace me_b = 8 if me >  me70 & me <= me80
replace me_b = 9 if me >  me80 & me <= me90
replace me_b =10 if me >  me90

preserve
  keep permno hp me_b
  save C:/Data/Thesis/ME_10B, replace
restore









log close
