clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/BUCKETS, replace text

*******************************************************************************
* Buckets														                                          *
*******************************************************************************

use C:/Data/Thesis/BookVars

local vars permno hp jun_me bmal bmac op op_ok opr gp cp inv

*********
* Value *
*********

*keep `vars'

merge m:1 hp using C:/Data/Thesis/BM_al_20Q, nogen keep(match master)

* 30th and 70th percentile
gen bmal_b = .

replace bmal_b = 1 if bmal <= bmal30
replace bmal_b = 2 if bmal >  bmal30 & bmal <= bmal70
replace bmal_b = 3 if bmal >  bmal70

replace bmal_b = . if bmal==.
drop if bmal_b==.

preserve
  keep permno hp bmal_b
  save C:/Data/Thesis/BM_al_3B, replace
restore

* Quintiles
replace bmal_b = .

replace bmal_b = 1 if bmal <= bmal20
replace bmal_b = 2 if bmal >  bmal20 & bmal <= bmal40
replace bmal_b = 3 if bmal >  bmal40 & bmal <= bmal60
replace bmal_b = 4 if bmal >  bmal60 & bmal <= bmal80
replace bmal_b = 5 if bmal >  bmal80

replace bmal_b = . if bmal==.
drop if bmal_b==.

preserve
  keep permno hp bmal_b
  save C:/Data/Thesis/BM_al_5B, replace
restore

* Deciles
replace bmal_b = .

replace bmal_b = 1 if bmal <= bmal10
replace bmal_b = 2 if bmal >  bmal10 & bmal <= bmal20
replace bmal_b = 3 if bmal >  bmal20 & bmal <= bmal30
replace bmal_b = 4 if bmal >  bmal30 & bmal <= bmal40
replace bmal_b = 5 if bmal >  bmal40 & bmal <= bmal50
replace bmal_b = 6 if bmal >  bmal50 & bmal <= bmal60
replace bmal_b = 7 if bmal >  bmal60 & bmal <= bmal70
replace bmal_b = 8 if bmal >  bmal70 & bmal <= bmal80
replace bmal_b = 9 if bmal >  bmal80 & bmal <= bmal90
replace bmal_b =10 if bmal >  bmal90

replace bmal_b = . if bmal==.
drop if bmal_b==.

preserve
  keep permno hp bmal_b
  save C:/Data/Thesis/BM_al_10B, replace
restore







keep `vars'

merge m:1 hp using C:/Data/Thesis/BM_ac_20Q, nogen keep(match master)

* 30th and 70th percentile
gen bmac_b = .

replace bmac_b = 1 if bmac <= bmac30
replace bmac_b = 2 if bmac >  bmac30 & bmac <= bmac70
replace bmac_b = 3 if bmac >  bmac70

replace bmac_b = . if bmac==.
drop if bmac_b==.

preserve
  keep permno hp bmac_b
  save C:/Data/Thesis/BM_ac_3B, replace
restore

* Quintiles
replace bmac_b = .

replace bmac_b = 1 if bmac <= bmac20
replace bmac_b = 2 if bmac >  bmac20 & bmac <= bmac40
replace bmac_b = 3 if bmac >  bmac40 & bmac <= bmac60
replace bmac_b = 4 if bmac >  bmac60 & bmac <= bmac80
replace bmac_b = 5 if bmac >  bmac80

replace bmac_b = . if bmac==.
drop if bmac_b==.

preserve
  keep permno hp bmac_b
  save C:/Data/Thesis/BM_ac_5B, replace
restore

* Deciles
replace bmac_b = .

replace bmac_b = 1 if bmac <= bmac10
replace bmac_b = 2 if bmac >  bmac10 & bmac <= bmac20
replace bmac_b = 3 if bmac >  bmac20 & bmac <= bmac30
replace bmac_b = 4 if bmac >  bmac30 & bmac <= bmac40
replace bmac_b = 5 if bmac >  bmac40 & bmac <= bmac50
replace bmac_b = 6 if bmac >  bmac50 & bmac <= bmac60
replace bmac_b = 7 if bmac >  bmac60 & bmac <= bmac70
replace bmac_b = 8 if bmac >  bmac70 & bmac <= bmac80
replace bmac_b = 9 if bmac >  bmac80 & bmac <= bmac90
replace bmac_b =10 if bmac >  bmac90

replace bmac_b = . if bmac==.
drop if bmac_b==.

preserve
  keep permno hp bmac_b
  save C:/Data/Thesis/BM_ac_10B, replace
restore








**********
* Profit *
**********

merge m:1 hp using C:/Data/Thesis/OP_20Q, nogen keep(match master)

* 30th and 70th percentile
gen op_b = .

replace op_b = 1 if op <= op30
replace op_b = 2 if op >  op30 & op <= op70
replace op_b = 3 if op >  op70

replace op_b = . if op==.
drop if op_b==.

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

replace op_b = . if op==.
drop if op_b==.

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

replace op_b = . if op==.
drop if op_b==.

preserve
  keep permno hp op_b
  save C:/Data/Thesis/OP_10B, replace
restore




keep `vars'

merge m:1 hp using C:/Data/Thesis/OPr_20Q, nogen keep(match master)

* 30th and 70th percentile
gen opr_b = .

replace opr_b = 1 if opr <= opr30
replace opr_b = 2 if opr >  opr30 & opr <= opr70
replace opr_b = 3 if opr >  opr70

replace opr_b = . if opr==.
drop if opr_b==.

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

replace opr_b = . if opr==.
drop if opr_b==.

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

replace opr_b = . if opr==.
drop if opr_b==.

preserve
  keep permno hp opr_b
  save C:/Data/Thesis/OPr_10B, replace
restore


keep `vars'

merge m:1 hp using C:/Data/Thesis/GP_20Q, nogen keep(match master)

* 30th and 70th percentile
gen gp_b = .

replace gp_b = 1 if gp <= gp30
replace gp_b = 2 if gp >  gp30 & gp <= gp70
replace gp_b = 3 if gp >  gp70

replace gp_b = . if gp==.
drop if gp_b==.

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

replace gp_b = . if gp==.
drop if gp_b==.

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

replace gp_b = . if gp==.
drop if gp_b==.

preserve
  keep permno hp gp_b
  save C:/Data/Thesis/GP_10B, replace
restore



keep `vars'

merge m:1 hp using C:/Data/Thesis/CP_20Q, nogen keep(match master)

* 30th and 70th percentile
gen cp_b = .

replace cp_b = 1 if cp <= cp30
replace cp_b = 2 if cp >  cp30 & cp <= cp70
replace cp_b = 3 if cp >  cp70

replace cp_b = . if cp==.
drop if cp_b==.

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

replace cp_b = . if cp==.
drop if cp_b==.

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

replace cp_b = . if cp==.
drop if cp_b==.

preserve
  keep permno hp cp_b
  save C:/Data/Thesis/CP_10B, replace
restore



****************
* Asset Growth *
****************

keep `vars'

merge m:1 hp using C:/Data/Thesis/INV_20Q, nogen keep(match master)

* 30th and 70th percentile
gen inv_b = .

replace inv_b = 1 if inv <= inv30
replace inv_b = 2 if inv >  inv30 & inv <= inv70
replace inv_b = 3 if inv >  inv70

replace inv_b = . if inv==.
drop if inv_b==.

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

replace inv_b = . if inv==.
drop if inv_b==.

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

replace inv_b = . if inv==.
drop if inv_b==.

preserve
  keep permno hp inv_b
  save C:/Data/Thesis/INV_10B, replace
restore




*****************
* Market Equity *
*****************

keep `vars'

merge m:1 hp using C:/Data/Thesis/Jun_ME_20Q, nogen keep(match master)

* Median
gen jun_me_b = .

replace jun_me_b = 1 if jun_me <= jun_me50
replace jun_me_b = 2 if jun_me >  jun_me50

replace jun_me_b = . if jun_me==.
drop if jun_me_b==.

preserve
  keep permno hp jun_me_b
  save C:/Data/Thesis/Jun_ME_2B, replace
restore

* Quintiles
replace jun_me_b = .

replace jun_me_b = 1 if jun_me <= jun_me20
replace jun_me_b = 2 if jun_me >  jun_me20 & jun_me <= jun_me40
replace jun_me_b = 3 if jun_me >  jun_me40 & jun_me <= jun_me60
replace jun_me_b = 4 if jun_me >  jun_me60 & jun_me <= jun_me80
replace jun_me_b = 5 if jun_me >  jun_me80

replace jun_me_b = . if jun_me==.
drop if jun_me_b==.

preserve
  keep permno hp jun_me_b
  save C:/Data/Thesis/Jun_ME_5B, replace
restore

* Deciles
replace jun_me_b = .

replace jun_me_b = 1 if jun_me <= jun_me10
replace jun_me_b = 2 if jun_me >  jun_me10 & jun_me <= jun_me20
replace jun_me_b = 3 if jun_me >  jun_me20 & jun_me <= jun_me30
replace jun_me_b = 4 if jun_me >  jun_me30 & jun_me <= jun_me40
replace jun_me_b = 5 if jun_me >  jun_me40 & jun_me <= jun_me50
replace jun_me_b = 6 if jun_me >  jun_me50 & jun_me <= jun_me60
replace jun_me_b = 7 if jun_me >  jun_me60 & jun_me <= jun_me70
replace jun_me_b = 8 if jun_me >  jun_me70 & jun_me <= jun_me80
replace jun_me_b = 9 if jun_me >  jun_me80 & jun_me <= jun_me90
replace jun_me_b =10 if jun_me >  jun_me90

replace jun_me_b = . if jun_me==.
drop if jun_me_b==.

preserve
  keep permno hp jun_me_b
  save C:/Data/Thesis/Jun_ME_10B, replace
restore




use C:/Data/Thesis/Returns, clear

merge m:1 date using C:/Data/Thesis/ME_20Q, nogen keep(match master)

* Median
gen me_b = .

replace me_b = 1 if me <= me50
replace me_b = 2 if me >  me50

replace me_b = . if me==.
drop if me_b==.

preserve
  keep permno date me_b
  save C:/Data/Thesis/ME_2B, replace
restore

* Quintiles
replace me_b = .

replace me_b = 1 if me <= me20
replace me_b = 2 if me >  me20 & me <= me40
replace me_b = 3 if me >  me40 & me <= me60
replace me_b = 4 if me >  me60 & me <= me80
replace me_b = 5 if me >  me80

replace me_b = . if me==.
drop if me_b==.

preserve
  keep permno date me_b
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

replace me_b = . if me==.
drop if me_b==.

preserve
  keep permno date me_b
  save C:/Data/Thesis/ME_10B, replace
restore




****************
* Prior Return *
****************

* keep `vars'
drop me*  // we only want pr_11_1 really so it doesn't matter that me dies

merge m:1 date using C:/Data/Thesis/PR_11_1_20Q, nogen keep(match master)

* 30th and 70th percentile
gen pr_11_1_b = .

replace pr_11_1_b = 1 if pr_11_1 <= pr_11_130
replace pr_11_1_b = 2 if pr_11_1 >  pr_11_130 & pr_11_1 <= pr_11_170
replace pr_11_1_b = 3 if pr_11_1 >  pr_11_170

replace pr_11_1_b = . if pr_11_1==.
drop if pr_11_1_b==.

preserve
  keep permno date pr_11_1_b
  save C:/Data/Thesis/PR_3B, replace
restore

* Quintiles
replace pr_11_1_b = .

replace pr_11_1_b = 1 if pr_11_1 <= pr_11_120
replace pr_11_1_b = 2 if pr_11_1 >  pr_11_120 & pr_11_1 <= pr_11_140
replace pr_11_1_b = 3 if pr_11_1 >  pr_11_140 & pr_11_1 <= pr_11_160
replace pr_11_1_b = 4 if pr_11_1 >  pr_11_160 & pr_11_1 <= pr_11_180
replace pr_11_1_b = 5 if pr_11_1 >  pr_11_180

replace pr_11_1_b = . if pr_11_1==.
drop if pr_11_1_b==.

preserve
  keep permno date pr_11_1_b
  save C:/Data/Thesis/PR_5B, replace
restore

* Deciles
replace pr_11_1_b = .

replace pr_11_1_b = 1 if pr_11_1 <= pr_11_110
replace pr_11_1_b = 2 if pr_11_1 >  pr_11_110 & pr_11_1 <= pr_11_120
replace pr_11_1_b = 3 if pr_11_1 >  pr_11_120 & pr_11_1 <= pr_11_130
replace pr_11_1_b = 4 if pr_11_1 >  pr_11_130 & pr_11_1 <= pr_11_140
replace pr_11_1_b = 5 if pr_11_1 >  pr_11_140 & pr_11_1 <= pr_11_150
replace pr_11_1_b = 6 if pr_11_1 >  pr_11_150 & pr_11_1 <= pr_11_160
replace pr_11_1_b = 7 if pr_11_1 >  pr_11_160 & pr_11_1 <= pr_11_170
replace pr_11_1_b = 8 if pr_11_1 >  pr_11_170 & pr_11_1 <= pr_11_180
replace pr_11_1_b = 9 if pr_11_1 >  pr_11_180 & pr_11_1 <= pr_11_190
replace pr_11_1_b =10 if pr_11_1 >  pr_11_190

replace pr_11_1_b = . if pr_11_1==.
drop if pr_11_1_b==.

preserve
  keep permno date pr_11_1_b
  save C:/Data/Thesis/PR_10B, replace
restore




*********
* Value *
*********

* keep `vars'
drop pr*  // we only want bmmc really so it doesn't matter that pr_11_1 dies

merge m:1 date using C:/Data/Thesis/BM_mc_20Q, nogen keep(match master)

* 30th and 70th percentile
gen bmmc_b = .

replace bmmc_b = 1 if bmmc <= bmmc30
replace bmmc_b = 2 if bmmc >  bmmc30 & bmmc <= bmmc70
replace bmmc_b = 3 if bmmc >  bmmc70

replace bmmc_b = . if bmmc==.
drop if bmmc_b==.

preserve
  keep permno date bmmc_b
  save C:/Data/Thesis/BM_mc_3B, replace
restore

* Quintiles
replace bmmc_b = .

replace bmmc_b = 1 if bmmc <= bmmc20
replace bmmc_b = 2 if bmmc >  bmmc20 & bmmc <= bmmc40
replace bmmc_b = 3 if bmmc >  bmmc40 & bmmc <= bmmc60
replace bmmc_b = 4 if bmmc >  bmmc60 & bmmc <= bmmc80
replace bmmc_b = 5 if bmmc >  bmmc80

replace bmmc_b = . if bmmc==.
drop if bmmc_b==.

preserve
  keep permno date bmmc_b
  save C:/Data/Thesis/BM_mc_5B, replace
restore

* Deciles
replace bmmc_b = .

replace bmmc_b = 1 if bmmc <= bmmc10
replace bmmc_b = 2 if bmmc >  bmmc10 & bmmc <= bmmc20
replace bmmc_b = 3 if bmmc >  bmmc20 & bmmc <= bmmc30
replace bmmc_b = 4 if bmmc >  bmmc30 & bmmc <= bmmc40
replace bmmc_b = 5 if bmmc >  bmmc40 & bmmc <= bmmc50
replace bmmc_b = 6 if bmmc >  bmmc50 & bmmc <= bmmc60
replace bmmc_b = 7 if bmmc >  bmmc60 & bmmc <= bmmc70
replace bmmc_b = 8 if bmmc >  bmmc70 & bmmc <= bmmc80
replace bmmc_b = 9 if bmmc >  bmmc80 & bmmc <= bmmc90
replace bmmc_b =10 if bmmc >  bmmc90

replace bmmc_b = . if bmmc==.
drop if bmmc_b==.

preserve
  keep permno date bmmc_b
  save C:/Data/Thesis/BM_mc_10B, replace
restore





log close
