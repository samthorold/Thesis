clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Create_Factors, replace text

*******************************************************************************
* Munge Factors                                                               *
*******************************************************************************

use C:/Data/Thesis/ME_BM_al_6_Returns

keep date HMLs HMLb HML SMB_HML

gen i = _n

merge 1:1 date using C:/Data/Thesis/ME_BM_ac_6_Returns, ///
  nogen keep(match) keepusing(HMLcs HMLcb HMLc SMB_HMLc)

merge 1:1 date using C:/Data/Thesis/ME_BM_mc_6_Returns, ///
  nogen keep(match) keepusing(HMLms HMLmb HMLm SMB_HMLm)

merge 1:1 date using C:/Data/Thesis/ME_OP_6_Returns, ///
  nogen keep(match) keepusing(RMWs RMWb RMW SMB_RMW)

merge 1:1 date using C:/Data/Thesis/ME_OPr_6_Returns, ///
  nogen keep(match) keepusing(RMWrs RMWrb RMWr SMB_RMWr)

merge 1:1 date using C:/Data/Thesis/ME_GP_6_Returns, ///
  nogen keep(match) keepusing(RMWgs RMWgb RMWg SMB_RMWg)

merge 1:1 date using C:/Data/Thesis/ME_CP_6_Returns, ///
  nogen keep(match) keepusing(RMWcs RMWcb RMWc SMB_RMWc)

merge 1:1 date using C:/Data/Thesis/ME_INV_6_Returns, ///
  nogen keep(match) keepusing(CMAs CMAb CMA SMB_CMA)

merge 1:1 date using C:/Data/Thesis/ME_PR_6_Returns, ///
  nogen keep(match) keepusing(WMLs WMLb WML SMB_WML)

merge 1:1 i using C:/Data/Thesis/FF_Factors, nogen keep(match)


gen SMB = (SMB_HML + SMB_RMW + SMB_CMA) / 3

reg hml rm smb rmw cma
predict u, resid
gen hmlo = _b[_cons] + u
drop u

reg HML rm SMB RMW CMA
predict u, resid
gen HMLo = _b[_cons] + u
drop u

save C:/Data/Thesis/Factors, replace
export delim using C:/Data/Thesis/Factors.csv, replace

log close
