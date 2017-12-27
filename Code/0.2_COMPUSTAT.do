clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/COMPUSTAT, replace text

*******************************************************************************
* COPMUSTAT Munging															  *
*******************************************************************************

use C:/Data/CRSP/20171224_COMP_196001_201612

**************
* Short year *
**************

* When firms change their fiscal year end we may have more than one observation
* for a single fyear but we only want the latest

//duplicates report lpermno fyear

* sort fyear descending within each fiscal year for each firm
sort lpermno fyear -fyr
* find the index for each observation (should be 1)
by lpermno fyear: gen n=_n
* Only keep the first observation in each fiscal year for each firm
* We sorted descending for the month so this will keep the latest observation
keep if n==1
drop n

***************
* Book Equity *
***************

* Value and Profitability measures rely on book equity

* be = seq + txditc - ps
* where ps = pstkrv, pstkl, pstk or 0 in order of preference
* if we are missing be, use ceq + upstk or at - lt in order of preference

gen ps = pstkrv
replace ps = pstkl if ps==.
replace ps = pstk  if ps==.

gen be = seq + txditc - ps

replace be = ceq + upstk if be==.
replace be = at - lt     if be==.



********************
* Operating Profit *
********************

gen op_ok = (cogs!=. | xsga!=. | xint!=.) & be > 0

replace cogs = 0 if cogs==.
replace xsga = 0 if xsga==.
replace xint = 0 if xint==.

gen op = (revt - cogs - xsga - xint) / be


********************************************************
* Operating Profit plus Research and Development Costs *
********************************************************

replace xrd = 0 if xrd==.

gen opr = (revt - cogs - (xsga-xrd) - xint) / be



****************
* Gross Profit *
****************

replace gp = revt - cogs if gp==.

replace gp = gp / at



***************
* Cash Profit *
***************

sort lpermno fyear

by lpermno: gen drect = rect[_n] - rect[_n-1]
by lpermno: gen dinvt = invt[_n] - invt[_n-1]
by lpermno: gen dxpp = xpp[_n] - xpp[_n-1]
by lpermno: gen ddrc = drc[_n] - drc[_n-1]
by lpermno: gen dap = ap[_n] - ap[_n-1]
by lpermno: gen dxacc = xacc[_n] - xacc[_n-1]

replace drect = 0 if drect==.
replace dinvt = 0 if dinvt==.
replace dxpp = 0 if dxpp==.
replace ddrc = 0 if ddrc==.
replace dap = 0 if dap==.
replace dxacc = 0 if dxacc==.

gen acc = - drect - dinvt - dxpp + ddrc + dap + dxacc

gen cp = (revt - cogs - (xsga-xrd) - xint + acc) / be



****************
* Asset Growth *
****************

sort lpermno fyear

by lpermno: gen inv = (at[_n] - at[_n-1]) / at[_n-1]



gen hp = fyear + 1

rename lpermno permno

merge 1:1 permno hp using C:/Data/Thesis/Jun_ME

drop if _merge==2

keep permno hp jun_me be op_ok op opr gp cp inv

describe
summarize

*************************************
save C:/Data/Thesis/BookVars, replace
*************************************


log close
