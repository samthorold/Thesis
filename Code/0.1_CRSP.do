clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/CRSP, replace text

*******************************************************************************
* CRSP Munging																  *
*******************************************************************************

use C:/Data/CRSP/20171224_CRSP_196001_201612

drop ticker comnam cusip spread vwretd

* We only want common shares
keep if shrcd==10 | shrcd==11
drop shrcd

* We only want firms on the NSYE, AMEX and NASDAQ
* AMEX firms are included from 1962 onwards
* NASDAQ firms are included from 1972 (?) onwards
* This is something to keep in mind because it will impact missing data and
* average firm size
keep if exchcd==1 | exchcd==2 | exchcd==3

* We only want one observation at each month for each firm
//duplicates report permno date  * takes a while


* Market Equity is shares outstanding (divided by 1,000) times the price
* We use the absolute price because CRSP denotes prices inferred from the bid
* ask spread with a "-" sign
gen me = abs(prc) * shrout / 1000
label var me "Market Equity"


***********************
* Summary by Exchange *
***********************

describe

/*

* Number of firms on each exchange at each month
preserve
  collapse (count) N=permno, by(exchcd date)
  twoway line N date if exchcd==1 || ///
         line N date if exchcd==2 || ///
		 line N date if exchcd==3,   ///
         legend(label(1 "NYSE") label(2 "AMEX") label(3 "NASDAQ"))
restore

*/

/*

* Average ME of firms on each exchange at each month
preserve
  collapse (mean) me, by(exchcd date)
  twoway line me date if exchcd==1 || ///
         line me date if exchcd==2 || ///
		 line me date if exchcd==3,   ///
         legend(label(1 "NYSE") label(2 "AMEX") label(3 "NASDAQ"))
restore

*/



*****************
* Prior Returns *
*****************

sort permno date
by permno: ///
  gen pr_11_1 = ret[_n-11] + ret[_n-10] + ret[_n- 9] + ret[_n- 8] + ///
                ret[_n- 7] + ret[_n- 6] + ret[_n- 5] + ret[_n- 4] + ///
			    ret[_n- 3] + ret[_n- 2] + ret[_n- 1]
label var pr_11_1 "Prior Return (11,1)"



******************
* Holding Period *
******************

gen hp = year(date)
label var hp "Holding Period"
replace hp = hp-1 if month(date)<7



*********************
* Return Adjustment *
*********************

* The holding period when the firm is "born" will all be missing because
* there is no lag of return adjustment and multiplying by . will make the
* remaining return adjustments .

gen ret_adj = 1+ret

sort permno hp date
by permno hp: replace ret_adj = 1+ret[_n] * ret_adj[_n-1] if month(date)!=7
label var ret_adj "Holding Period Return Adjustment"



**************************
* December Market Equity *
**************************

preserve
  keep if month(date)==12
  keep permno hp me
  rename me dec_me
  replace hp = hp+1
  save C:/Data/Thesis/Dec_ME, replace
restore



**********************
* June Market Equity *
**********************

preserve
  keep if month(date)==6
  keep permno hp me
  rename me jun_me
  replace hp = hp+1
  save C:/Data/Thesis/Jun_ME, replace
restore

merge m:1 permno hp using C:/Data/Thesis/Jun_ME

* keep only firms that had market equity at least once in June
keep if _merge==3
drop _merge


********
* Size *
********

gen size = jun_me
label var size "Market Equity adjusted for holding period return"

sort permno date
by permno: replace size = size[_n-1] * ret_adj[_n-1] if month(date)!=7

drop jun_me

describe
summarize

************************************
save C:/Data/Thesis/Returns, replace
************************************





log close
