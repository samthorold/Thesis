clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/Spanning_Regs, replace text

local path C:/Users/samth/Dropbox/Thesis/Tex/Tables/
local table_options "b(%9.2f) t(%9.2f) ar2 nostar compress"

*******************************************************************************
* Spanning Regs                                                               *
*******************************************************************************

use C:/Data/Thesis/Factors, clear

* FF2015

reg rm smb hml rmw cma
estimates store FF2015_Mkt

reg smb rm hml rmw cma
estimates store FF2015_Size

reg hml rm smb rmw cma
estimates store FF2015_Val

reg rmw rm smb hml cma
estimates store FF2015_Prof

reg cma rm smb hml rmw
estimates store FF2015_Inv

* FF2016a

reg rm smb hml RMWr cma
estimates store FF2016a_Mkt

reg smb rm hml RMWr cma
estimates store FF2016a_Size

reg hml rm smb RMWr cma
estimates store FF2016a_Val

reg RMWr rm smb hml cma
estimates store FF2016a_Prof

reg cma rm smb hml RMWr
estimates store FF2016a_Inv

* FF2016b

reg rm smb hml RMWc cma
estimates store FF2016b_Mkt

reg smb rm hml RMWc cma
estimates store FF2016b_Size

reg hml rm smb RMWc cma
estimates store FF2016b_Val

reg RMWc rm smb hml cma
estimates store FF2016b_Prof

reg cma rm smb hml RMWc
estimates store FF2016b_Inv

* BS2015

reg rm smb HMLm wml RMWc
estimates store BS2015_Mkt

reg smb rm HMLm wml RMWc
estimates store BS2015_Size

reg HMLm rm smb wml RMWc
estimates store BS2015_Val

reg RMWc rm smb HMLm wml
estimates store BS2015_Prof

reg wml rm smb HMLm RMWc
estimates store BS2015_Mom

reg cma rm smb HMLm wml RMWc
estimates store BS2015_Inv

* Investment

reg cma HMLm wml
estimates store inv1

reg cma HMLm
estimates store inv2

reg cma wml
estimates store inv3

reg cma rm RMWc
estimates store inv4

reg cma rm HMLm RMWc
estimates store inv5

reg cma rm wml RMWc
estimates store inv6

* Tables

esttab *Val,      `table_options'

esttab FF2015*,   `table_options'

esttab FF2016a*,  `table_options'

esttab FF2016b*,  `table_options'

esttab BS2015* using `path'BS2015_Spanning.tex, `table_options' replace

esttab inv* BS2015_Inv using `path'Inv.tex,     `table_options' replace

log close
