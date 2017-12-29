clear
set more off

capture log close
log using C:/Users/samth/Dropbox/Thesis/Code/Logs/QUANTILES, replace text

*******************************************************************************
* Quantiles                                                                   *
*******************************************************************************

use C:/Data/Thesis/Returns

*****************
* Market Equity *
*****************

preserve
  collapse ///
    (count) N=permno ///
    (p5)  me05=me (p10) me10=me (p15) me15=me (p20) me20=me (p25) me25=me ///
    (p30) me30=me (p35) me35=me (p40) me40=me (p45) me45=me (p50) me50=me ///
    (p55) me55=me (p60) me60=me (p65) me65=me (p70) me70=me (p75) me75=me ///
    (p80) me80=me (p85) me85=me (p90) me90=me (p95) me95=me (max) me100=me ///
    if exchcd==1 & me!=., ///
    by(date)
  save C:/Data/Thesis/ME_20Q, replace
restore

************
* Momentum *
************

preserve
  collapse ///
    (count) N=permno ///
    (p5)  pr_11_105=pr_11_1 (p10) pr_11_110=pr_11_1 (p15) pr_11_115=pr_11_1 (p20) pr_11_120=pr_11_1 (p25) pr_11_125=pr_11_1 ///
    (p30) pr_11_130=pr_11_1 (p35) pr_11_135=pr_11_1 (p40) pr_11_140=pr_11_1 (p45) pr_11_145=pr_11_1 (p50) pr_11_150=pr_11_1 ///
    (p55) pr_11_155=pr_11_1 (p60) pr_11_160=pr_11_1 (p65) pr_11_165=pr_11_1 (p70) pr_11_170=pr_11_1 (p75) pr_11_175=pr_11_1 ///
    (p80) pr_11_180=pr_11_1 (p85) pr_11_185=pr_11_1 (p90) pr_11_190=pr_11_1 (p95) pr_11_195=pr_11_1 (max) pr_11_1100=pr_11_1 ///
    if exchcd==1 & pr_11_1!=., ///
    by(date)
  save C:/Data/Thesis/PR_11_1_20Q, replace
restore

merge m:1 permno hp using C:/Data/Thesis/BookVars, nogen keep(match master)


**********************
* June Market Equity *
**********************

preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  jun_me05=jun_me (p10) jun_me10=jun_me (p15) jun_me15=jun_me (p20) jun_me20=jun_me (p25) jun_me25=jun_me ///
    (p30) jun_me30=jun_me (p35) jun_me35=jun_me (p40) jun_me40=jun_me (p45) jun_me45=jun_me (p50) jun_me50=jun_me ///
    (p55) jun_me55=jun_me (p60) jun_me60=jun_me (p65) jun_me65=jun_me (p70) jun_me70=jun_me (p75) jun_me75=jun_me ///
    (p80) jun_me80=jun_me (p85) jun_me85=jun_me (p90) jun_me90=jun_me (p95) jun_me95=jun_me (max) jun_me100=jun_me ///
    if exchcd==1 & jun_me!=., ///
    by(hp)
  save C:/Data/Thesis/Jun_ME_20Q, replace
restore


**********
* Profit *
**********

preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  op05=op (p10) op10=op (p15) op15=op (p20) op20=op (p25) op25=op ///
    (p30) op30=op (p35) op35=op (p40) op40=op (p45) op45=op (p50) op50=op ///
    (p55) op55=op (p60) op60=op (p65) op65=op (p70) op70=op (p75) op75=op ///
    (p80) op80=op (p85) op85=op (p90) op90=op (p95) op95=op (max) op100=op ///
    if exchcd==1 & op_ok, ///
    by(hp)
  save C:/Data/Thesis/OP_20Q, replace
restore


preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  opr05=opr (p10) opr10=opr (p15) opr15=opr (p20) opr20=opr (p25) opr25=opr ///
    (p30) opr30=opr (p35) opr35=opr (p40) opr40=opr (p45) opr45=opr (p50) opr50=opr ///
    (p55) opr55=opr (p60) opr60=opr (p65) opr65=opr (p70) opr70=opr (p75) opr75=opr ///
    (p80) opr80=opr (p85) opr85=opr (p90) opr90=opr (p95) opr95=opr (max) opr100=opr ///
    if exchcd==1 & op_ok, ///
    by(hp)
  save C:/Data/Thesis/OPr_20Q, replace
restore



preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  gp05=gp (p10) gp10=gp (p15) gp15=gp (p20) gp20=gp (p25) gp25=gp ///
    (p30) gp30=gp (p35) gp35=gp (p40) gp40=gp (p45) gp45=gp (p50) gp50=gp ///
    (p55) gp55=gp (p60) gp60=gp (p65) gp65=gp (p70) gp70=gp (p75) gp75=gp ///
    (p80) gp80=gp (p85) gp85=gp (p90) gp90=gp (p95) gp95=gp (max) gp100=gp ///
    if exchcd==1 & gp!=., ///
    by(hp)
  save C:/Data/Thesis/GP_20Q, replace
restore



preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  cp05=cp (p10) cp10=cp (p15) cp15=cp (p20) cp20=cp (p25) cp25=cp ///
    (p30) cp30=cp (p35) cp35=cp (p40) cp40=cp (p45) cp45=cp (p50) cp50=cp ///
    (p55) cp55=cp (p60) cp60=cp (p65) cp65=cp (p70) cp70=cp (p75) cp75=cp ///
    (p80) cp80=cp (p85) cp85=cp (p90) cp90=cp (p95) cp95=cp (max) cp100=cp ///
    if exchcd==1 & op_ok, ///
    by(hp)
  save C:/Data/Thesis/CP_20Q, replace
restore



****************
* Asset Growth *
****************

preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  inv05=inv (p10) inv10=inv (p15) inv15=inv (p20) inv20=inv (p25) inv25=inv ///
    (p30) inv30=inv (p35) inv35=inv (p40) inv40=inv (p45) inv45=inv (p50) inv50=inv ///
    (p55) inv55=inv (p60) inv60=inv (p65) inv65=inv (p70) inv70=inv (p75) inv75=inv ///
    (p80) inv80=inv (p85) inv85=inv (p90) inv90=inv (p95) inv95=inv (max) inv100=inv ///
    if exchcd==1 & inv!=., ///
    by(hp)
  save C:/Data/Thesis/INV_20Q, replace
restore


*********
* Value *
*********

preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  bmal05=bmal (p10) bmal10=bmal (p15) bmal15=bmal (p20) bmal20=bmal (p25) bmal25=bmal ///
    (p30) bmal30=bmal (p35) bmal35=bmal (p40) bmal40=bmal (p45) bmal45=bmal (p50) bmal50=bmal ///
    (p55) bmal55=bmal (p60) bmal60=bmal (p65) bmal65=bmal (p70) bmal70=bmal (p75) bmal75=bmal ///
    (p80) bmal80=bmal (p85) bmal85=bmal (p90) bmal90=bmal (p95) bmal95=bmal (max) bmal100=bmal ///
    if exchcd==1 & jun_me!=. & be>0, ///
    by(hp)
  save C:/Data/Thesis/BM_al_20Q, replace
restore


preserve
  keep if month(date)==7
  collapse ///
    (count) N=permno ///
    (p5)  bmac05=bmac (p10) bmac10=bmac (p15) bmac15=bmac (p20) bmac20=bmac (p25) bmac25=bmac ///
    (p30) bmac30=bmac (p35) bmac35=bmac (p40) bmac40=bmac (p45) bmac45=bmac (p50) bmac50=bmac ///
    (p55) bmac55=bmac (p60) bmac60=bmac (p65) bmac65=bmac (p70) bmac70=bmac (p75) bmac75=bmac ///
    (p80) bmac80=bmac (p85) bmac85=bmac (p90) bmac90=bmac (p95) bmac95=bmac (max) bmac100=bmac ///
    if exchcd==1 & jun_me!=. & be>0, ///
    by(hp)
  save C:/Data/Thesis/BM_ac_20Q, replace
restore


preserve
  collapse ///
    (count) N=permno ///
    (p5)  bmmc05=bmmc (p10) bmmc10=bmmc (p15) bmmc15=bmmc (p20) bmmc20=bmmc (p25) bmmc25=bmmc ///
    (p30) bmmc30=bmmc (p35) bmmc35=bmmc (p40) bmmc40=bmmc (p45) bmmc45=bmmc (p50) bmmc50=bmmc ///
    (p55) bmmc55=bmmc (p60) bmmc60=bmmc (p65) bmmc65=bmmc (p70) bmmc70=bmmc (p75) bmmc75=bmmc ///
    (p80) bmmc80=bmmc (p85) bmmc85=bmmc (p90) bmmc90=bmmc (p95) bmmc95=bmmc (max) bmmc100=bmmc ///
    if exchcd==1 & me!=. & be>0, ///
    by(date)
  save C:/Data/Thesis/BM_mc_20Q, replace
restore










log close

