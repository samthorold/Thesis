---
title: "Test Factors"
author: "Sam Thorold"
output:
  html_document:
    keep_md: true
    df_print: paged
    code_folding: show
    toc: true
    toc_float: true
    number_sections: true
---


```r
knitr::opts_chunk$set(echo = TRUE, fig.height=4)
```

# Definitions and Links

- **[Variables](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html)**
- **[Factors](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_5_factors_2x3.html)**


- [Size breakpoints](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_me_breakpoints.html)
- [Size returns](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_port_form_sz.html)


- [Book-to-Market breakpoints](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_beme_breakpoints.html)
- [Book-to-Market returns](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_form_btm.html)


- [Operating Profit breakpoints](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_op_breakpoints.html)
- [Operating Profit returns](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_port_form_op.html)


- [Investment breakpoints](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_inv_breakpoints.html)
- [Investment returns](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_port_form_inv.html)


- [Momentum breakpoints](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_2-12_breakpoints.html)
- [Momentum returns](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_10_port_form_pr_12_2.html)


```r
library(data.table)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(knitr)
library(rlang)
library(Rsolnp)
```


```r
se = function(x) sd(x, na.rm=TRUE)/sqrt(length(x))

t.stat = function(x) t.test(x)$statistic

sr = function(x) mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE)

stat.summary = function(df){
    return(
        rbind(sapply(df, mean), sapply(df, se), sapply(df, t.stat), sapply(df, sr))
    )
}
```


```r
i = 13   # 1964-07
j = 630  # 2015-12
```

# My Factors


```r
f = fread("C:/Data/Thesis/factors.csv")

colnames(f)
```

```
##  [1] "V1"        "Date"      "HML"       "SMB.HML"   "HMLm"     
##  [6] "SMB.HMLm"  "RMWo"      "SMB.RMWo"  "RMWor"     "SMB.RMWor"
## [11] "RMWg"      "SMB.RMWg"  "RMWc"      "SMB.RMWc"  "CMA"      
## [16] "SMB.CMA"   "WML"       "SMB.WML"   "Rm"        "SMB"
```

```r
f = f[i:j,]  %>% select(
    -c(V1, Date, SMB.HML, SMB.HMLm, SMB.RMWo, SMB.RMWor, SMB.RMWg, SMB.RMWc, SMB.CMA, SMB.WML)
)

rbind(sapply(f, mean), sapply(f, se), sapply(f, t.stat), sapply(f, sr))  %>% round(2)
```

```
##       HML HMLm RMWo RMWor RMWg RMWc  CMA  WML   Rm  SMB
## [1,] 0.29 0.34 0.27  0.35 0.25 0.44 0.28 0.64 0.47 0.26
## [2,] 0.12 0.14 0.09  0.07 0.09 0.06 0.07 0.16 0.18 0.12
## [3,] 2.52 2.39 3.05  5.10 2.77 7.61 3.78 3.95 2.62 2.17
## [4,] 0.10 0.10 0.12  0.21 0.11 0.31 0.15 0.16 0.11 0.09
```

# French Dartmouth Web Factors


```r
ff = fread("C:/Data/FrenchDartmouth/FF_factors.csv")

ff = ff[i:j,] %>% select(-c(Date))
setnames(ff,
         c("Mkt-RF", "SMB", "HML", "RMW", "CMA"),
         c("FF.Rm", "FF.SMB", "FF.HML", "FF.RMW", "FF.CMA"))

rbind(sapply(ff, mean), sapply(ff, se), sapply(ff, t.stat))  %>% round(2)
```

```
##      FF.Rm FF.SMB FF.HML FF.RMW FF.CMA    RF
## [1,]  0.49   0.27   0.33   0.25   0.30  0.40
## [2,]  0.18   0.12   0.11   0.09   0.08  0.01
## [3,]  2.69   2.17   2.92   2.77   3.70 37.91
```


```r
f = cbind(f, ff)  # i:j
```


```r
fit = lm(FF.HML~FF.Rm+FF.SMB+FF.RMW+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"-0.02","2":"0.08","3":"-0.27","4":"0.79","_rn_":"(Intercept)"},{"1":"0.02","2":"0.02","3":"0.84","4":"0.40","_rn_":"FF.Rm"},{"1":"0.03","2":"0.03","3":"1.02","4":"0.31","_rn_":"FF.SMB"},{"1":"0.14","2":"0.04","3":"3.69","4":"0.00","_rn_":"FF.RMW"},{"1":"1.00","2":"0.04","3":"22.91","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
f$HMLo = fit$residuals

fit = lm(FF.HML~FF.Rm+FF.SMB+RMWor+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.15","2":"0.09","3":"1.81","4":"0.07","_rn_":"(Intercept)"},{"1":"-0.01","2":"0.02","3":"-0.36","4":"0.72","_rn_":"FF.Rm"},{"1":"-0.02","2":"0.03","3":"-0.83","4":"0.40","_rn_":"FF.SMB"},{"1":"-0.24","2":"0.05","3":"-4.93","4":"0.00","_rn_":"RMWor"},{"1":"0.91","2":"0.05","3":"19.98","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
fit = lm(FF.HML~FF.Rm+FF.SMB+RMWg+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.21","2":"0.08","3":"2.78","4":"0.01","_rn_":"(Intercept)"},{"1":"-0.01","2":"0.02","3":"-0.72","4":"0.47","_rn_":"FF.Rm"},{"1":"0.04","2":"0.03","3":"1.66","4":"0.10","_rn_":"FF.SMB"},{"1":"-0.45","2":"0.04","3":"-11.90","4":"0.00","_rn_":"RMWg"},{"1":"0.75","2":"0.04","3":"16.88","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
fit = lm(FF.HML~FF.Rm+FF.SMB+RMWc+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.25","2":"0.09","3":"2.87","4":"0.00","_rn_":"(Intercept)"},{"1":"-0.03","2":"0.02","3":"-1.56","4":"0.12","_rn_":"FF.Rm"},{"1":"-0.04","2":"0.03","3":"-1.40","4":"0.16","_rn_":"FF.SMB"},{"1":"-0.39","2":"0.06","3":"-6.49","4":"0.00","_rn_":"RMWc"},{"1":"0.91","2":"0.04","3":"20.94","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Factor Correlation


```r
cor(f) %>% round(2)
```

```
##          HML  HMLm  RMWo RMWor  RMWg  RMWc   CMA   WML    Rm   SMB FF.Rm
## HML     1.00  0.79  0.13 -0.33 -0.59 -0.21  0.65 -0.22 -0.34 -0.13 -0.34
## HMLm    0.79  1.00  0.03 -0.37 -0.50 -0.33  0.48 -0.68 -0.17 -0.07 -0.17
## RMWo    0.13  0.03  1.00  0.78  0.23  0.62 -0.15  0.01 -0.23 -0.35 -0.22
## RMWor  -0.33 -0.37  0.78  1.00  0.62  0.77 -0.37  0.16 -0.01 -0.13  0.00
## RMWg   -0.59 -0.50  0.23  0.62  1.00  0.54 -0.42  0.13  0.15  0.17  0.15
## RMWc   -0.21 -0.33  0.62  0.77  0.54  1.00 -0.15  0.24 -0.24 -0.24 -0.23
## CMA     0.65  0.48 -0.15 -0.37 -0.42 -0.15  1.00 -0.07 -0.35 -0.04 -0.35
## WML    -0.22 -0.68  0.01  0.16  0.13  0.24 -0.07  1.00 -0.04  0.05 -0.04
## Rm     -0.34 -0.17 -0.23 -0.01  0.15 -0.24 -0.35 -0.04  1.00  0.27  1.00
## SMB    -0.13 -0.07 -0.35 -0.13  0.17 -0.24 -0.04  0.05  0.27  1.00  0.26
## FF.Rm  -0.34 -0.17 -0.22  0.00  0.15 -0.23 -0.35 -0.04  1.00  0.26  1.00
## FF.SMB -0.13 -0.07 -0.35 -0.13  0.18 -0.24 -0.05  0.05  0.28  0.99  0.27
## FF.HML  0.95  0.80  0.08 -0.34 -0.60 -0.25  0.64 -0.24 -0.27 -0.08 -0.27
## FF.RMW  0.15  0.03  0.97  0.74  0.24  0.61 -0.14  0.03 -0.24 -0.35 -0.24
## FF.CMA  0.71  0.53 -0.02 -0.29 -0.46 -0.11  0.92 -0.08 -0.39 -0.11 -0.39
## RF      0.09  0.06  0.01 -0.04 -0.05  0.04  0.06  0.03 -0.08 -0.05 -0.08
## HMLo    0.62  0.60  0.00 -0.29 -0.42 -0.31  0.00 -0.26  0.00  0.01  0.00
##        FF.SMB FF.HML FF.RMW FF.CMA    RF  HMLo
## HML     -0.13   0.95   0.15   0.71  0.09  0.62
## HMLm    -0.07   0.80   0.03   0.53  0.06  0.60
## RMWo    -0.35   0.08   0.97  -0.02  0.01  0.00
## RMWor   -0.13  -0.34   0.74  -0.29 -0.04 -0.29
## RMWg     0.18  -0.60   0.24  -0.46 -0.05 -0.42
## RMWc    -0.24  -0.25   0.61  -0.11  0.04 -0.31
## CMA     -0.05   0.64  -0.14   0.92  0.06  0.00
## WML      0.05  -0.24   0.03  -0.08  0.03 -0.26
## Rm       0.28  -0.27  -0.24  -0.39 -0.08  0.00
## SMB      0.99  -0.08  -0.35  -0.11 -0.05  0.01
## FF.Rm    0.27  -0.27  -0.24  -0.39 -0.08  0.00
## FF.SMB   1.00  -0.09  -0.35  -0.12 -0.05  0.00
## FF.HML  -0.09   1.00   0.08   0.70  0.08  0.71
## FF.RMW  -0.35   0.08   1.00  -0.03  0.00  0.00
## FF.CMA  -0.12   0.70  -0.03   1.00  0.06  0.00
## RF      -0.05   0.08   0.00   0.06  1.00  0.05
## HMLo     0.00   0.71   0.00   0.00  0.05  1.00
```

# Maximum Sharpe Ratio

$$
E(R_p) = w^T R
$$


```r
ER = function(w, R) t(w)%*%R
```

$$
Var(R_p) = w^T \Omega w
$$


```r
VAR = function(w, covar.mat) t(w)%*%covar.mat%*%w
SD = function(w, covar.mat) sqrt(VAR(w, covar.mat))
```


```r
# Objective function passed to one of R's minimizing thingies
SR.obj = function(w, factor.mat, covar.mat){

    w = matrix(w, nrow=length(w), ncol=1)

    R = matrix(colMeans(factor.mat), nrow=length(w), ncol=1)
    # -ve sign because we want to maximize and most optimizers minimize
    Rp = -ER(w, R)

    stdev = SD(w, covar.mat)

    return(Rp/stdev)
}
```


```r
SR.sq.mat = function(R, covar.mat) t(R)%*%solve(covar.mat)%*%R
```


```r
w.star.mat = function(Rp, R, covar.mat){
    return(((Rp/(SR.sq.mat(R, covar.mat)))[1])*solve(covar.mat)%*%R)
}
```


```r
# The objective function and the constraint function have to take the same
# arguments
sum.constraint = function(w, factor.mat, covar.mat){
    return(sum(w))
}
```

# Fama and French (2016) Table 3

## Rm, SMB, HML, RMWor, CMA


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, RMWor, FF.CMA)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + FF.HML + RMWor + FF.CMA, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + FF.HML + RMWor + FF.CMA, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.HML ~ FF.Rm + FF.SMB + RMWor + FF.CMA, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWor ~ FF.Rm + FF.SMB + FF.HML + FF.CMA, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + FF.HML + RMWor, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWor = cbind(Sh2, Mkt, Size, Val, Prof, Inv)
rownames(RMWor) = c("Mkt, SMB, HML, RMWor, CMA")
# round(RMWor, 3)
```

## Rm, SMB, HML, RMWc, CMA


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, RMWc, FF.CMA)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + FF.HML + RMWc + FF.CMA, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + FF.HML + RMWc + FF.CMA, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.HML ~ FF.Rm + FF.SMB + RMWc + FF.CMA, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWc ~ FF.Rm + FF.SMB + FF.HML + FF.CMA, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + FF.HML + RMWc, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWc = cbind(Sh2, Mkt, Size, Val, Prof, Inv)
rownames(RMWc) = c("Mkt, SMB, HML, RMWc, CMA")
res = rbind(RMWor, RMWc)
# round(res, 3)
```

## Rm, SMB, HML, RMWg, CMA


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, RMWg, FF.CMA)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + FF.HML + RMWg + FF.CMA, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + FF.HML + RMWg + FF.CMA, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.HML ~ FF.Rm + FF.SMB + RMWg + FF.CMA, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWg ~ FF.Rm + FF.SMB + FF.HML + FF.CMA, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + FF.HML + RMWg, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWg = cbind(Sh2, Mkt, Size, Val, Prof, Inv)
rownames(RMWg) = c("Mkt, SMB, HML, RMWg, CMA")
res = rbind(RMWor, RMWc, RMWg)
# round(res, 3)
```

## Rm, SMB, HML, RMWor, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, RMWor, FF.CMA, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + FF.HML + RMWor + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + FF.HML + RMWor + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.HML ~ FF.Rm + FF.SMB + RMWor + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWor ~ FF.Rm + FF.SMB + FF.HML + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + FF.HML + RMWor + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + FF.HML + RMWor + FF.CMA, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Mom = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWor = cbind(Sh2, Mkt, Size, Val, Prof, Inv, Mom)
rownames(RMWor) = c("Mkt, SMB, HML, RMWor, CMA, WML")
res = rbind(cbind(res, NA), RMWor)
colnames(res) = c("Sh2", "Mkt", "Size", "Val", "Prof", "Inv", "Mom")
# round(res, 3)
```

## Rm, SMB, HML, RMWc, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, RMWc, FF.CMA, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + FF.HML + RMWc + FF.CMA + WML, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + FF.HML + RMWc + FF.CMA + WML, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.HML ~ FF.Rm + FF.SMB + RMWc + FF.CMA + WML, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWc ~ FF.Rm + FF.SMB + FF.HML + FF.CMA + WML, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + FF.HML + RMWc + WML, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + FF.HML + RMWc + FF.CMA, data=factor.mat)
Mom = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWc = cbind(Sh2, Mkt, Size, Val, Prof, Inv, Mom)
rownames(RMWc) = c("Mkt, SMB, HML, RMWc, CMA, WML")
res = rbind(res, RMWc)
# round(res, 3)
```

## Rm, SMB, HML, RMWg, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, RMWg, FF.CMA, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + FF.HML + RMWg + FF.CMA + WML, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + FF.HML + RMWg + FF.CMA + WML, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.HML ~ FF.Rm + FF.SMB + RMWg + FF.CMA + WML, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWg ~ FF.Rm + FF.SMB + FF.HML + FF.CMA + WML, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + FF.HML + RMWg + WML, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + FF.HML + RMWg + FF.CMA, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWg = cbind(Sh2, Mkt, Size, Val, Prof, Inv, Mom)
rownames(RMWg) = c("Mkt, SMB, HML, RMWg, CMA, WML")
res = rbind(res, RMWg)
# round(res, 3)
```

## Rm, SMB, HMLm, RMWor, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, RMWor, FF.CMA, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + HMLm + RMWor + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + HMLm + RMWor + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(HMLm ~ FF.Rm + FF.SMB + RMWor + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWor ~ FF.Rm + FF.SMB + HMLm + FF.CMA + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + HMLm + RMWor + WML, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + HMLm + RMWor + FF.CMA, data=factor.mat)
# round(coef(fit), 3)
# round(sd(fit$residuals), 3)
Mom = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWor = cbind(Sh2, Mkt, Size, Val, Prof, Inv, Mom)
rownames(RMWor) = c("Mkt, SMB, HMLm, RMWor, CMA, WML")
res = rbind(res, RMWor)
# round(res, 3)
```

## Rm, SMB, HMLm, RMWc, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, RMWc, FF.CMA, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + HMLm + RMWc + FF.CMA + WML, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + HMLm + RMWc + FF.CMA + WML, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(HMLm ~ FF.Rm + FF.SMB + RMWc + FF.CMA + WML, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWc ~ FF.Rm + FF.SMB + HMLm + FF.CMA + WML, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + HMLm + RMWc + WML, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + HMLm + RMWc + FF.CMA, data=factor.mat)
Mom = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWc = cbind(Sh2, Mkt, Size, Val, Prof, Inv, Mom)
rownames(RMWc) = c("Mkt, SMB, HMLm, RMWc, CMA, WML")
res = rbind(res, RMWc)
# round(res, 3)
```

## Rm, SMB, HMLm, RMWc, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, RMWc, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + HMLm + RMWc + WML, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + HMLm + RMWc + WML, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(HMLm ~ FF.Rm + FF.SMB + RMWc + WML, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWc ~ FF.Rm + FF.SMB + HMLm + WML, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + HMLm + RMWc, data=factor.mat)
Mom = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWc = cbind(Sh2, Mkt, Size, Val, Prof, NA, Mom)
rownames(RMWc) = c("Mkt, SMB, HMLm, RMWc, WML")
res = rbind(res, RMWc)
# round(res, 3)
```

## Rm, SMB, HMLm, RMWg, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, RMWg, FF.CMA, WML)
factor.mat = factor.mat

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

# Max Sharpe Ratio
Sh2 = rbind(SR.sq.mat(R, covar.mat), sqrt(SR.sq.mat(R, covar.mat)[1]))[1]

# Marginal contributions from spanning regression

fit = lm(FF.Rm ~ FF.SMB + HMLm + RMWg + FF.CMA + WML, data=factor.mat)
Mkt = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.SMB ~ FF.Rm + HMLm + RMWg + FF.CMA + WML, data=factor.mat)
Size = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(HMLm ~ FF.Rm + FF.SMB + RMWg + FF.CMA + WML, data=factor.mat)
Val = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(RMWg ~ FF.Rm + FF.SMB + HMLm + FF.CMA + WML, data=factor.mat)
Prof = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(FF.CMA ~ FF.Rm + FF.SMB + HMLm + RMWg + WML, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

fit = lm(WML ~ FF.Rm + FF.SMB + HMLm + RMWg + FF.CMA, data=factor.mat)
Inv = (coef(fit)["(Intercept)"] / sd(fit$residuals))^2

RMWg = cbind(Sh2, Mkt, Size, Val, Prof, Inv, Mom)
rownames(RMWg) = c("Mkt, SMB, HMLm, RMWg, CMA, WML")
res = rbind(res, RMWg)
# round(res, 3) %>% as.data.frame
```

## Results


```r
round(res, 3) %>% as.data.frame
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Sh2"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Mkt"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Size"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Val"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Prof"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Inv"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Mom"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.154","2":"0.035","3":"0.012","4":"0.006","5":"0.095","6":"0.032","7":"NA","_rn_":"Mkt, SMB, HML, RMWor, CMA"},{"1":"0.251","2":"0.072","3":"0.023","4":"0.017","5":"0.192","6":"0.022","7":"NA","_rn_":"Mkt, SMB, HML, RMWc, CMA"},{"1":"0.114","2":"0.031","3":"0.001","4":"0.014","5":"0.055","6":"0.026","7":"NA","_rn_":"Mkt, SMB, HML, RMWg, CMA"},{"1":"0.180","2":"0.040","3":"0.009","4":"0.013","5":"0.084","6":"0.026","7":"0.026","_rn_":"Mkt, SMB, HML, RMWor, CMA, WML"},{"1":"0.264","2":"0.075","3":"0.019","4":"0.022","5":"0.168","6":"0.018","7":"0.013","_rn_":"Mkt, SMB, HML, RMWc, CMA, WML"},{"1":"0.153","2":"0.038","3":"0.000","4":"0.026","5":"0.056","6":"0.039","7":"0.013","_rn_":"Mkt, SMB, HML, RMWg, CMA, WML"},{"1":"0.260","2":"0.045","3":"0.009","4":"0.093","5":"0.115","6":"0.006","7":"0.103","_rn_":"Mkt, SMB, HMLm, RMWor, CMA, WML"},{"1":"0.355","2":"0.085","3":"0.020","4":"0.114","5":"0.210","6":"0.002","7":"0.094","_rn_":"Mkt, SMB, HMLm, RMWc, CMA, WML"},{"1":"0.353","2":"0.084","3":"0.020","4":"0.204","5":"0.209","6":"NA","7":"0.128","_rn_":"Mkt, SMB, HMLm, RMWc, WML"},{"1":"0.242","2":"0.042","3":"0.000","4":"0.115","5":"0.096","6":"0.134","7":"0.128","_rn_":"Mkt, SMB, HMLm, RMWg, CMA, WML"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Anomalies

Downloaded from Dr. Kenneth French's
[website](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Research)

Unconditional sorts (US)

* 25 Size BM
* 25 Size BMm
* 25 Size OP
* 25 Size GP
* 25 Size CP
* 25 Size INV
* 25 Size Mom
* 25 Size $\beta$
* 35 Size Net Share Issues (quintiles plus 0 and negative NI)
* 25 Size Variance
* 25 Size Residual Variance
* 25 Size Accruals
* (25 BM OP)
* (25 BM INV)
* (25 OP INV)

Conditional on Size sorts (US)

* 32 Size BM OP
* 32 Size BM INV
* 32 Size OP INV


```r
a = fread("C:/Data/FrenchDartmouth/25_Portfolios_5x5.CSV")[i:j,]

bmm = fread("C:/Data/Thesis/25_L1ME_BMM_Returns.CSV")[,-1]
bmm$Date = as.integer(substr(bmm$Date, 1, 4))*100 + as.integer(substr(bmm$Date, 6, 7))
a = left_join(a, bmm, by="Date")

a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_OP_5x5.CSV"), by="Date")

gp = fread("C:/Data/Thesis/25_Size_GP_Returns.CSV")[,-1]
gp$Date = as.integer(substr(gp$Date, 1, 4))*100 + as.integer(substr(gp$Date, 6, 7))
a = left_join(a, gp, by="Date")

cp = fread("C:/Data/Thesis/25_Size_CP_Returns.CSV")[,-1]
cp$Date = as.integer(substr(cp$Date, 1, 4))*100 + as.integer(substr(cp$Date, 6, 7))
a = left_join(a, cp, by="Date")

a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_INV_5x5.CSV"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_Prior_12_2.CSV"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_BETA_5x5.csv"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_NI_5x5.csv"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_VAR_5x5.csv"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_RESVAR_5x5.csv"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_ME_AC_5x5.csv"), by="Date")
#a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_BEME_OP_5x5.csv"), by="Date")
#a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_BEME_INV_5x5.csv"), by="Date")
#a = left_join(a, fread("C:/Data/FrenchDartmouth/25_Portfolios_OP_INV_5x5.csv"), by="Date")

a = left_join(a, fread("C:/Data/FrenchDartmouth/32_Portfolios_ME_BEME_OP_2x4x4.CSV"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/32_Portfolios_ME_BEME_INV_2x4x4.CSV"), by="Date")
a = left_join(a, fread("C:/Data/FrenchDartmouth/32_Portfolios_ME_OP_INV_2x4x4.CSV"), by="Date")

dim(a)
```

```
## [1] 618 407
```

```r
summary(a$Date)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  196400  197700  199000  199000  200300  201500
```


```r
a = a[,-1]
```

## FF.Rm+FF.SMB+HMLo+FF.RMW+FF.CMA


```r
cs = c()
ts = c()
reg.resid = c()

for (p in colnames(a)){  # not Date column
    model = paste0("`", p, "` -RF ~ FF.Rm+FF.SMB+HMLo+FF.RMW+FF.CMA")
    fit = lm(model, data=cbind(f, a))
    cs = rbind(cs, coef(fit))
    ts = rbind(ts, coef(summary(fit))[,3])
    reg.resid = cbind(reg.resid, fit$residuals)
}
colnames(cs) = c("a", "Rm", "s", "h", "r", "c")
colnames(ts) = c("a", "Rm", "s", "h", "r", "c")
colnames(ts) = paste0("t(", colnames(ts), ")")

regs = as.data.frame(cbind(cs, ts))
rownames(regs) = colnames(a)
write.csv(regs, "C:/Data/Thesis/FF-Rm_FF-SMB_HMLo_FF-RMW_FF-CMA_Anomalies.csv")
round(summary(abs(regs$a)), 2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.05    0.09    0.12    0.16    0.90
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 129
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.610133
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1189032
```

## FF.Rm+FF.SMB+FF.HML+RMWor+FF.CMA


```r
cs = c()
ts = c()
reg.resid = c()

for (p in colnames(a)){
    model = paste0("`", p, "` -RF~ FF.Rm+FF.SMB+FF.HML+RMWor+FF.CMA")
    fit = lm(model, data=cbind(f, a))
    cs = rbind(cs, coef(fit))
    ts = rbind(ts, coef(summary(fit))[,3])
    reg.resid = cbind(reg.resid, fit$residuals)
}
colnames(cs) = c("a", "Rm", "s", "h", "r", "c")
colnames(ts) = c("a", "Rm", "s", "h", "r", "c")
colnames(ts) = paste0("t(", colnames(ts), ")")

regs = as.data.frame(cbind(cs, ts))
rownames(regs) = colnames(a)
write.csv(regs, "C:/Data/Thesis/FF-Rm_FF-SMB_FF-HML_RMWor_FF-CMA_Anomalies.csv")
round(summary(abs(regs$a)), 2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.04    0.09    0.11    0.14    0.76
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 96
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.560016
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1079267
```

## FF.Rm+FF.SMB+FF.HML+RMWc+FF.CMA


```r
cs = c()
ts = c()
reg.resid = c()

for (p in colnames(a)){
    model = paste0("`", p, "` -RF~ FF.Rm+FF.SMB+FF.HML+RMWc+FF.CMA")
    fit = lm(model, data=cbind(f, a))
    cs = rbind(cs, coef(fit))
    ts = rbind(ts, coef(summary(fit))[,3])
    reg.resid = cbind(reg.resid, fit$residuals)
}
colnames(cs) = c("a", "Rm", "s", "h", "r", "c")
colnames(ts) = c("a", "Rm", "s", "h", "r", "c")
colnames(ts) = paste0("t(", colnames(ts), ")")

regs = as.data.frame(cbind(cs, ts))
rownames(regs) = colnames(a)
write.csv(regs, "C:/Data/Thesis/FF-Rm_FF-SMB_FF-HML_RMWc_FF-CMA_Anomalies.csv")
round(summary(abs(regs$a)), 2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.04    0.08    0.10    0.13    0.71
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 74
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.462473
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.100076
```

## FF.Rm+FF.SMB+HMLm+RMWg+FF.CMA+WML


```r
cs = c()
ts = c()
reg.resid = c()

for (p in colnames(a)){
    model = paste0("`", p, "` -RF~ FF.Rm+FF.SMB+HMLm+RMWg+FF.CMA+WML")
    fit = lm(model, data=cbind(f, a))
    cs = rbind(cs, coef(fit))
    ts = rbind(ts, coef(summary(fit))[,3])
    reg.resid = cbind(reg.resid, fit$residuals)
}
colnames(cs) = c("a", "Rm", "s", "h", "r", "c", "w")
colnames(ts) = c("a", "Rm", "s", "h", "r", "c", "w")
colnames(ts) = paste0("t(", colnames(ts), ")")

regs = as.data.frame(cbind(cs, ts))
rownames(regs) = colnames(a)
write.csv(regs, "C:/Data/Thesis/FF-Rm_FF-SMB_HMLm_RMWg_FF-CMA_WML_Anomalies.csv")
round(summary(abs(regs$a)), 2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.04    0.08    0.10    0.13    0.95
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 85
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.571443
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1041996
```

## FF.Rm+FF.SMB+HMLm+RMWc+FF.CMA+WML


```r
cs = c()
ts = c()

for (p in colnames(a)){
    model = paste0("`", p, "` -RF~ FF.Rm+FF.SMB+HMLm+RMWc+FF.CMA+WML")
    fit = lm(model, data=cbind(f, a))
    cs = rbind(cs, coef(fit))
    ts = rbind(ts, coef(summary(fit))[,3])
}
colnames(cs) = c("a", "Rm", "s", "h", "r", "c", "w")
colnames(ts) = c("a", "Rm", "s", "h", "r", "c", "w")
colnames(ts) = paste0("t(", colnames(ts), ")")

regs = as.data.frame(cbind(cs, ts))
rownames(regs) = colnames(a)
write.csv(regs, "C:/Data/Thesis/FF-Rm_FF-SMB_HMLm_RMWc_FF-CMA_WML_Anomalies.csv")
round(summary(abs(regs$a)), 2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.04    0.09    0.11    0.14    0.80
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 76
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.746238
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1063653
```

## FF.Rm+FF.SMB+HMLm+RMWc+WML


```r
cs = c()
ts = c()
reg.resid = c()

for (p in colnames(a)){
    model = paste0("`", p, "` -RF~ FF.Rm+FF.SMB+HMLm+RMWc+WML")
    fit = lm(model, data=cbind(f, a))
    cs = rbind(cs, coef(fit))
    ts = rbind(ts, coef(summary(fit))[,3])
    reg.resid = cbind(reg.resid, fit$residuals)
}
colnames(cs) = c("a", "Rm", "s", "h", "r", "w")
colnames(ts) = c("a", "Rm", "s", "h", "r", "w")
colnames(ts) = paste0("t(", colnames(ts), ")")

regs = as.data.frame(cbind(cs, ts))
rownames(regs) = colnames(a)
write.csv(regs, "C:/Data/Thesis/FF-Rm_FF-SMB_HMLm_RMWc_WML_Anomalies.csv")
round(summary(abs(regs$a)), 2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.04    0.09    0.11    0.14    0.81
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 78
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.446134
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1053299
```

