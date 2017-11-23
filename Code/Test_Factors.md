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
f = f[1:630,]  %>% select(
    -c(V1, Date, SMB.HML, SMB.HMLm, SMB.RMWo, SMB.RMWor, SMB.RMWg, SMB.RMWc, SMB.CMA, SMB.WML)
)

rbind(sapply(f, mean), sapply(f, se), sapply(f, t.stat), sapply(f, sr))  %>% round(2)
```

```
##       HML HMLm RMWo RMWor RMWg RMWc  CMA  WML   Rm  SMB
## [1,] 0.29 0.34 0.27  0.35 0.25 0.43 0.27 0.65 0.49 0.25
## [2,] 0.11 0.14 0.09  0.07 0.09 0.06 0.07 0.16 0.18 0.12
## [3,] 2.52 2.46 3.05  5.10 2.77 7.60 3.78 4.04 2.74 2.17
## [4,] 0.10 0.10 0.12  0.20 0.11 0.30 0.15 0.16 0.11 0.09
```

# French Dartmouth Web Factors


```r
ff = fread("C:/Data/FrenchDartmouth/FF_factors.csv")

ff = ff[1:630,] %>% select(-c(Date))
setnames(ff,
         c("Mkt-RF", "SMB", "HML", "RMW", "CMA"),
         c("FF.Rm", "FF.SMB", "FF.HML", "FF.RMW", "FF.CMA"))

rbind(sapply(ff, mean), sapply(ff, se), sapply(ff, t.stat))  %>% round(2)
```

```
##      FF.Rm FF.SMB FF.HML FF.RMW FF.CMA    RF
## [1,]  0.50   0.25   0.35   0.24   0.30  0.40
## [2,]  0.18   0.12   0.11   0.09   0.08  0.01
## [3,]  2.82   2.09   3.09   2.75   3.74 38.35
```


```r
f = cbind(f, ff)
```

# Factor Correlation


```r
cor(f[1:630,]) %>% round(2)
```

```
##          HML  HMLm  RMWo RMWor  RMWg  RMWc   CMA   WML    Rm   SMB FF.Rm
## HML     1.00  0.79  0.13 -0.33 -0.59 -0.21  0.65 -0.22 -0.34 -0.13 -0.34
## HMLm    0.79  1.00  0.03 -0.37 -0.50 -0.33  0.48 -0.68 -0.17 -0.07 -0.17
## RMWo    0.13  0.03  1.00  0.78  0.23  0.62 -0.15  0.01 -0.23 -0.35 -0.22
## RMWor  -0.33 -0.37  0.78  1.00  0.62  0.77 -0.36  0.16 -0.01 -0.13  0.00
## RMWg   -0.59 -0.50  0.23  0.62  1.00  0.54 -0.42  0.13  0.15  0.17  0.15
## RMWc   -0.21 -0.33  0.62  0.77  0.54  1.00 -0.14  0.24 -0.24 -0.24 -0.23
## CMA     0.65  0.48 -0.15 -0.36 -0.42 -0.14  1.00 -0.07 -0.35 -0.04 -0.35
## WML    -0.22 -0.68  0.01  0.16  0.13  0.24 -0.07  1.00 -0.04  0.05 -0.04
## Rm     -0.34 -0.17 -0.23 -0.01  0.15 -0.24 -0.35 -0.04  1.00  0.27  1.00
## SMB    -0.13 -0.07 -0.35 -0.13  0.17 -0.24 -0.04  0.05  0.27  1.00  0.26
## FF.Rm  -0.34 -0.17 -0.22  0.00  0.15 -0.23 -0.35 -0.04  1.00  0.26  1.00
## FF.SMB -0.13 -0.07 -0.35 -0.13  0.18 -0.24 -0.05  0.05  0.28  0.99  0.27
## FF.HML  0.95  0.80  0.08 -0.34 -0.60 -0.25  0.64 -0.23 -0.26 -0.08 -0.27
## FF.RMW  0.15  0.02  0.96  0.74  0.24  0.61 -0.14  0.03 -0.24 -0.35 -0.23
## FF.CMA  0.70  0.53 -0.02 -0.29 -0.46 -0.11  0.92 -0.08 -0.39 -0.11 -0.39
## RF      0.09  0.06  0.01 -0.04 -0.05  0.04  0.06  0.03 -0.08 -0.05 -0.08
##        FF.SMB FF.HML FF.RMW FF.CMA    RF
## HML     -0.13   0.95   0.15   0.70  0.09
## HMLm    -0.07   0.80   0.02   0.53  0.06
## RMWo    -0.35   0.08   0.96  -0.02  0.01
## RMWor   -0.13  -0.34   0.74  -0.29 -0.04
## RMWg     0.18  -0.60   0.24  -0.46 -0.05
## RMWc    -0.24  -0.25   0.61  -0.11  0.04
## CMA     -0.05   0.64  -0.14   0.92  0.06
## WML      0.05  -0.23   0.03  -0.08  0.03
## Rm       0.28  -0.26  -0.24  -0.39 -0.08
## SMB      0.99  -0.08  -0.35  -0.11 -0.05
## FF.Rm    0.27  -0.27  -0.23  -0.39 -0.08
## FF.SMB   1.00  -0.09  -0.35  -0.12 -0.05
## FF.HML  -0.09   1.00   0.07   0.70  0.08
## FF.RMW  -0.35   0.07   1.00  -0.03  0.01
## FF.CMA  -0.12   0.70  -0.03   1.00  0.06
## RF      -0.05   0.08   0.01   0.06  1.00
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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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
factor.mat = factor.mat[1:630,]  # through 2015-12

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

## Rm, SMB, HMLm, RMWg, CMA, WML


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, RMWg, FF.CMA, WML)
factor.mat = factor.mat[1:630,]  # through 2015-12

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
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Sh2"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Mkt"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Size"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Val"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Prof"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Inv"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Mom"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.154","2":"0.037","3":"0.010","4":"0.007","5":"0.094","6":"0.030","7":"NA","_rn_":"Mkt, SMB, HML, RMWor, CMA"},{"1":"0.250","2":"0.075","3":"0.021","4":"0.018","5":"0.190","6":"0.020","7":"NA","_rn_":"Mkt, SMB, HML, RMWc, CMA"},{"1":"0.116","2":"0.033","3":"0.000","4":"0.016","5":"0.056","6":"0.025","7":"NA","_rn_":"Mkt, SMB, HML, RMWg, CMA"},{"1":"0.182","2":"0.042","3":"0.008","4":"0.015","5":"0.083","6":"0.024","7":"0.028","_rn_":"Mkt, SMB, HML, RMWor, CMA, WML"},{"1":"0.265","2":"0.077","3":"0.018","4":"0.025","5":"0.166","6":"0.017","7":"0.014","_rn_":"Mkt, SMB, HML, RMWc, CMA, WML"},{"1":"0.157","2":"0.039","3":"0.000","4":"0.029","5":"0.057","6":"0.041","7":"0.014","_rn_":"Mkt, SMB, HML, RMWg, CMA, WML"},{"1":"0.263","2":"0.047","3":"0.008","4":"0.095","5":"0.113","6":"0.006","7":"0.106","_rn_":"Mkt, SMB, HMLm, RMWor, CMA, WML"},{"1":"0.356","2":"0.088","3":"0.018","4":"0.116","5":"0.207","6":"0.002","7":"0.097","_rn_":"Mkt, SMB, HMLm, RMWc, CMA, WML"},{"1":"0.246","2":"0.044","3":"0.000","4":"0.118","5":"0.097","6":"0.138","7":"0.097","_rn_":"Mkt, SMB, HMLm, RMWg, CMA, WML"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


