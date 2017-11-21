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
```

# My Factors


```r
f = fread("C:/Data/Thesis/factors.csv")

f = f %>% select(Rm, SMB, HML, HMLm, RMWo, RMWg, CMA, WML)

rbind(sapply(f, mean), sapply(f, se), sapply(f, t.stat), sapply(f, sr))  %>% round(2)
```

```
##        Rm  SMB  HML HMLm RMWo RMWg  CMA  WML
## [1,] 0.50 0.26 0.31 0.37 0.27 0.23 0.28 0.61
## [2,] 0.17 0.12 0.11 0.14 0.09 0.09 0.07 0.16
## [3,] 2.84 2.28 2.76 2.73 3.09 2.59 3.93 3.82
## [4,] 0.11 0.09 0.11 0.11 0.12 0.10 0.16 0.15
```

# French Dartmouth Web Factors


```r
ff = fread("C:/Data/FrenchDartmouth/FF_factors.csv")

ff = ff %>% select(-c(Date))
setnames(ff,
         c("Mkt-RF", "SMB", "HML", "RMW", "CMA"),
         c("FF.Rm", "FF.SMB", "FF.HML", "FF.RMW", "FF.CMA"))

rbind(sapply(ff, mean), sapply(ff, se), sapply(ff, t.stat))  %>% round(2)
```

```
##      FF.Rm FF.SMB FF.HML FF.RMW FF.CMA    RF
## [1,]  0.51   0.26   0.37   0.25   0.31  0.39
## [2,]  0.17   0.12   0.11   0.09   0.08  0.01
## [3,]  2.92   2.19   3.32   2.79   3.88 37.63
```


```r
f = cbind(f, ff)
```

# Factor Correlation


```r
cor(f) %>% round(2)
```

```
##           Rm   SMB   HML  HMLm  RMWo  RMWg   CMA   WML FF.Rm FF.SMB FF.HML
## Rm      1.00  0.27 -0.33 -0.16 -0.23  0.14 -0.35 -0.05  1.00   0.28  -0.26
## SMB     0.27  1.00 -0.12 -0.06 -0.35  0.16 -0.04  0.04  0.27   0.99  -0.07
## HML    -0.33 -0.12  1.00  0.79  0.13 -0.59  0.65 -0.22 -0.34  -0.13   0.95
## HMLm   -0.16 -0.06  0.79  1.00  0.03 -0.51  0.48 -0.68 -0.16  -0.07   0.80
## RMWo   -0.23 -0.35  0.13  0.03  1.00  0.24 -0.14  0.01 -0.22  -0.35   0.08
## RMWg    0.14  0.16 -0.59 -0.51  0.24  1.00 -0.41  0.13  0.14   0.16  -0.60
## CMA    -0.35 -0.04  0.65  0.48 -0.14 -0.41  1.00 -0.07 -0.35  -0.04   0.63
## WML    -0.05  0.04 -0.22 -0.68  0.01  0.13 -0.07  1.00 -0.04   0.04  -0.24
## FF.Rm   1.00  0.27 -0.34 -0.16 -0.22  0.14 -0.35 -0.04  1.00   0.28  -0.26
## FF.SMB  0.28  0.99 -0.13 -0.07 -0.35  0.16 -0.04  0.04  0.28   1.00  -0.08
## FF.HML -0.26 -0.07  0.95  0.80  0.08 -0.60  0.63 -0.24 -0.26  -0.08   1.00
## FF.RMW -0.24 -0.35  0.14  0.02  0.96  0.24 -0.13  0.03 -0.23  -0.35   0.07
## FF.CMA -0.38 -0.10  0.70  0.53 -0.02 -0.46  0.92 -0.08 -0.38  -0.11   0.70
## RF     -0.08 -0.05  0.07  0.04  0.01 -0.04  0.06  0.05 -0.08  -0.05   0.06
##        FF.RMW FF.CMA    RF
## Rm      -0.24  -0.38 -0.08
## SMB     -0.35  -0.10 -0.05
## HML      0.14   0.70  0.07
## HMLm     0.02   0.53  0.04
## RMWo     0.96  -0.02  0.01
## RMWg     0.24  -0.46 -0.04
## CMA     -0.13   0.92  0.06
## WML      0.03  -0.08  0.05
## FF.Rm   -0.23  -0.38 -0.08
## FF.SMB  -0.35  -0.11 -0.05
## FF.HML   0.07   0.70  0.06
## FF.RMW   1.00  -0.03  0.00
## FF.CMA  -0.03   1.00  0.06
## RF       0.00   0.06  1.00
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
SR = function(w, factor.mat, covar.mat){

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

## $f = [Rm, SMB, HML]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##        [,1]
## [1,] 0.2109
```

## $f = [Rm, SMB, RMW, CMA]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, FF.RMW, FF.CMA)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##        [,1]
## [1,] 0.3188
```

## $f = [Rm, SMB, RMW, CMA]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.RMW, FF.CMA)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##        [,1]
## [1,] 0.3187
```

## $f = [Rm, SMB, RMW, CMA, WML]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.RMW, FF.CMA, WML)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##        [,1]
## [1,] 0.3623
```

## $f = [Rm, SMB, HML, RMW, CMA, WML]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, FF.HML, FF.RMW, FF.CMA, WML)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##        [,1]
## [1,] 0.3659
```

## $f = [Rm, SMB, HML_M, RMW, CMA, WML]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, FF.RMW, FF.CMA, WML)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##       [,1]
## [1,] 0.423
```

## $f = [Rm, SMB, HML_M, RMW_g, CMA, WML]$


```r
factor.mat = f %>% select(FF.Rm, FF.SMB, HMLm, RMWg, FF.CMA, WML)

R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
covar.mat = cov(factor.mat)

sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)
```

```
##        [,1]
## [1,] 0.4954
```


```r
#w0 = c(1/3, 1/3, 1/3)
#results = solnp(
#    w0, SR, factor.mat=factor.mat, covar.mat=covar.mat,
#    eqfun=sum.constraint, eqB=c(1), control=list(trace=0))
#results$pars %>% round(4)
#sum(results$par)
#-results$values %>% round(4)
```


```r
#factor.mat = f %>% select(RF, FF.Rm, FF.SMB, FF.HML)

#R = matrix(colMeans(factor.mat), nrow=ncol(factor.mat), ncol=1)
#covar.mat = cov(factor.mat)

#w0 = c(1/4, 1/4, 1/4, 1/4)
#SR(w0, factor.mat, covar.mat)

#sqrt(SR.sq.mat(R, covar.mat)) %>% round(4)

#results = solnp(
#    pars=w0, SR, factor.mat=factor.mat, covar.mat=covar.mat,
#    eqfun=sum.constraint, eqB=1, control=list(trace=0))
#results$pars %>% round(4)
#sum(results$par)
#-results$values %>% round(4)

#w.star = results$pars
#ER(w.star, R)
#w.star.mat(R[1], R, covar.mat) %>% round(4)
```

