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
library(pander)
library(rlang)
library(Rsolnp)
library(stargazer)
```


```r
se = function(x) sd(x, na.rm=TRUE)/sqrt(length(x))

t.stat = function(x) t.test(x)$statistic

sr = function(x) mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE)

stat.summary = function(df){
    stat.summ = rbind(sapply(df, mean), sapply(df, se), sapply(df, t.stat), sapply(df, sr))
    row.names(stat.summ) = c("Mean", "StdErr", "t-Stat", "Sh")
    return(stat.summ)
}
```


```r
i = 1    # 1963-07
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

round(stat.summary(f), 2)
```

```
##         HML HMLm RMWo RMWor RMWg RMWc  CMA  WML   Rm  SMB
## Mean   0.30 0.34 0.26  0.34 0.25 0.42 0.27 0.65 0.49 0.25
## StdErr 0.11 0.14 0.09  0.07 0.09 0.06 0.07 0.16 0.18 0.12
## t-Stat 2.65 2.46 2.99  4.99 2.85 7.44 3.75 4.04 2.74 2.11
## Sh     0.11 0.10 0.12  0.20 0.11 0.30 0.15 0.16 0.11 0.08
```

# French Dartmouth Web Factors


```r
ff = fread("C:/Data/FrenchDartmouth/French_6_Factors_2x3.csv")
summary(ff)
```

```
##      Month              Rm                SMB                HML          
##  Min.   :196307   Min.   :-23.2400   Min.   :-15.2800   Min.   :-11.2500  
##  1st Qu.:197611   1st Qu.: -1.9700   1st Qu.: -1.4375   1st Qu.: -1.1400  
##  Median :199004   Median :  0.8050   Median :  0.0850   Median :  0.3100  
##  Mean   :198981   Mean   :  0.5103   Mean   :  0.2652   Mean   :  0.3727  
##  3rd Qu.:200308   3rd Qu.:  3.4225   3rd Qu.:  2.0900   3rd Qu.:  1.7100  
##  Max.   :201612   Max.   : 16.1000   Max.   : 18.7300   Max.   : 12.9100  
##       RMW                CMA                RF              WML          
##  Min.   :-19.1100   Min.   :-6.8800   Min.   :0.0000   Min.   :-34.5800  
##  1st Qu.: -0.8975   1st Qu.:-0.9500   1st Qu.:0.2400   1st Qu.: -0.7450  
##  Median :  0.2150   Median : 0.1750   Median :0.4000   Median :  0.7550  
##  Mean   :  0.2418   Mean   : 0.3098   Mean   :0.3924   Mean   :  0.6641  
##  3rd Qu.:  1.2900   3rd Qu.: 1.5200   3rd Qu.:0.5300   3rd Qu.:  2.8800  
##  Max.   : 13.5200   Max.   : 9.5500   Max.   :1.3500   Max.   : 18.3800
```

```r
ff = ff[i:j,] %>% select(-c(Month))
setnames(ff,
         c("Rm", "SMB", "HML", "RMW", "CMA", "WML"),
         c("FF.Rm", "FF.SMB", "FF.HML", "FF.RMW", "FF.CMA", "FF.WML"))

round(stat.summary(ff), 2)
```

```
##        FF.Rm FF.SMB FF.HML FF.RMW FF.CMA    RF FF.WML
## Mean    0.50   0.26   0.35   0.24   0.30  0.40   0.71
## StdErr  0.18   0.12   0.11   0.09   0.08  0.01   0.17
## t-Stat  2.82   2.10   3.11   2.70   3.77 38.35   4.18
## Sh      0.11   0.08   0.12   0.11   0.15  1.53   0.17
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
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"-0.01","2":"0.08","3":"-0.08","4":"0.94","_rn_":"(Intercept)"},{"1":"0.02","2":"0.02","3":"1.06","4":"0.29","_rn_":"FF.Rm"},{"1":"0.02","2":"0.03","3":"0.68","4":"0.50","_rn_":"FF.SMB"},{"1":"0.15","2":"0.04","3":"3.92","4":"0.00","_rn_":"FF.RMW"},{"1":"1.00","2":"0.04","3":"22.91","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
f$HMLo = fit$residuals

fit = lm(FF.HML~FF.Rm+FF.SMB+RMWor+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.17","2":"0.08","3":"2.03","4":"0.04","_rn_":"(Intercept)"},{"1":"0.00","2":"0.02","3":"-0.20","4":"0.84","_rn_":"FF.Rm"},{"1":"-0.03","2":"0.03","3":"-1.28","4":"0.20","_rn_":"FF.SMB"},{"1":"-0.25","2":"0.05","3":"-5.03","4":"0.00","_rn_":"RMWor"},{"1":"0.90","2":"0.05","3":"19.88","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
fit = lm(FF.HML~FF.Rm+FF.SMB+RMWg+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.23","2":"0.08","3":"3.03","4":"0.00","_rn_":"(Intercept)"},{"1":"-0.01","2":"0.02","3":"-0.49","4":"0.62","_rn_":"FF.Rm"},{"1":"0.03","2":"0.03","3":"1.17","4":"0.24","_rn_":"FF.SMB"},{"1":"-0.44","2":"0.04","3":"-11.73","4":"0.00","_rn_":"RMWg"},{"1":"0.74","2":"0.04","3":"16.76","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
fit = lm(FF.HML~FF.Rm+FF.SMB+RMWc+FF.CMA, data=f)
round(as.data.frame(coef(summary(fit))), 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Std. Error"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["t value"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Pr(>|t|)"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.27","2":"0.09","3":"3.08","4":"0.00","_rn_":"(Intercept)"},{"1":"-0.03","2":"0.02","3":"-1.40","4":"0.16","_rn_":"FF.Rm"},{"1":"-0.05","2":"0.03","3":"-1.85","4":"0.06","_rn_":"FF.SMB"},{"1":"-0.39","2":"0.06","3":"-6.59","4":"0.00","_rn_":"RMWc"},{"1":"0.91","2":"0.04","3":"20.85","4":"0.00","_rn_":"FF.CMA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Factor Correlation


```r
cor(f) %>% round(2)
```

```
##          HML  HMLm  RMWo RMWor  RMWg  RMWc   CMA   WML    Rm   SMB FF.Rm
## HML     1.00  0.79  0.12 -0.33 -0.59 -0.22  0.65 -0.22 -0.34 -0.13 -0.34
## HMLm    0.79  1.00  0.03 -0.37 -0.50 -0.33  0.49 -0.68 -0.17 -0.07 -0.17
## RMWo    0.12  0.03  1.00  0.78  0.24  0.63 -0.15  0.01 -0.22 -0.35 -0.22
## RMWor  -0.33 -0.37  0.78  1.00  0.61  0.77 -0.36  0.16 -0.01 -0.13  0.00
## RMWg   -0.59 -0.50  0.24  0.61  1.00  0.53 -0.42  0.13  0.15  0.17  0.16
## RMWc   -0.22 -0.33  0.63  0.77  0.53  1.00 -0.15  0.24 -0.24 -0.23 -0.23
## CMA     0.65  0.49 -0.15 -0.36 -0.42 -0.15  1.00 -0.07 -0.35 -0.03 -0.35
## WML    -0.22 -0.68  0.01  0.16  0.13  0.24 -0.07  1.00 -0.04  0.05 -0.04
## Rm     -0.34 -0.17 -0.22 -0.01  0.15 -0.24 -0.35 -0.04  1.00  0.27  1.00
## SMB    -0.13 -0.07 -0.35 -0.13  0.17 -0.23 -0.03  0.05  0.27  1.00  0.26
## FF.Rm  -0.34 -0.17 -0.22  0.00  0.16 -0.23 -0.35 -0.04  1.00  0.26  1.00
## FF.SMB -0.13 -0.08 -0.35 -0.13  0.17 -0.24 -0.04  0.05  0.28  0.99  0.27
## FF.HML  0.95  0.80  0.08 -0.34 -0.59 -0.25  0.64 -0.24 -0.26 -0.08 -0.26
## FF.RMW  0.14  0.03  0.97  0.74  0.24  0.61 -0.14  0.03 -0.24 -0.35 -0.23
## FF.CMA  0.70  0.52 -0.03 -0.30 -0.47 -0.12  0.92 -0.08 -0.39 -0.10 -0.39
## RF      0.09  0.06  0.01 -0.04 -0.05  0.04  0.06  0.03 -0.08 -0.05 -0.08
## FF.WML -0.15 -0.64  0.09  0.20  0.09  0.28 -0.02  0.98 -0.13 -0.02 -0.13
## HMLo    0.63  0.61  0.00 -0.30 -0.42 -0.32  0.01 -0.26  0.00  0.01  0.00
##        FF.SMB FF.HML FF.RMW FF.CMA    RF FF.WML  HMLo
## HML     -0.13   0.95   0.14   0.70  0.09  -0.15  0.63
## HMLm    -0.08   0.80   0.03   0.52  0.06  -0.64  0.61
## RMWo    -0.35   0.08   0.97  -0.03  0.01   0.09  0.00
## RMWor   -0.13  -0.34   0.74  -0.30 -0.04   0.20 -0.30
## RMWg     0.17  -0.59   0.24  -0.47 -0.05   0.09 -0.42
## RMWc    -0.24  -0.25   0.61  -0.12  0.04   0.28 -0.32
## CMA     -0.04   0.64  -0.14   0.92  0.06  -0.02  0.01
## WML      0.05  -0.24   0.03  -0.08  0.03   0.98 -0.26
## Rm       0.28  -0.26  -0.24  -0.39 -0.08  -0.13  0.00
## SMB      0.99  -0.08  -0.35  -0.10 -0.05  -0.02  0.01
## FF.Rm    0.27  -0.26  -0.23  -0.39 -0.08  -0.13  0.00
## FF.SMB   1.00  -0.09  -0.35  -0.11 -0.05  -0.02  0.00
## FF.HML  -0.09   1.00   0.08   0.69  0.08  -0.18  0.71
## FF.RMW  -0.35   0.08   1.00  -0.04  0.01   0.11  0.00
## FF.CMA  -0.11   0.69  -0.04   1.00  0.06  -0.01  0.00
## RF      -0.05   0.08   0.01   0.06  1.00   0.05  0.05
## FF.WML  -0.02  -0.18   0.11  -0.01  0.05   1.00 -0.26
## HMLo     0.00   0.71   0.00   0.00  0.05  -0.26  1.00
```

# Spanning Regressions

Regressing CMA on the other factors shows that the intercept, size and profitability are insignificant.
The market is significant but the coefficient is small.
Value and momentum are significant and their coefficients are greater than 0.2.
The combination of value and momentum absorbs the information in the investment factor.


```r
fit = lm(FF.CMA~FF.Rm+FF.SMB+HMLm+RMWc+FF.WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.06       0.07    0.92     0.36
## FF.Rm          -0.09       0.01   -6.31     0.00
## FF.SMB          0.00       0.02    0.19     0.85
## HMLm            0.45       0.02   18.51     0.00
## RMWc           -0.05       0.05   -1.11     0.27
## FF.WML          0.22       0.02   11.85     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+FF.SMB+HMLm+RMWg+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.12       0.06    1.91     0.06
## FF.Rm          -0.11       0.01   -7.64     0.00
## FF.SMB          0.01       0.02    0.41     0.68
## HMLm            0.39       0.03   13.81     0.00
## RMWg           -0.13       0.03   -3.90     0.00
## WML             0.20       0.02    9.23     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+FF.SMB+FF.HML+RMWc, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.20       0.06    3.25     0.00
## FF.Rm          -0.10       0.01   -7.28     0.00
## FF.SMB          0.00       0.02    0.26     0.80
## FF.HML          0.45       0.02   20.85     0.00
## RMWc           -0.01       0.04   -0.31     0.75
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+FF.SMB+FF.HML+RMWc+FF.WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.18       0.06    2.87     0.00
## FF.Rm          -0.10       0.01   -6.88     0.00
## FF.SMB          0.00       0.02    0.08     0.93
## FF.HML          0.46       0.02   21.22     0.00
## RMWc           -0.04       0.04   -0.94     0.35
## FF.WML          0.04       0.01    3.08     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWc~FF.Rm+FF.SMB+FF.HML+RMWor+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.25       0.04    7.16        0
## FF.Rm          -0.06       0.01   -7.61        0
## FF.SMB         -0.04       0.01   -3.65        0
## FF.HML         -0.07       0.02   -4.17        0
## RMWor           0.62       0.02   29.89        0
## FF.CMA          0.08       0.02    3.30        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWc~FF.Rm+FF.SMB+FF.HML+FF.RMW+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.39       0.04    9.41     0.00
## FF.Rm          -0.05       0.01   -4.41     0.00
## FF.SMB          0.00       0.01   -0.10     0.92
## FF.HML         -0.23       0.02  -11.38     0.00
## FF.RMW          0.39       0.02   19.85     0.00
## FF.CMA          0.12       0.03    3.91     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWg~FF.Rm+FF.SMB+FF.HML+FF.RMW+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.28       0.07    4.16     0.00
## FF.Rm           0.01       0.02    0.50     0.61
## FF.SMB          0.18       0.02    7.90     0.00
## FF.HML         -0.47       0.03  -14.63     0.00
## FF.RMW          0.37       0.03   11.81     0.00
## FF.CMA         -0.01       0.05   -0.18     0.86
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWg~FF.Rm+FF.SMB+FF.HML+RMWor+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.11       0.06    1.84     0.07
## FF.Rm          -0.01       0.01   -0.43     0.67
## FF.SMB          0.15       0.02    7.49     0.00
## FF.HML         -0.31       0.03  -10.60     0.00
## RMWor           0.65       0.04   17.86     0.00
## FF.CMA         -0.03       0.04   -0.82     0.41
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWg~FF.Rm+FF.SMB+FF.HML+RMWc+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -0.01       0.06   -0.14     0.89
## FF.Rm           0.04       0.02    2.48     0.01
## FF.SMB          0.17       0.02    8.33     0.00
## FF.HML         -0.28       0.03   -9.38     0.00
## RMWc            0.79       0.05   17.42     0.00
## FF.CMA         -0.12       0.04   -2.87     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+FF.HML+FF.RMW+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.71       0.16    4.40     0.00
## FF.Rm          -0.07       0.04   -1.81     0.07
## FF.SMB          0.11       0.06    1.97     0.05
## FF.HML         -0.52       0.08   -6.78     0.00
## FF.RMW          0.13       0.08    1.70     0.09
## FF.CMA          0.32       0.11    2.78     0.01
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+FF.HML+RMWc+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.46       0.17    2.69     0.01
## FF.Rm          -0.04       0.04   -0.94     0.35
## FF.SMB          0.13       0.05    2.46     0.01
## FF.HML         -0.41       0.08   -5.28     0.00
## RMWc            0.56       0.12    4.68     0.00
## FF.CMA          0.28       0.11    2.56     0.01
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+HMLm+RMWc+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.82       0.12    6.93     0.00
## FF.Rm          -0.06       0.03   -2.05     0.04
## FF.SMB          0.05       0.04    1.29     0.20
## HMLm           -1.03       0.04  -27.11     0.00
## RMWc           -0.05       0.08   -0.64     0.52
## FF.CMA          0.72       0.07   11.03     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+HMLm+RMWg+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.94       0.11    8.78     0.00
## FF.Rm          -0.06       0.03   -2.38     0.02
## FF.SMB          0.09       0.04    2.50     0.01
## HMLm           -1.10       0.04  -30.07     0.00
## RMWg           -0.37       0.06   -6.69     0.00
## FF.CMA          0.61       0.07    9.23     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+HMLm+RMWg, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     1.15       0.11   10.36        0
## FF.Rm          -0.14       0.03   -5.61        0
## FF.SMB          0.11       0.04    2.82        0
## HMLm           -0.98       0.04  -26.92        0
## RMWg           -0.51       0.06   -8.95        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.HML~FF.Rm+FF.SMB+FF.RMW+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -0.01       0.08   -0.08     0.94
## FF.Rm           0.02       0.02    1.06     0.29
## FF.SMB          0.02       0.03    0.68     0.50
## FF.RMW          0.15       0.04    3.92     0.00
## FF.CMA          1.00       0.04   22.91     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.HML~FF.Rm+FF.SMB+RMWc+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.27       0.09    3.08     0.00
## FF.Rm          -0.03       0.02   -1.40     0.16
## FF.SMB         -0.05       0.03   -1.85     0.06
## RMWc           -0.39       0.06   -6.59     0.00
## FF.CMA          0.91       0.04   20.85     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(HMLm~FF.Rm+FF.SMB+FF.RMW+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.01       0.12    0.12     0.90
## FF.Rm           0.05       0.03    1.50     0.13
## FF.SMB         -0.01       0.04   -0.33     0.74
## FF.RMW          0.09       0.06    1.55     0.12
## FF.CMA          0.94       0.06   14.60     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(HMLm~FF.Rm+FF.SMB+FF.RMW+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.41       0.08    5.04     0.00
## FF.Rm           0.00       0.02   -0.05     0.96
## FF.SMB          0.04       0.03    1.49     0.14
## FF.RMW          0.12       0.04    3.09     0.00
## FF.CMA          0.83       0.04   19.47     0.00
## WML            -0.56       0.02  -28.84     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(HMLm~FF.Rm+FF.SMB+RMWc+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.64       0.08    7.64     0.00
## FF.Rm          -0.04       0.02   -2.21     0.03
## FF.SMB         -0.02       0.03   -0.81     0.42
## RMWc           -0.37       0.06   -6.36     0.00
## FF.CMA          0.75       0.04   17.81     0.00
## WML            -0.53       0.02  -27.11     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(HMLm~FF.Rm+FF.SMB+FF.HML+RMWc+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.40       0.05    8.07     0.00
## FF.Rm          -0.02       0.01   -1.61     0.11
## FF.SMB          0.01       0.02    0.41     0.68
## FF.HML          0.80       0.02   34.81     0.00
## RMWc           -0.12       0.04   -3.47     0.00
## FF.CMA          0.03       0.03    0.91     0.36
## WML            -0.44       0.01  -38.29     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWc~FF.Rm+FF.SMB+FF.HML+FF.RMW+RMWg+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.28       0.04    7.36        0
## FF.Rm          -0.04       0.01   -4.83        0
## FF.SMB         -0.06       0.01   -4.30        0
## FF.HML         -0.07       0.02   -3.36        0
## FF.RMW          0.28       0.02   14.63        0
## RMWg            0.28       0.02   12.53        0
## FF.CMA          0.10       0.03    3.87        0
## WML             0.05       0.01    5.75        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWc~FF.Rm+FF.SMB+FF.HML+HMLm+FF.RMW+RMWor+RMWg+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.21       0.04    5.79     0.00
## FF.Rm          -0.06       0.01   -7.14     0.00
## FF.SMB         -0.07       0.01   -5.47     0.00
## FF.HML         -0.01       0.03   -0.37     0.71
## HMLm            0.00       0.03    0.06     0.96
## FF.RMW          0.01       0.03    0.53     0.60
## RMWor           0.50       0.04   12.53     0.00
## RMWg            0.15       0.02    6.74     0.00
## FF.CMA          0.07       0.02    3.15     0.00
## WML             0.04       0.02    2.64     0.01
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWg~FF.Rm+FF.SMB+FF.HML+HMLm+FF.RMW+RMWor+RMWc+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.02       0.07    0.36     0.72
## FF.Rm           0.01       0.01    0.71     0.48
## FF.SMB          0.15       0.02    7.54     0.00
## FF.HML         -0.25       0.05   -4.96     0.00
## HMLm           -0.01       0.05   -0.28     0.78
## FF.RMW         -0.12       0.05   -2.54     0.01
## RMWor           0.51       0.08    6.66     0.00
## RMWc            0.46       0.07    6.74     0.00
## FF.CMA         -0.08       0.04   -1.82     0.07
## WML            -0.06       0.03   -2.14     0.03
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(RMWor~FF.Rm+FF.SMB+FF.HML+HMLm+FF.RMW+RMWg+RMWc+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.08       0.03    2.31     0.02
## FF.Rm           0.05       0.01    6.09     0.00
## FF.SMB          0.04       0.01    3.99     0.00
## FF.HML          0.02       0.03    0.62     0.54
## HMLm           -0.14       0.03   -5.50     0.00
## FF.RMW          0.42       0.02   23.63     0.00
## RMWg            0.13       0.02    6.66     0.00
## RMWc            0.40       0.03   12.53     0.00
## FF.CMA          0.02       0.02    0.83     0.41
## WML            -0.06       0.01   -4.37     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(HMLm~FF.Rm+FF.SMB+FF.HML+FF.RMW+RMWor+RMWg+RMWc+FF.CMA+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.41       0.05    8.58     0.00
## FF.Rm           0.00       0.01    0.02     0.98
## FF.SMB          0.04       0.02    2.23     0.03
## FF.HML          0.74       0.03   27.78     0.00
## FF.RMW          0.20       0.04    5.32     0.00
## RMWor          -0.33       0.06   -5.50     0.00
## RMWg           -0.01       0.03   -0.28     0.78
## RMWc            0.00       0.05    0.06     0.96
## FF.CMA          0.05       0.03    1.40     0.16
## WML            -0.44       0.01  -38.91     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+FF.HML+FF.RMW+RMWor+RMWg+RMWc+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.44       0.17    2.58     0.01
## FF.Rm          -0.03       0.04   -0.72     0.47
## FF.SMB          0.17       0.06    3.02     0.00
## FF.HML         -0.47       0.09   -5.18     0.00
## FF.RMW         -0.14       0.13   -1.12     0.27
## RMWor           0.09       0.21    0.42     0.68
## RMWg           -0.39       0.11   -3.56     0.00
## RMWc            0.92       0.19    4.87     0.00
## FF.CMA          0.20       0.11    1.78     0.08
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(WML~FF.Rm+FF.SMB+FF.HML+HMLm+FF.RMW+RMWor+RMWg+RMWc+FF.CMA, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.79       0.09    8.61     0.00
## FF.Rm          -0.01       0.02   -0.37     0.71
## FF.SMB          0.11       0.03    3.51     0.00
## FF.HML          1.04       0.06   16.57     0.00
## HMLm           -1.60       0.04  -38.91     0.00
## FF.RMW          0.27       0.07    3.84     0.00
## RMWor          -0.50       0.11   -4.37     0.00
## RMWg           -0.13       0.06   -2.14     0.03
## RMWc            0.27       0.10    2.64     0.01
## FF.CMA          0.13       0.06    2.15     0.03
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.39       0.07    5.24        0
## FF.Rm          -0.17       0.02  -10.50        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~RMWc, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.37       0.08    4.49        0
## RMWc           -0.17       0.06   -3.00        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+RMWc, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.53       0.08    6.98        0
## FF.Rm          -0.20       0.02  -11.88        0
## RMWc           -0.31       0.05   -5.97        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.33       0.08    4.04     0.00
## WML            -0.04       0.02   -1.92     0.06
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~HMLm, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)      0.2       0.07    2.90        0
## HMLm             0.3       0.02   15.32        0
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~HMLm+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -0.04       0.06   -0.59     0.56
## HMLm            0.51       0.02   21.00     0.00
## WML             0.26       0.02   12.46     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+HMLm+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.05       0.06    0.86     0.39
## FF.Rm          -0.11       0.01   -7.79     0.00
## HMLm            0.46       0.02   19.11     0.00
## WML             0.23       0.02   11.09     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+FF.SMB+HMLm+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.05       0.06    0.87     0.39
## FF.Rm          -0.11       0.01   -7.43     0.00
## FF.SMB         -0.01       0.02   -0.26     0.80
## HMLm            0.46       0.02   19.09     0.00
## WML             0.23       0.02   11.07     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```


```r
fit = lm(FF.CMA~FF.Rm+FF.SMB+HMLm+RMWc+WML, data=f)
round(coef(summary(fit)), 2)
```

```
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     0.07       0.07    1.09     0.28
## FF.Rm          -0.11       0.01   -7.37     0.00
## FF.SMB         -0.01       0.02   -0.41     0.68
## HMLm            0.45       0.03   17.81     0.00
## RMWc           -0.04       0.05   -0.75     0.45
## WML             0.23       0.02   11.03     0.00
```

```r
#rbind(coef(summary(fit))[,1], coef(summary(fit))[,3]) %>% round(2)
```

# Useless CMA


```r
fit1 = lm(CMA ~ FF.Rm, data=f)
fit2 = lm(CMA ~ RMWc, data=f)
fit3 = lm(CMA ~ FF.Rm + RMWc, data=f)

fit4 = lm(CMA ~ HMLm, data=f)
fit5 = lm(CMA ~ FF.WML, data=f)
fit6 = lm(CMA ~ HMLm + FF.WML, data=f)

fit7 = lm(CMA ~ FF.HML, data=f)
fit8 = lm(CMA ~ FF.HML + FF.WML, data=f)

fit9 = lm(CMA ~ FF.Rm + FF.SMB + HMLm + RMWc + FF.WML, data=f)
fit10 = lm(CMA ~ FF.Rm + FF.SMB + FF.HML + RMWc + FF.WML, data=f)
```


```r
stargazer(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
          type="text", keep.stat=c("n", "rsq"), report="vcs",
          order=c(7, 1, 2, 6, 4, 3, 5))
```

```
## 
## ============================================================================================
##                                            Dependent variable:                              
##              -------------------------------------------------------------------------------
##                                                    CMA                                      
##                (1)     (2)     (3)     (4)     (5)     (6)     (7)     (8)     (9)    (10)  
## --------------------------------------------------------------------------------------------
## Constant      0.347   0.354   0.489   0.187   0.280  -0.018   0.130   0.095   0.090   0.186 
##              (0.069) (0.076) (0.070) (0.064) (0.074) (0.061) (0.057) (0.058) (0.065) (0.061)
##                                                                                             
## FF.Rm        -0.146          -0.169                                          -0.088  -0.090 
##              (0.015)         (0.015)                                         (0.014) (0.014)
##                                                                                             
## FF.SMB                                                                        0.036   0.034 
##                                                                              (0.020) (0.019)
##                                                                                             
## FF.HML                                                        0.415   0.427           0.380 
##                                                              (0.020) (0.020)         (0.022)
##                                                                                             
## HMLm                                  0.257           0.421                   0.369         
##                                      (0.018)         (0.022)                 (0.023)        
##                                                                                             
## RMWc                 -0.188  -0.310                                          -0.081  -0.074 
##                      (0.051) (0.048)                                         (0.045) (0.044)
##                                                                                             
## FF.WML                                       -0.008   0.211           0.043   0.181   0.033 
##                                              (0.017) (0.018)         (0.013) (0.018) (0.014)
##                                                                                             
## --------------------------------------------------------------------------------------------
## Observations   630     630     630     630     630     630     630     630     630     630  
## R2            0.124   0.021   0.179   0.235  0.0004   0.377   0.404   0.414   0.412   0.452 
## ============================================================================================
## Note:                                                            *p<0.1; **p<0.05; ***p<0.01
```


```r
stargazer(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
          type="text", keep.stat=c("n", "rsq"), report="vcs",
          order=c(7, 1, 2, 6, 4, 3, 5))
```


============================================================================================
                                           Dependent variable:                              
             -------------------------------------------------------------------------------
                                                   CMA                                      
               (1)     (2)     (3)     (4)     (5)     (6)     (7)     (8)     (9)    (10)  
--------------------------------------------------------------------------------------------
Constant      0.347   0.354   0.489   0.187   0.280  -0.018   0.130   0.095   0.090   0.186 
             (0.069) (0.076) (0.070) (0.064) (0.074) (0.061) (0.057) (0.058) (0.065) (0.061)
                                                                                            
FF.Rm        -0.146          -0.169                                          -0.088  -0.090 
             (0.015)         (0.015)                                         (0.014) (0.014)
                                                                                            
FF.SMB                                                                        0.036   0.034 
                                                                             (0.020) (0.019)
                                                                                            
FF.HML                                                        0.415   0.427           0.380 
                                                             (0.020) (0.020)         (0.022)
                                                                                            
HMLm                                  0.257           0.421                   0.369         
                                     (0.018)         (0.022)                 (0.023)        
                                                                                            
RMWc                 -0.188  -0.310                                          -0.081  -0.074 
                     (0.051) (0.048)                                         (0.045) (0.044)
                                                                                            
FF.WML                                       -0.008   0.211           0.043   0.181   0.033 
                                             (0.017) (0.018)         (0.013) (0.018) (0.014)
                                                                                            
--------------------------------------------------------------------------------------------
Observations   630     630     630     630     630     630     630     630     630     630  
R2            0.124   0.021   0.179   0.235  0.0004   0.377   0.404   0.414   0.412   0.452 
============================================================================================
Note:                                                            *p<0.1; **p<0.05; ***p<0.01

```r
stargazer(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
          type="html", keep.stat=c("n", "rsq"), report="vcs",
          order=c(7, 1, 2, 6, 4, 3, 5))
```


<table style="text-align:center"><tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="10"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="10" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="10">CMA</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td><td>(9)</td><td>(10)</td></tr>
<tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Constant</td><td>0.347</td><td>0.354</td><td>0.489</td><td>0.187</td><td>0.280</td><td>-0.018</td><td>0.130</td><td>0.095</td><td>0.090</td><td>0.186</td></tr>
<tr><td style="text-align:left"></td><td>(0.069)</td><td>(0.076)</td><td>(0.070)</td><td>(0.064)</td><td>(0.074)</td><td>(0.061)</td><td>(0.057)</td><td>(0.058)</td><td>(0.065)</td><td>(0.061)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">FF.Rm</td><td>-0.146</td><td></td><td>-0.169</td><td></td><td></td><td></td><td></td><td></td><td>-0.088</td><td>-0.090</td></tr>
<tr><td style="text-align:left"></td><td>(0.015)</td><td></td><td>(0.015)</td><td></td><td></td><td></td><td></td><td></td><td>(0.014)</td><td>(0.014)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">FF.SMB</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td>0.036</td><td>0.034</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td>(0.020)</td><td>(0.019)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">FF.HML</td><td></td><td></td><td></td><td></td><td></td><td></td><td>0.415</td><td>0.427</td><td></td><td>0.380</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td>(0.020)</td><td>(0.020)</td><td></td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">HMLm</td><td></td><td></td><td></td><td>0.257</td><td></td><td>0.421</td><td></td><td></td><td>0.369</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.018)</td><td></td><td>(0.022)</td><td></td><td></td><td>(0.023)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">RMWc</td><td></td><td>-0.188</td><td>-0.310</td><td></td><td></td><td></td><td></td><td></td><td>-0.081</td><td>-0.074</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.051)</td><td>(0.048)</td><td></td><td></td><td></td><td></td><td></td><td>(0.045)</td><td>(0.044)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">FF.WML</td><td></td><td></td><td></td><td></td><td>-0.008</td><td>0.211</td><td></td><td>0.043</td><td>0.181</td><td>0.033</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td>(0.017)</td><td>(0.018)</td><td></td><td>(0.013)</td><td>(0.018)</td><td>(0.014)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>630</td><td>630</td><td>630</td><td>630</td><td>630</td><td>630</td><td>630</td><td>630</td><td>630</td><td>630</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.124</td><td>0.021</td><td>0.179</td><td>0.235</td><td>0.0004</td><td>0.377</td><td>0.404</td><td>0.414</td><td>0.412</td><td>0.452</td></tr>
<tr><td colspan="11" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="10" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

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
kable(res, digits=3)
```

                                     Sh2     Mkt    Size     Val    Prof     Inv     Mom
--------------------------------  ------  ------  ------  ------  ------  ------  ------
Mkt, SMB, HML, RMWor, CMA          0.153   0.037   0.010   0.008   0.092   0.031      NA
Mkt, SMB, HML, RMWc, CMA           0.246   0.074   0.021   0.019   0.185   0.021      NA
Mkt, SMB, HML, RMWg, CMA           0.119   0.033   0.000   0.016   0.058   0.026      NA
Mkt, SMB, HML, RMWor, CMA, WML     0.181   0.042   0.008   0.015   0.081   0.024   0.028
Mkt, SMB, HML, RMWc, CMA, WML      0.260   0.076   0.017   0.025   0.161   0.017   0.014
Mkt, SMB, HML, RMWg, CMA, WML      0.159   0.039   0.000   0.029   0.059   0.040   0.014
Mkt, SMB, HMLm, RMWor, CMA, WML    0.261   0.047   0.008   0.095   0.111   0.006   0.106
Mkt, SMB, HMLm, RMWc, CMA, WML     0.351   0.087   0.018   0.115   0.201   0.003   0.096
Mkt, SMB, HMLm, RMWc, WML          0.348   0.086   0.018   0.206   0.200      NA   0.131
Mkt, SMB, HMLm, RMWg, CMA, WML     0.248   0.044   0.000   0.119   0.099   0.137   0.131

```r
kable(res, digits=3, format="markdown")
```



|                                |   Sh2|   Mkt|  Size|   Val|  Prof|   Inv|   Mom|
|:-------------------------------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
|Mkt, SMB, HML, RMWor, CMA       | 0.153| 0.037| 0.010| 0.008| 0.092| 0.031|    NA|
|Mkt, SMB, HML, RMWc, CMA        | 0.246| 0.074| 0.021| 0.019| 0.185| 0.021|    NA|
|Mkt, SMB, HML, RMWg, CMA        | 0.119| 0.033| 0.000| 0.016| 0.058| 0.026|    NA|
|Mkt, SMB, HML, RMWor, CMA, WML  | 0.181| 0.042| 0.008| 0.015| 0.081| 0.024| 0.028|
|Mkt, SMB, HML, RMWc, CMA, WML   | 0.260| 0.076| 0.017| 0.025| 0.161| 0.017| 0.014|
|Mkt, SMB, HML, RMWg, CMA, WML   | 0.159| 0.039| 0.000| 0.029| 0.059| 0.040| 0.014|
|Mkt, SMB, HMLm, RMWor, CMA, WML | 0.261| 0.047| 0.008| 0.095| 0.111| 0.006| 0.106|
|Mkt, SMB, HMLm, RMWc, CMA, WML  | 0.351| 0.087| 0.018| 0.115| 0.201| 0.003| 0.096|
|Mkt, SMB, HMLm, RMWc, WML       | 0.348| 0.086| 0.018| 0.206| 0.200|    NA| 0.131|
|Mkt, SMB, HMLm, RMWg, CMA, WML  | 0.248| 0.044| 0.000| 0.119| 0.099| 0.137| 0.131|

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
## [1] 630 407
```

```r
summary(a$Date)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  196300  197600  198900  198900  200200  201500
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
##    0.00    0.05    0.09    0.12    0.15    0.91
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 121
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##         [,1]
## [1,] 6.42085
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1186753
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
##    0.00    0.03    0.08    0.11    0.14    0.78
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 93
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.366908
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.10829
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
##    0.00    0.04    0.08    0.10    0.13    0.73
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 70
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.279579
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.0997849
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
##    0.00    0.04    0.08    0.10    0.14    0.95
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 83
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.355437
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.10314
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
##    0.00    0.04    0.09    0.10    0.14    0.78
```

```r
sum(abs(regs["t(a)"])>2)
```

```
## [1] 79
```

```r
# Sharpe squared intercepts
SR.sq.mat(regs$a, cov(reg.resid))
```

```
##          [,1]
## [1,] 6.648472
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1040583
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
##    0.00    0.03    0.08    0.10    0.14    0.79
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
## [1,] 6.240851
```

```r
# Average absolute intercepts
(mean(abs(regs$a)))
```

```
## [1] 0.1034087
```

