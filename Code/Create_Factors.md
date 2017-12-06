---
title: "Create Factors"
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
```

# Functions


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
combine.sources = function(crsp, comp){

    df = left_join(
        crsp,
        comp %>% select(
            PERMNO, Period, BE, D1.BE, OP, OP.2, OP.OK, CP, GP, Acc, INV, INV.ppe
        ),
        by=c("PERMNO", "Period"))

    df$BM = df$BE/df$M              # Annual, Lagged
    df$bm = log(df$BM)              # Log Annual, Lagged
    df$BMC = df$BE/df$Size          # Annual, Current
    df$BMM = df$BE/df$L1.ME         # Monthly, Current
    
    df$BM[is.infinite(df$BM)] = NA
    df$bm[is.infinite(df$bm)] = NA
    df$BMC[is.infinite(df$BMC)] = NA
    df$BMM[is.infinite(df$BMM)] = NA

    return(df %>% filter(Date>as.Date("1962-06-30")))
}
```


```r
assign.bkts = function(df, var, quantiles){

    bkt.col = paste0(var, ".B")

    df[,bkt.col] = NA

    brk.cols = paste0("D", quantiles*100)

    for (i in 0:(length(quantiles) - 1)){
        if (i==0){
            ix = df[,var]<=df[,brk.cols[1]]
        } else {
            ix = (df[,var]>df[,brk.cols[i]]) & (df[,var]<=df[,brk.cols[i+1]])
        }

        df[,bkt.col][ix] = i+1
    }

    # for now, breakpoints are always deciles
    # regradless of the quantiles used to assigned buckets
    df[, paste0("D", 1:10*10)] = NULL

    return(df)
}
```


```r
ri.adj.Size = function(df){

    df$adj.L1.ri = 1 + df$L1.ri
    df$adj.L1.ri[df$Month==7] = 1

    df = df %>% group_by(PERMNO, Period) %>% mutate(
        adj.Size=Size*cumprod(adj.L1.ri)
    ) %>% as.data.frame

    return(df)
}
```

# CRSP


```r
# fread much faster than read.csv
# colClasses='character' prevents scary message about losing data due to
# changing column types part way through reading the data
crsp = fread(
    "C:/Data/CRSP/CRSP_196001_201612_d.csv", colClasses='character', showProgress=FALSE
)
```


```r
crsp = crsp %>% filter(SHRCD %in% c(10, 11)) %>% filter(EXCHCD %in% c(1, 2, 3))

crsp$SHRCD = NULL  # sharecode no longer needed

setnames(crsp,
         c("EXCHCD", "SICCD", "DLRET", "RET", "PRC", "SHROUT", "vwretd"),
         c("Exchange", "Industry", "dl.ri", "ri", "price", "shares", "rmkt"))

crsp$PERMNO = factor(crsp$PERMNO)
crsp$Date = as.Date(crsp$date, "%Y%m%d")
crsp$Year = as.numeric(format(crsp$Date, "%Y"))
crsp$Month = as.numeric(format(crsp$Date, "%m"))
crsp$Exchange = factor(crsp$Exchange,
                       levels=c("1","2","3"),
                       labels=c("NYSE", "AMEX", "NASDAQ"))
crsp$Industry = factor(crsp$Industry)
crsp$dl.ri = as.numeric(crsp$dl.ri)
crsp$price = as.numeric(crsp$price)
crsp$ri = as.numeric(crsp$ri)
crsp$shares = as.numeric(crsp$shares) / 1000
crsp$rmkt = as.numeric(crsp$rmkt)

summary(crsp %>% select(Date, ri, price, shares))
```

```
##       Date                  ri            price          
##  Min.   :1960-01-29   Min.   :-0.98   Min.   : -1832.50  
##  1st Qu.:1981-02-27   1st Qu.:-0.07   1st Qu.:     1.40  
##  Median :1992-05-29   Median : 0.00   Median :     9.88  
##  Mean   :1991-08-03   Mean   : 0.01   Mean   :    23.78  
##  3rd Qu.:2002-01-31   3rd Qu.: 0.07   3rd Qu.:    23.50  
##  Max.   :2016-12-30   Max.   :24.00   Max.   :244121.00  
##                       NA's   :62078   NA's   :37773      
##      shares         
##  Min.   :    0.000  
##  1st Qu.:    2.733  
##  Median :    7.757  
##  Mean   :   41.691  
##  3rd Qu.:   24.590  
##  Max.   :29206.400  
##  NA's   :4012
```


```r
no.return = is.na(crsp$ri) & is.na(crsp$dl.ri)
crsp$ri[is.na(crsp$ri)] = 0
crsp$dl.ri[is.na(crsp$dl.ri)] = 0
crsp$ri = (1 + crsp$ri) * (1 + crsp$dl.ri) - 1
crsp$ri[no.return] = NA
```

There should be no duplicate $PERMNO$-$date$ pairs
(0 found).
CRSP data has 3 117 344 rows and
12 columns.


```r
# duplicated will omit return FALSE for the first instance of a duplicated row
# calling from the top and the bottom ensure all instances are caught
pairs.ix = duplicated(crsp[, c("PERMNO", "date")]) |
           duplicated(crsp[, c("PERMNO", "date")], fromLast=TRUE)
if (sum(pairs.ix)>0){
    kable(head(crsp[pairs.ix], n=10), digits=2, caption="Duplicates")
    crsp = crsp %>% filter(!duplicated(crsp[,c("PERMNO", "date")]))
}
```

## Market Equity, $ME$


```r
crsp$ME = abs(crsp$price) * crsp$shares
summary(crsp$ME)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##      0.0     17.6     73.4   1395.0    379.9 750700.0    37773
```

## Holding Period

Most portfolios will be reformed at the end of every June and held for 12 months.


```r
crsp$Period = crsp$Year
crsp$Period[crsp$Month<7] = crsp$Period[crsp$Month<7] - 1
```

## Lagged Market Equity

For portfolios reformed monthly we need the lagged market equity.


```r
crsp = crsp %>% group_by(PERMNO) %>%
    mutate(L1.ME=lag(ME)) %>% as.data.frame
```

## June and December Market Equity, $Size$ and $M$

We need the market equity in June for the $Size$ variable in July
and the market equity in December for the $BM$ variable.


```r
ME = crsp %>% filter(Month==6) %>% select(PERMNO, Period, ME)
```

```
## Warning: package 'bindrcpp' was built under R version 3.3.3
```

```r
ME$Period = ME$Period + 1
ME$Size = ME$ME
ME$ME = NULL
summary(ME)
```

```
##      PERMNO           Period          Size         
##  10145  :    57   Min.   :1960   Min.   :     0.0  
##  10516  :    57   1st Qu.:1981   1st Qu.:    18.1  
##  10866  :    57   Median :1992   Median :    74.7  
##  10874  :    57   Mean   :1991   Mean   :  1402.3  
##  10890  :    57   3rd Qu.:2002   3rd Qu.:   387.8  
##  11308  :    57   Max.   :2016   Max.   :715599.8  
##  (Other):259006                  NA's   :3157
```


```r
missing = ME %>% group_by(Period) %>% summarise(Missing=sum(is.na(Size)))
missing.pct = ME %>% group_by(Period) %>% summarise(Missing=sum(is.na(Size))/n())
ggplot(missing, aes(x=Period, y=Missing)) + geom_line()
```

![](Create_Factors_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggplot(missing.pct, aes(x=Period, y=Missing)) + geom_line()
```

![](Create_Factors_files/figure-html/unnamed-chunk-1-2.png)<!-- -->


```r
dim(crsp)
```

```
## [1] 3117344      15
```


```r
crsp = left_join(crsp, ME, by=c("PERMNO", "Period"))
```


```r
ME = crsp %>% filter(Month==12) %>% select(PERMNO, Period, ME)
ME$Period = ME$Period + 1
ME$M = ME$ME
ME$ME = NULL
summary(ME)
```

```
##      PERMNO           Period           M           
##  10145  :    57   Min.   :1961   Min.   :     0.0  
##  10516  :    57   1st Qu.:1981   1st Qu.:    16.7  
##  10866  :    57   Median :1992   Median :    72.3  
##  10874  :    57   Mean   :1992   Mean   :  1431.0  
##  10890  :    57   3rd Qu.:2002   3rd Qu.:   387.2  
##  11308  :    57   Max.   :2017   Max.   :643120.1  
##  (Other):262444                  NA's   :3461
```


```r
missing = ME %>% group_by(Period) %>% summarise(Missing=sum(is.na(M)))
missing.pct = ME %>% group_by(Period) %>% summarise(Missing=sum(is.na(M))/n())
ggplot(missing, aes(x=Period, y=Missing)) + geom_line()
```

![](Create_Factors_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
ggplot(missing.pct, aes(x=Period, y=Missing)) + geom_line()
```

![](Create_Factors_files/figure-html/unnamed-chunk-4-2.png)<!-- -->


```r
crsp = left_join(crsp, ME, by=c("PERMNO", "Period"))
```


```r
dim(crsp)
```

```
## [1] 3117344      17
```

## Prior Returns, $Prior$


```r
crsp$old.ri = crsp$ri
crsp$ri[is.null(crsp$ri)] = -.9999
crsp = crsp %>% group_by(PERMNO) %>%
    mutate(Prior.sum=
        lag(ri, 2) + lag(ri, 3) + lag(ri, 4) + lag(ri, 5) + lag(ri, 6) +
        lag(ri, 7) + lag(ri, 8) + lag(ri, 9) + lag(ri, 10) + lag(ri, 11) +
        lag(ri, 12),
        Prior.cum=log(
        (1+lag(ri, 2))*(1+lag(ri, 3))*(1+lag(ri, 4))*(1+lag(ri, 5))*(1+lag(ri, 6))*
        (1+lag(ri, 7))*(1+lag(ri, 8))*(1+lag(ri, 9))*(1+lag(ri, 10))*(1+lag(ri, 11))*
        (1+lag(ri, 12))),
        L13.price=lag(price, 13),
        L1.ri=lag(ri),
        L1.ME=lag(ME)
    ) %>% as.data.frame
crsp$ri = crsp$old.ri
crsp$old.ri = NULL

crsp$Prior = crsp$Prior.sum

summary(crsp %>% select(L1.ri, L13.price, Prior.sum, Prior.cum))
```

```
##      L1.ri         L13.price          Prior.sum        Prior.cum     
##  Min.   :-1.00   Min.   : -1832.5   Min.   :-4.3     Min.   :-Inf    
##  1st Qu.:-0.07   1st Qu.:     1.8   1st Qu.:-0.1     1st Qu.:-0.2    
##  Median : 0.00   Median :    10.2   Median : 0.1     Median : 0.1    
##  Mean   : 0.01   Mean   :    23.9   Mean   : 0.1     Mean   :-Inf    
##  3rd Qu.: 0.07   3rd Qu.:    23.9   3rd Qu.: 0.4     3rd Qu.: 0.3    
##  Max.   :24.00   Max.   :226000.0   Max.   :23.7     Max.   : 4.6    
##  NA's   :65196   NA's   :322643     NA's   :352882   NA's   :352882
```

## Adjusted Size

We hold portfolios for 12 months but the return each month will alter weights.
We only need to do this once because we hold all annual portfolios for the same
period.
Monthly portfolios do not require a size adjustment.


```r
crsp = ri.adj.Size(crsp)
```

# COMPUSTAT


```r
# Don't need to use colClasses here
comp = fread("C:/Data/CRSP/20171123_COMP_196001_201612.csv", showProgress=FALSE)
```


```r
comp$GVKEY = NULL

setnames(comp, c("LPERMNO"), c("PERMNO"))

comp$PERMNO = factor(comp$PERMNO)
comp$Date = as.Date(as.character(comp$datadate), "%Y%m%d")
```

## Short Years

There should be no duplicate $PERMNO$-$fyear$ pairs
(5 found).
CRSP data has 287 083 rows and
54 columns.


```r
# duplicated will omit return FALSE for the first instance of a duplicated row
# calling from the top and the bottom ensure all instances are caught
pairs.ix = duplicated(comp[, c("PERMNO", "fyear")]) |
           duplicated(comp[, c("PERMNO", "fyear")], fromLast=TRUE)
comp[pairs.ix] %>% arrange(PERMNO, fyear, fyr)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["PERMNO"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["datadate"],"name":[2],"type":["int"],"align":["right"]},{"label":["fyear"],"name":[3],"type":["int"],"align":["right"]},{"label":["indfmt"],"name":[4],"type":["chr"],"align":["left"]},{"label":["consol"],"name":[5],"type":["chr"],"align":["left"]},{"label":["popsrc"],"name":[6],"type":["chr"],"align":["left"]},{"label":["datafmt"],"name":[7],"type":["chr"],"align":["left"]},{"label":["curcd"],"name":[8],"type":["chr"],"align":["left"]},{"label":["fyr"],"name":[9],"type":["int"],"align":["right"]},{"label":["aco"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["act"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["ap"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["apc"],"name":[13],"type":["lgl"],"align":["right"]},{"label":["arc"],"name":[14],"type":["lgl"],"align":["right"]},{"label":["artfs"],"name":[15],"type":["lgl"],"align":["right"]},{"label":["at"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["ceq"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["ch"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["che"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["cogs"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["cshpri"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["dlc"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["dp"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["drc"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["drlt"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["gp"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["invfg"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["invt"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["ivst"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["lct"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["lt"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["ppegt"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["ppent"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["pstk"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["pstkl"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["pstkrv"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["rect"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["rectr"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["revt"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["sale"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["seq"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["txditc"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["upstk"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["urect"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["xacc"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["xinst"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["xint"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["xintd"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["xpp"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["xrd"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["xsga"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["costat"],"name":[52],"type":["chr"],"align":["left"]},{"label":["fyrc"],"name":[53],"type":["int"],"align":["right"]},{"label":["Date"],"name":[54],"type":["date"],"align":["right"]}],"data":[{"1":"22074","2":"19870630","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"44.777","11":"140.682","12":"0.000","13":"NA","14":"NA","15":"NA","16":"149.379","17":"63.262","18":"0.025","19":"95.701","20":"341.350","21":"13.953","22":"0.000","23":"5.031","24":"NA","25":"NA","26":"79.229","27":"0.000","28":"0.000","29":"95.676","30":"3.724","31":"86.117","32":"0.000","33":"0.000","34":"0.000","35":"0.000","36":"0.000","37":"0.204","38":"0.204","39":"420.579","40":"420.579","41":"63.262","42":"0","43":"NA","44":"NA","45":"3.724","46":"NA","47":"12.969","48":"NA","49":"0.000","50":"0.000","51":"67.039","52":"I","53":"6","54":"1987-06-30"},{"1":"22074","2":"19871231","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"10.900","11":"223.200","12":"187.100","13":"NA","14":"NA","15":"NA","16":"1056.500","17":"153.900","18":"NA","19":"3.300","20":"NA","21":"NA","22":"3.000","23":"NA","24":"NA","25":"NA","26":"NA","27":"NA","28":"182.500","29":"NA","30":"310.300","31":"902.600","32":"478.000","33":"460.300","34":"0.000","35":"0.000","36":"0.000","37":"26.500","38":"26.500","39":"NA","40":"NA","41":"153.900","42":"0","43":"NA","44":"NA","45":"120.200","46":"NA","47":"NA","48":"NA","49":"0.000","50":"NA","51":"NA","52":"I","53":"12","54":"1987-12-31"},{"1":"67563","2":"19980630","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"NA","11":"NA","12":"10698.265","13":"NA","14":"NA","15":"NA","16":"18116.737","17":"1234.060","18":"311.278","19":"485.478","20":"716.058","21":"52.354","22":"4148.551","23":"74.582","24":"NA","25":"NA","26":"546.775","27":"NA","28":"70.182","29":"174.200","30":"NA","31":"16878.060","32":"NA","33":"146.893","34":"4.617","35":"115.425","36":"4.617","37":"13742.673","38":"NA","39":"1262.833","40":"1262.833","41":"1238.677","42":"NA","43":"NA","44":"NA","45":"NA","46":"NA","47":"NA","48":"NA","49":"0.000","50":"NA","51":"185.259","52":"I","53":"6","54":"1998-06-30"},{"1":"67563","2":"19981231","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"NA","11":"NA","12":"24620.066","13":"NA","14":"NA","15":"NA","16":"54868.984","17":"1581.778","18":"854.954","19":"907.625","20":"1859.545","21":"79.557","22":"12394.326","23":"238.298","24":"NA","25":"NA","26":"1057.442","27":"NA","28":"2446.651","29":"52.671","30":"NA","31":"52693.768","32":"NA","33":"336.874","34":"0.000","35":"0.000","36":"0.000","37":"30280.944","38":"NA","39":"2916.987","40":"2916.987","41":"1581.778","42":"NA","43":"NA","44":"NA","45":"NA","46":"NA","47":"NA","48":"NA","49":"NA","50":"NA","51":"393.728","52":"I","53":"12","54":"1998-12-31"},{"1":"75228","2":"19871031","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"10","10":"0.135","11":"8.502","12":"2.200","13":"NA","14":"NA","15":"NA","16":"17.970","17":"7.217","18":"0.790","19":"0.790","20":"8.045","21":"3.936","22":"6.935","23":"0.228","24":"NA","25":"NA","26":"5.446","27":"2.392","28":"3.748","29":"0.000","30":"10.374","31":"10.753","32":"3.769","33":"3.177","34":"0.000","35":"0.000","36":"0.000","37":"3.829","38":"3.738","39":"13.491","40":"13.491","41":"7.217","42":"0","43":"NA","44":"NA","45":"1.170","46":"NA","47":"0.184","48":"NA","49":"0.135","50":"NA","51":"4.193","52":"I","53":"10","54":"1987-10-31"},{"1":"75228","2":"19871231","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"1.840","11":"14.113","12":"3.342","13":"NA","14":"NA","15":"NA","16":"45.769","17":"33.247","18":"3.617","19":"3.617","20":"2.044","21":"8.385","22":"7.657","23":"0.223","24":"NA","25":"NA","26":"2.196","27":"2.668","28":"4.026","29":"0.000","30":"12.075","31":"12.522","32":"8.642","33":"8.225","34":"0.000","35":"0.000","36":"0.000","37":"4.630","38":"4.630","39":"4.240","40":"4.240","41":"33.247","42":"0","43":"NA","44":"NA","45":"NA","46":"NA","47":"0.147","48":"NA","49":"0.000","50":"0.864","51":"3.478","52":"I","53":"12","54":"1987-12-31"},{"1":"85531","2":"19980630","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"NA","11":"NA","12":"NA","13":"NA","14":"NA","15":"NA","16":"NA","17":"NA","18":"NA","19":"NA","20":"90.868","21":"3.797","22":"NA","23":"NA","24":"NA","25":"NA","26":"16.265","27":"NA","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"NA","35":"NA","36":"NA","37":"NA","38":"NA","39":"107.133","40":"107.133","41":"NA","42":"NA","43":"NA","44":"NA","45":"NA","46":"NA","47":"NA","48":"NA","49":"NA","50":"NA","51":"12.021","52":"I","53":"6","54":"1998-06-30"},{"1":"85531","2":"19981231","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"6.099","11":"42.083","12":"1.394","13":"NA","14":"NA","15":"NA","16":"91.463","17":"53.380","18":"20.439","19":"20.439","20":"177.740","21":"11.172","22":"0.067","23":"3.923","24":"NA","25":"NA","26":"31.632","27":"0.000","28":"0.000","29":"0.000","30":"30.043","31":"38.083","32":"41.030","33":"31.482","34":"0.000","35":"0.000","36":"0.000","37":"15.545","38":"13.302","39":"209.372","40":"209.372","41":"53.380","42":"0","43":"NA","44":"NA","45":"20.365","46":"NA","47":"NA","48":"NA","49":"0.000","50":"NA","51":"18.972","52":"A","53":"12","54":"1998-12-31"},{"1":"88031","2":"19900630","3":"1990","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"0.000","11":"1.571","12":"0.000","13":"NA","14":"NA","15":"NA","16":"1.613","17":"1.569","18":"NA","19":"1.571","20":"0.089","21":"1.257","22":"0.000","23":"0.000","24":"NA","25":"NA","26":"0.043","27":"0.000","28":"0.000","29":"NA","30":"0.004","31":"0.044","32":"0.000","33":"0.000","34":"0.000","35":"0.000","36":"0.000","37":"0.000","38":"0.000","39":"0.132","40":"0.132","41":"1.569","42":"0","43":"NA","44":"NA","45":"0.004","46":"NA","47":"NA","48":"NA","49":"0.000","50":"NA","51":"NA","52":"I","53":"6","54":"1990-06-30"},{"1":"88031","2":"19901231","3":"1990","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"0.000","11":"7.134","12":"0.072","13":"NA","14":"NA","15":"NA","16":"7.139","17":"6.924","18":"7.116","19":"7.116","20":"NA","21":"NA","22":"0.000","23":"NA","24":"NA","25":"NA","26":"NA","27":"0.000","28":"0.000","29":"0.000","30":"0.178","31":"0.215","32":"0.000","33":"0.000","34":"0.000","35":"0.000","36":"0.000","37":"0.018","38":"0.000","39":"NA","40":"NA","41":"6.924","42":"0","43":"NA","44":"NA","45":"0.106","46":"NA","47":"NA","48":"NA","49":"0.000","50":"NA","51":"NA","52":"A","53":"12","54":"1990-12-31"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
comp = comp %>% filter(!duplicated(comp[,c("PERMNO", "fyear")]))
```

## Holding Period


```r
comp$Period = comp$fyear + 1
```

## Book Equity, $BE$


```r
comp$ps = comp$pstkrv                                   # Redemption
comp$ps[is.na(comp$ps)] = comp$pstkl[is.na(comp$ps)]    # Liquidation
comp$ps[is.na(comp$ps)] = comp$pstk[is.na(comp$ps)]     # Book
comp$ps[is.na(comp$ps)] = 0

comp$txditc[is.na(comp$txditc)] = 0

# Asness and Frazzini use only seq

comp$BE = comp$seq + comp$txditc - comp$ps

comp$upstk[is.na(comp$upstk)] = 0

# Common Equity PLUS Par Value of Preferred Stock?
comp$BE[is.na(comp$BE)] = comp$ceq[is.na(comp$BE)] + comp$upstk[is.na(comp$BE)]

comp$BE[is.na(comp$BE)] = comp$at[is.na(comp$BE)] - comp$lt[is.na(comp$BE)]

comp = comp %>% group_by(PERMNO) %>% mutate(D1.BE=BE-lag(BE)) %>% as.data.frame

summary(comp[,c("BE", "D1.BE")])
```

```
##        BE               D1.BE          
##  Min.   :-96620.0   Min.   :-99618.00  
##  1st Qu.:    13.6   1st Qu.:    -1.02  
##  Median :    63.3   Median :     2.92  
##  Mean   :  1036.6   Mean   :    67.66  
##  3rd Qu.:   309.8   3rd Qu.:    26.02  
##  Max.   :359960.0   Max.   :223210.69  
##  NA's   :19540      NA's   :43668
```

## Operating Profit, $OP$

Compustat has $REVT$ and $SALE$ values.

```r
summary(comp %>% select(revt, sale))
```

```
##       revt               sale         
##  Min.   :-15009.3   Min.   :-15009.3  
##  1st Qu.:    25.5   1st Qu.:    25.5  
##  Median :   114.2   Median :   114.2  
##  Mean   :  1762.0   Mean   :  1762.0  
##  3rd Qu.:   571.3   3rd Qu.:   571.3  
##  Max.   :483521.0   Max.   :483521.0  
##  NA's   :19880      NA's   :19880
```

Whichever one we use the expenses check is the same.


```r
expenses.OK = !is.na(comp$cogs) | !is.na(!comp$xsga) | !is.na(comp$xint)
comp$cogs[is.na(comp$cogs)] = 0
comp$xsga[is.na(comp$xsga)] = 0
comp$xint[is.na(comp$xint)] = 0

comp$op.revt.OK = !is.na(comp$revt) & expenses.OK
comp$op.sale.OK = !is.na(comp$sale) & expenses.OK

comp$OP.revt.OK = (comp$BE > 0) & comp$op.revt.OK
comp$OP.sale.OK = (comp$BE > 0) & comp$op.sale.OK

comp$OP.revt.OK[is.na(comp$OP.revt.OK)] = FALSE
comp$OP.sale.OK[is.na(comp$OP.sale.OK)] = FALSE

comp$OP.revt = (comp$revt - comp$cogs - comp$xsga - comp$xint)/comp$BE
comp$OP.sale = (comp$sale - comp$cogs - comp$xsga - comp$xint)/comp$BE

summary(comp %>% select(OP.revt, OP.sale))
```

```
##     OP.revt         OP.sale     
##  Min.   : -Inf   Min.   : -Inf  
##  1st Qu.:0.057   1st Qu.:0.057  
##  Median :0.196   Median :0.196  
##  Mean   :  NaN   Mean   :  NaN  
##  3rd Qu.:0.315   3rd Qu.:0.315  
##  Max.   :  Inf   Max.   :  Inf  
##  NA's   :20724   NA's   :20724
```

```r
summary(comp$OP.revt[comp$OP.revt.OK])
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -1941.000     0.062     0.196     0.132     0.310  9424.000
```

```r
summary(comp$OP.sale[comp$OP.sale.OK])
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -1941.000     0.062     0.196     0.132     0.310  9424.000
```

```r
comp %>% select(revt, OP.revt, sale, OP.sale) %>% filter(revt!=sale) %>% head %>% round(3)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["revt"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["OP.revt"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["sale"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["OP.sale"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

They seem to be the same.


```r
comp$op.OK = comp$op.revt.OK
comp$OP.OK = comp$OP.revt.OK
comp$OP = comp$OP.revt
```

### RnD


```r
summary(comp$xrd)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##    -0.65     0.10     2.30    68.56    15.64 16080.00   162030
```

```r
comp$xrd[is.na(comp$xrd)] = 0
comp$OP.2 = (comp$revt - comp$cogs - comp$xsga - comp$xint + comp$xrd)/comp$BE
summary(comp %>% filter(OP.OK) %>% select(OP, OP.2))
```

```
##        OP                 OP.2         
##  Min.   :-1941.333   Min.   :-945.333  
##  1st Qu.:    0.062   1st Qu.:   0.092  
##  Median :    0.196   Median :   0.223  
##  Mean   :    0.132   Mean   :   0.255  
##  3rd Qu.:    0.310   3rd Qu.:   0.346  
##  Max.   : 9423.750   Max.   :9423.750
```

## Gross Profit, $GP$


```r
comp$GP = comp$gp / comp$at
summary(comp$GP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -Inf   0.097   0.266     NaN   0.460     Inf   20236
```

```r
ix = is.na(comp$GP)
comp$GP[ix] = (comp$revt[ix] - comp$cogs[ix]) / comp$at[ix]
comp$GP[is.infinite(comp$GP)] = NA
summary(comp$GP)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
## -134.200    0.097    0.266    0.308    0.460  227.400    20006
```

## Cash Profit, $CP$


```r
summary(comp %>% select(rect, xpp, ap, invt, drc, xacc))
```

```
##       rect                xpp                 ap           
##  Min.   :      0.0   Min.   :    0.00   Min.   :      0.0  
##  1st Qu.:      3.9   1st Qu.:    0.00   1st Qu.:      1.9  
##  Median :     21.4   Median :    0.02   Median :      9.5  
##  Mean   :   1678.8   Mean   :   14.81   Mean   :   1603.4  
##  3rd Qu.:    142.3   3rd Qu.:    1.06   3rd Qu.:     76.1  
##  Max.   :2080955.3   Max.   :20091.00   Max.   :1974374.5  
##  NA's   :26245       NA's   :129342     NA's   :31584      
##       invt               drc                xacc         
##  Min.   :    -2.7   Min.   :    0.00   Min.   :  -20.27  
##  1st Qu.:     0.3   1st Qu.:    0.00   1st Qu.:    1.09  
##  Median :     6.9   Median :    0.00   Median :    5.63  
##  Mean   :   307.3   Mean   :   50.83   Mean   :  110.61  
##  3rd Qu.:    47.3   3rd Qu.:    4.31   3rd Qu.:   31.85  
##  Max.   :472266.2   Max.   :27468.00   Max.   :65614.00  
##  NA's   :26346      NA's   :217718     NA's   :99451
```

```r
apply(
    comp %>% select(rect, xpp, ap, invt, drc, xacc),
    2,
    function(x) sum(is.na(x)/length(x)*100)
) %>% round(2)
```

```
##  rect   xpp    ap  invt   drc  xacc 
##  9.14 45.05 11.00  9.18 75.84 34.64
```

Let's assume for now that firms don't need all of these values.
We'll set everything that is missing, except $revt$ and $ap$, to 0.
This essentially requires firms to have both Receivables and Payables to consider their accruals.


```r
comp$xpp[is.na(comp$xpp)] = 0
comp$invt[is.na(comp$invt)] = 0
comp$drc[is.na(comp$drc)] = 0
comp$xacc[is.na(comp$xacc)] = 0
```


```r
comp = comp %>% group_by(PERMNO) %>% mutate(
    D1.rect=rect-lag(rect), D1.xpp=xpp-lag(xpp),
    D1.ap=ap-lag(ap), D1.invt=invt-lag(invt),
    D1.drc=drc-lag(drc), D1.xacc=xacc-lag(xacc)
) %>% as.data.frame
```


```r
summary(comp %>% select(D1.rect, D1.xpp, D1.ap, D1.invt, D1.drc, D1.xacc))
```

```
##     D1.rect              D1.xpp               D1.ap          
##  Min.   :-600169.0   Min.   :-10503.170   Min.   :-662237.4  
##  1st Qu.:     -0.6   1st Qu.:     0.000   1st Qu.:     -0.5  
##  Median :      0.9   Median :     0.000   Median :      0.4  
##  Mean   :    100.2   Mean   :     0.373   Mean   :    108.9  
##  3rd Qu.:     10.5   3rd Qu.:     0.000   3rd Qu.:      5.8  
##  Max.   : 709139.0   Max.   :  6937.000   Max.   : 580318.3  
##  NA's   :50375       NA's   :25137        NA's   :55590      
##     D1.invt              D1.drc             D1.xacc         
##  Min.   :-379695.5   Min.   :-5280.000   Min.   :-65614.00  
##  1st Qu.:     -0.1   1st Qu.:    0.000   1st Qu.:     0.00  
##  Median :      0.0   Median :    0.000   Median :     0.00  
##  Mean   :     16.0   Mean   :    1.647   Mean   :     4.26  
##  3rd Qu.:      2.5   3rd Qu.:    0.000   3rd Qu.:     0.86  
##  Max.   : 224230.5   Max.   :12323.000   Max.   : 44511.00  
##  NA's   :25137       NA's   :25137       NA's   :25137
```

```r
apply(
    comp %>% select(D1.rect, D1.xpp, D1.ap, D1.invt, D1.drc, D1.xacc),
    2,
    function(x) sum(is.na(x)/length(x)*100)
) %>% round(2)
```

```
## D1.rect  D1.xpp   D1.ap D1.invt  D1.drc D1.xacc 
##   17.55    8.76   19.36    8.76    8.76    8.76
```


```r
comp = comp %>% mutate(OP.AccFama=D1.rect+D1.xpp-D1.ap-D1.invt-D1.drc-D1.xacc)
comp = comp %>% mutate(OP.Acc=-D1.rect-D1.invt-D1.xpp+D1.drc+D1.ap+D1.xacc)
summary(comp %>% select(OP.AccFama, OP.Acc))
```

```
##    OP.AccFama            OP.Acc         
##  Min.   :-702558.3   Min.   :-403256.0  
##  1st Qu.:     -8.2   1st Qu.:     -7.9  
##  Median :     -0.3   Median :     -0.4  
##  Mean   :    -32.0   Mean   :     -4.0  
##  3rd Qu.:      3.4   3rd Qu.:      3.5  
##  Max.   : 394654.8   Max.   : 704184.1  
##  NA's   :61215       NA's   :61215
```

Now for years with missing accruals we don't want this to alter the non-missing operating profits we have.


```r
comp$OP.Acc[is.na(comp$OP.Acc)] = 0
summary(comp$OP.Acc)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -403300.0      -4.0       0.0      -3.1       1.3  704200.0
```


```r
comp$CP = (comp$revt-comp$cogs-comp$xsga-comp$xint+comp$xrd+comp$OP.Acc)/comp$BE
summary(comp %>% filter(OP.OK) %>% select(OP, OP.2, GP, CP))
```

```
##        OP                 OP.2                GP           
##  Min.   :-1941.333   Min.   :-945.333   Min.   :-31.58693  
##  1st Qu.:    0.062   1st Qu.:   0.092   1st Qu.:  0.09808  
##  Median :    0.196   Median :   0.223   Median :  0.26675  
##  Mean   :    0.132   Mean   :   0.255   Mean   :  0.31175  
##  3rd Qu.:    0.310   3rd Qu.:   0.346   3rd Qu.:  0.45893  
##  Max.   : 9423.750   Max.   :9423.750   Max.   :227.44910  
##                                         NA's   :12         
##        CP           
##  Min.   :-1270.000  
##  1st Qu.:    0.042  
##  Median :    0.200  
##  Mean   :    0.283  
##  3rd Qu.:    0.352  
##  Max.   : 8683.125  
## 
```

## Accruals, $Ac$


```r
summary(comp %>% select(act, ch, lct, dlc))
```

```
##       act                  ch                lct          
##  Min.   :     0.00   Min.   :  -279.1   Min.   :     0.0  
##  1st Qu.:    13.50   1st Qu.:     1.3   1st Qu.:     5.7  
##  Median :    54.11   Median :     7.7   Median :    23.6  
##  Mean   :   649.02   Mean   :   247.7   Mean   :   495.2  
##  3rd Qu.:   237.83   3rd Qu.:    46.0   3rd Qu.:   120.9  
##  Max.   :161978.00   Max.   :380619.4   Max.   :329795.0  
##  NA's   :62161       NA's   :50779      NA's   :58554     
##       dlc          
##  Min.   : -3753.5  
##  1st Qu.:     0.1  
##  Median :     2.4  
##  Mean   :   549.8  
##  3rd Qu.:    21.2  
##  Max.   :575319.4  
##  NA's   :21507
```

We will require that firms have both current assets and liabilities.


```r
comp$ch[is.na(comp$ch)] = 0
comp$dlc[is.na(comp$dlc)] = 0

comp$WC = comp$act - comp$ch - comp$lct + comp$dlc
comp = comp %>% group_by(PERMNO) %>% mutate(D1.WC=WC-lag(WC)) %>% as.data.frame

comp$Acc = comp$D1.WC / comp$BE

comp$Acc[is.infinite(comp$Acc)] = 0

summary(comp$Acc[comp$BE>0])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## -713.50   -0.06    0.02   -0.03    0.11 2317.00   82093
```

## Investment, $INV$


```r
comp = comp %>% group_by(PERMNO) %>% mutate(INV=(at-lag(at))/lag(at)) %>%
    as.data.frame
comp$INV[(comp$INV==Inf) | (comp$INV==-Inf)] = NA
summary(comp$INV)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   -1.00   -0.03    0.07    0.22    0.19 7898.00   42930
```

Makes more sense to define investment as the change in PPE plus depreciation.


```r
comp = comp %>% group_by(PERMNO) %>%
    mutate(INV.ppe=(ppent-lag(ppent)+dp)/lag(ppent)) %>%
    as.data.frame
comp$INV.ppe[(comp$INV.ppe==Inf) | (comp$INV.ppe==-Inf)] = NA
summary(comp$INV.ppe)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##    -1.00     0.11     0.24     1.77     0.50 74240.00    60234
```

## Missing Values


```r
missing = comp %>% filter(Date>as.Date("1963-06-30")) %>%
    group_by(Period) %>% summarise(
        BE=sum(is.na(BE)),
        OP=sum(!op.OK),
        GP=sum(is.na(GP)),
        OP_BE=sum(!OP.OK),
        INV=sum(is.na(INV))
) %>% melt(id="Period") %>% filter(!is.na(Period))

ggplot(missing, aes(x=Period, color=variable)) + geom_line(aes(y=value))
```

![](Create_Factors_files/figure-html/comp.missing.val-1.png)<!-- -->


```r
missing = comp %>% filter(Date>as.Date("1963-06-30")) %>%
    group_by(Period) %>% summarise(
        BE=sum(is.na(BE))/n(),
        OP=sum(!op.OK)/n(),
        GP=sum(is.na(GP))/n(),
        OP_BE=sum(!OP.OK)/n(),
        INV=sum(is.na(INV))/n()
) %>% melt(id="Period") %>% filter(!is.na(Period))

ggplot(missing, aes(x=Period, color=variable)) + geom_line(aes(y=value))
```

![](Create_Factors_files/figure-html/comp.missing.pct-1.png)<!-- -->

# Univariate Sorts

## $Size$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
```

### $ME$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", !is.na(ME)) %>% select(Date, PERMNO, ME)

breakpoints = nyse %>% arrange(Date, ME) %>% group_by(Date) %>% summarize(
    D10=ME[.1*n()], D20=ME[.2*n()],
    D30=ME[.3*n()], D40=ME[.4*n()],
    D50=ME[.5*n()], D60=ME[.6*n()],
    D70=ME[.7*n()], D80=ME[.8*n()],
    D90=ME[.9*n()], D100=ME[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/ME_Decile_Breakpoints.csv")
```

### $Size$ Deciles


```r
quantiles = 1:10/10

nyse = df %>% filter(
    Exchange=="NYSE", Month==7, !is.na(Size)
    ) %>% select(Period, PERMNO, Size)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "Size")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "Size", quantiles)
```


```r
df %>% group_by(Size.B, Date) %>% summarize(N=n()) %>% group_by(Size.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2106.2461"},{"1":"2","2":"561.3692"},{"1":"3","2":"372.4221"},{"1":"4","2":"300.3925"},{"1":"5","2":"247.8287"},{"1":"6","2":"210.9003"},{"1":"7","2":"191.6822"},{"1":"8","2":"182.4268"},{"1":"9","2":"166.5312"},{"1":"10","2":"169.7383"},{"1":"NA","2":"289.4404"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

$Size$ adjusted for last month's return


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ Size.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/Size_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 1.15 1.10 1.12 1.10 1.10 1.05 1.05 1.04 0.96 0.83 0.44
```

![French Website $Size$ Decile Returns](C:/Data/FrenchDartmouth/10_ME.JPG)

## $BM$ (Annual, Lagged)

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(BM), !is.na(Size), BE>0)
```

### $BM$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, BM)

breakpoints = nyse %>% arrange(Period, BM) %>% group_by(Period) %>% summarize(
    D10=BM[.1*n()], D20=BM[.2*n()],
    D30=BM[.3*n()], D40=BM[.4*n()],
    D50=BM[.5*n()], D60=BM[.6*n()],
    D70=BM[.7*n()], D80=BM[.8*n()],
    D90=BM[.9*n()], D100=BM[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/BM_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "BM")] = NULL

df = left_join(df, breakpoints, by=c("Period"))

df = assign.bkts(df, "BM", quantiles)
```


```r
df %>% group_by(BM.B, Date) %>% summarize(N=n()) %>% group_by(BM.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"600.81776"},{"1":"2","2":"399.37850"},{"1":"3","2":"353.13084"},{"1":"4","2":"333.83489"},{"1":"5","2":"310.41745"},{"1":"6","2":"310.24299"},{"1":"7","2":"309.64174"},{"1":"8","2":"327.11059"},{"1":"9","2":"363.24611"},{"1":"10","2":"505.99688"},{"1":"NA","2":"16.71519"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(BM.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ BM.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/BM_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.81 0.92 0.90 0.88 1.04 0.98 0.99 1.10 1.06 1.15 1.22
```

![French Website $BM$ Decile Returns](C:/Data/FrenchDartmouth/10_BM.JPG)

### $ME$


```r
df %>% group_by(BM.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(BM.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2883.84"},{"1":"2","2":"2704.91"},{"1":"3","2":"2233.62"},{"1":"4","2":"1888.02"},{"1":"5","2":"1503.88"},{"1":"6","2":"1400.40"},{"1":"7","2":"1054.26"},{"1":"8","2":"973.99"},{"1":"9","2":"817.43"},{"1":"10","2":"530.22"},{"1":"NA","2":"89.24"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(BM.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(BM.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2792.68"},{"1":"2","2":"2596.04"},{"1":"3","2":"2154.33"},{"1":"4","2":"1822.38"},{"1":"5","2":"1432.28"},{"1":"6","2":"1360.15"},{"1":"7","2":"1016.14"},{"1":"8","2":"937.60"},{"1":"9","2":"786.40"},{"1":"10","2":"502.26"},{"1":"NA","2":"74.73"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(BM.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(BM.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.14"},{"1":"2","2":"0.13"},{"1":"3","2":"0.13"},{"1":"4","2":"0.12"},{"1":"5","2":"0.13"},{"1":"6","2":"0.13"},{"1":"7","2":"0.14"},{"1":"8","2":"0.14"},{"1":"9","2":"0.15"},{"1":"10","2":"0.17"},{"1":"NA","2":"0.45"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Forecasting $BE$

We want to distinguish between changes in price and changes in expected $BE$.

Exhibit 4 from Asness and Frazzini (2013).

$$BM_t = \beta_0 + \beta_1\cdot BM_{t-1} + \beta_2\cdot BM_{t-1}^{Current ME}$$


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(BM), Month==7)
```


```r
df = df %>% group_by(PERMNO) %>% mutate(
    L1.BM=lag(BM),
    L1.BMC=lag(BMC),
    L1.BMC_L1.BM=L1.BMC-L1.BM) %>% as.data.frame
```


```r
summary(df %>% select(PERMNO, L1.BM, L1.BMC, L1.BMC_L1.BM))
```

```
##     PERMNO              L1.BM              L1.BMC        
##  Length:219931      Min.   :-906.639   Min.   :-222.335  
##  Class :character   1st Qu.:   0.350   1st Qu.:   0.327  
##  Mode  :character   Median :   0.654   Median :   0.624  
##                     Mean   :   0.860   Mean   :   0.818  
##                     3rd Qu.:   1.116   3rd Qu.:   1.063  
##                     Max.   : 294.428   Max.   : 202.201  
##                     NA's   :20089      NA's   :20248     
##   L1.BMC_L1.BM     
##  Min.   :-194.602  
##  1st Qu.:  -0.144  
##  Median :  -0.024  
##  Mean   :  -0.042  
##  3rd Qu.:   0.067  
##  Max.   : 850.603  
##  NA's   :20248
```


```r
formula.str = "BM ~ L1.BM + L1.BMC_L1.BM"
results = c()
for (period in 1964:2016){
    dt = df %>% filter(Period==period)
    results = rbind(results, lm(formula(formula.str), data=dt)$coefficients)
}

results = as.data.frame(results)
results$diff = results$L1.BM - results$L1.BMC_L1.BM

rbind(sapply(results, mean), sapply(results, se), sapply(results, t.stat))
```

```
##      (Intercept)       L1.BM L1.BMC_L1.BM       diff
## [1,]  0.04855788  0.97065682   0.86610139 0.10455543
## [2,]  0.04379389  0.06168029   0.09389111 0.06856677
## [3,]  1.10878220 15.73690546   9.22453024 1.52487034
```


## $\Delta log(Book Price/Share)$

Exhibit 5 from Asness and Frazzini (2013).

We want to forecast unobserved changes in book value per share.
Regress changes in book value per share on the lagged prior returns.
Prior returns are already lagged but the change in book value per share uses a
lag. Also see Sloan et al. (2017).


```r
df = combine.sources(crsp, comp)

df$bp = log(df$BE / df$shares)

df = df %>% filter(!is.na(bp), !is.infinite(bp), Month==7) %>% group_by(PERMNO) %>%
    mutate(D1.bp=bp-lag(bp), L1.Prior=lag(Prior.cum)) %>% as.data.frame
```


```r
summary(df %>% select(PERMNO, Period, D1.bp, L1.Prior))
```

```
##     PERMNO              Period         D1.bp            L1.Prior     
##  Length:214896      Min.   :1962   Min.   :-10.017   Min.   :-6.190  
##  Class :character   1st Qu.:1983   1st Qu.: -0.089   1st Qu.:-0.194  
##  Mode  :character   Median :1994   Median :  0.052   Median : 0.061  
##                     Mean   :1993   Mean   :  0.005   Mean   : 0.033  
##                     3rd Qu.:2003   3rd Qu.:  0.142   3rd Qu.: 0.291  
##                     Max.   :2016   Max.   :  8.555   Max.   : 3.819  
##                                    NA's   :19932     NA's   :29631
```


```r
df %>% group_by(Period) %>% summarise(N=n())
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Period"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["N"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"1962","2":"518"},{"1":"1963","2":"916"},{"1":"1964","2":"1002"},{"1":"1965","2":"1097"},{"1":"1966","2":"1231"},{"1":"1967","2":"1453"},{"1":"1968","2":"1546"},{"1":"1969","2":"1704"},{"1":"1970","2":"1869"},{"1":"1971","2":"1999"},{"1":"1972","2":"2116"},{"1":"1973","2":"3132"},{"1":"1974","2":"3641"},{"1":"1975","2":"3892"},{"1":"1976","2":"3871"},{"1":"1977","2":"3844"},{"1":"1978","2":"3756"},{"1":"1979","2":"3786"},{"1":"1980","2":"3880"},{"1":"1981","2":"3974"},{"1":"1982","2":"4226"},{"1":"1983","2":"4261"},{"1":"1984","2":"4640"},{"1":"1985","2":"4658"},{"1":"1986","2":"4621"},{"1":"1987","2":"4817"},{"1":"1988","2":"4916"},{"1":"1989","2":"4752"},{"1":"1990","2":"4645"},{"1":"1991","2":"4614"},{"1":"1992","2":"4655"},{"1":"1993","2":"4957"},{"1":"1994","2":"5933"},{"1":"1995","2":"6181"},{"1":"1996","2":"6372"},{"1":"1997","2":"6732"},{"1":"1998","2":"6535"},{"1":"1999","2":"6101"},{"1":"2000","2":"5977"},{"1":"2001","2":"5619"},{"1":"2002","2":"5074"},{"1":"2003","2":"4731"},{"1":"2004","2":"4541"},{"1":"2005","2":"4485"},{"1":"2006","2":"4401"},{"1":"2007","2":"4316"},{"1":"2008","2":"4265"},{"1":"2009","2":"3948"},{"1":"2010","2":"3761"},{"1":"2011","2":"3633"},{"1":"2012","2":"3535"},{"1":"2013","2":"3413"},{"1":"2014","2":"3430"},{"1":"2015","2":"3519"},{"1":"2016","2":"3405"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
formula.str = "D1.bp ~ L1.Prior"

results = c()
for (period in 1964:2016){
    dt = df %>% filter(Period==period)
    results = rbind(results, lm(formula(formula.str), data=dt)$coefficients)
}

results = as.data.frame(results)

rbind(sapply(results, mean), sapply(results, se), sapply(results, t.stat))
```

```
##      (Intercept)   L1.Prior
## [1,] 0.000670307  0.1803592
## [2,] 0.006073168  0.0116269
## [3,] 0.110371889 15.5122378
```

## $BM_C$ (Annual, Current)

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(BMC), !is.na(Size), BE>0)
```

### $BM$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, BMC)

breakpoints = nyse %>% arrange(Period, BMC) %>% group_by(Period) %>% summarize(
    D10=BMC[.1*n()], D20=BMC[.2*n()],
    D30=BMC[.3*n()], D40=BMC[.4*n()],
    D50=BMC[.5*n()], D60=BMC[.6*n()],
    D70=BMC[.7*n()], D80=BMC[.8*n()],
    D90=BMC[.9*n()], D100=BMC[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/BMC_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "BMC")] = NULL

df = left_join(df, breakpoints, by=c("Period"))

df = assign.bkts(df, "BMC", quantiles)
```


```r
df %>% group_by(BMC.B, Date) %>% summarize(N=n()) %>% group_by(BMC.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"610.18536"},{"1":"2","2":"383.19470"},{"1":"3","2":"349.38941"},{"1":"4","2":"329.30841"},{"1":"5","2":"322.72430"},{"1":"6","2":"319.99221"},{"1":"7","2":"321.06542"},{"1":"8","2":"335.71807"},{"1":"9","2":"367.13396"},{"1":"10","2":"491.49688"},{"1":"NA","2":"21.56009"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(BMC.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ BMC.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/BMC_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.84 0.88 0.94 0.92 0.86 1.02 0.94 1.09 1.13 1.19 0.30
```

### $ME$


```r
df %>% group_by(BMC.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(BMC.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2842.99"},{"1":"2","2":"2661.47"},{"1":"3","2":"2419.74"},{"1":"4","2":"1819.49"},{"1":"5","2":"1685.96"},{"1":"6","2":"1306.59"},{"1":"7","2":"1156.71"},{"1":"8","2":"927.94"},{"1":"9","2":"688.50"},{"1":"10","2":"521.69"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(BMC.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(BMC.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2751.66"},{"1":"2","2":"2567.18"},{"1":"3","2":"2338.13"},{"1":"4","2":"1756.02"},{"1":"5","2":"1630.66"},{"1":"6","2":"1270.96"},{"1":"7","2":"1119.02"},{"1":"8","2":"895.46"},{"1":"9","2":"670.79"},{"1":"10","2":"490.23"},{"1":"NA","2":"134.93"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(BMC.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(BMC.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.28"},{"1":"2","2":"0.20"},{"1":"3","2":"0.17"},{"1":"4","2":"0.15"},{"1":"5","2":"0.13"},{"1":"6","2":"0.12"},{"1":"7","2":"0.10"},{"1":"8","2":"0.09"},{"1":"9","2":"0.08"},{"1":"10","2":"0.02"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $BM_M$ (Monthly, Current)

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(BMM), !is.na(L1.ME), BE>0)
```

### $BMM$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, BMM)

breakpoints = nyse %>% arrange(Date, BMM) %>% group_by(Date) %>% summarize(
    D10=BMM[.1*n()], D20=BMM[.2*n()],
    D30=BMM[.3*n()], D40=BMM[.4*n()],
    D50=BMM[.5*n()], D60=BMM[.6*n()],
    D70=BMM[.7*n()], D80=BMM[.8*n()],
    D90=BMM[.9*n()], D100=BMM[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/BMM_Decile_Breakpoints.csv")

# breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "BMM")] = NULL

df = left_join(df, breakpoints, by=c("Date"))

df = assign.bkts(df, "BMM", quantiles)
```


```r
df %>% group_by(BMM.B, Date) %>% summarize(N=n()) %>% group_by(BMM.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"545.558104"},{"1":"2","2":"367.223242"},{"1":"3","2":"338.117737"},{"1":"4","2":"325.350153"},{"1":"5","2":"324.123853"},{"1":"6","2":"329.172783"},{"1":"7","2":"333.175841"},{"1":"8","2":"349.047401"},{"1":"9","2":"383.732416"},{"1":"10","2":"481.946483"},{"1":"NA","2":"3.621495"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(BMM.B, Date) %>% mutate(
    bkt.L1.ME=sum(L1.ME, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$L1.ME / df$bkt.L1.ME
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ BMM.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/BMM_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.90 0.89 0.91 0.93 0.97 1.05 1.02 1.18 1.27 1.33 3.95
```

### $ME$


```r
df %>% group_by(BMM.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(BMM.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2880.98"},{"1":"2","2":"2766.78"},{"1":"3","2":"2251.22"},{"1":"4","2":"1960.83"},{"1":"5","2":"1578.19"},{"1":"6","2":"1398.45"},{"1":"7","2":"1118.28"},{"1":"8","2":"928.30"},{"1":"9","2":"705.73"},{"1":"10","2":"420.14"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(BMM.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(BMM.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2665.05"},{"1":"2","2":"2632.21"},{"1":"3","2":"2157.54"},{"1":"4","2":"1895.31"},{"1":"5","2":"1534.03"},{"1":"6","2":"1369.07"},{"1":"7","2":"1106.04"},{"1":"8","2":"935.07"},{"1":"9","2":"730.61"},{"1":"10","2":"451.29"},{"1":"NA","2":"49.66"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(BMM.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(BMM.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.40"},{"1":"2","2":"0.25"},{"1":"3","2":"0.20"},{"1":"4","2":"0.17"},{"1":"5","2":"0.14"},{"1":"6","2":"0.11"},{"1":"7","2":"0.09"},{"1":"8","2":"0.06"},{"1":"9","2":"0.01"},{"1":"10","2":"-0.13"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $OP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(OP.OK, !is.na(Size))
```

### $OP$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, OP)

breakpoints = nyse %>% arrange(Period, OP) %>% group_by(Period) %>% summarize(
    D10=OP[.1*n()], D20=OP[.2*n()],
    D30=OP[.3*n()], D40=OP[.4*n()],
    D50=OP[.5*n()], D60=OP[.6*n()],
    D70=OP[.7*n()], D80=OP[.8*n()],
    D90=OP[.9*n()], D100=OP[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/OP_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "OP")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "OP", quantiles)
```


```r
df %>% group_by(OP.B, Date) %>% summarize(N=n()) %>% group_by(OP.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"954.36137"},{"1":"2","2":"413.24455"},{"1":"3","2":"323.93146"},{"1":"4","2":"311.92835"},{"1":"5","2":"310.70561"},{"1":"6","2":"312.30374"},{"1":"7","2":"294.26324"},{"1":"8","2":"286.35358"},{"1":"9","2":"288.51713"},{"1":"10","2":"320.10592"},{"1":"NA","2":"14.87912"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(OP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ OP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/OP_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.71 0.80 0.81 0.92 0.82 0.95 0.91 0.98 0.99 0.96 1.55
```

![French Website $OP$ Decile Returns](C:/Data/FrenchDartmouth/10_OP.JPG)

### $BM$


```r
df %>% group_by(OP.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(OP.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.20"},{"1":"2","2":"1.30"},{"1":"3","2":"1.14"},{"1":"4","2":"1.02"},{"1":"5","2":"0.93"},{"1":"6","2":"0.84"},{"1":"7","2":"0.75"},{"1":"8","2":"0.68"},{"1":"9","2":"0.58"},{"1":"10","2":"0.47"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Monthly $BM$


```r
df %>% group_by(OP.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(OP.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.30"},{"1":"2","2":"1.30"},{"1":"3","2":"1.13"},{"1":"4","2":"1.00"},{"1":"5","2":"0.89"},{"1":"6","2":"0.82"},{"1":"7","2":"0.72"},{"1":"8","2":"0.65"},{"1":"9","2":"0.61"},{"1":"10","2":"0.45"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(OP.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(OP.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.12"},{"1":"2","2":"0.12"},{"1":"3","2":"0.13"},{"1":"4","2":"0.13"},{"1":"5","2":"0.14"},{"1":"6","2":"0.14"},{"1":"7","2":"0.15"},{"1":"8","2":"0.15"},{"1":"9","2":"0.16"},{"1":"10","2":"0.18"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $INV$


```r
df %>% group_by(OP.B, Date) %>% summarise(avg.INV=mean(INV, na.rm=TRUE)) %>%
    group_by(OP.B) %>% summarise(avg.INV=mean(avg.INV)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.INV"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.18"},{"1":"2","2":"0.15"},{"1":"3","2":"0.18"},{"1":"4","2":"0.17"},{"1":"5","2":"0.16"},{"1":"6","2":"0.16"},{"1":"7","2":"0.16"},{"1":"8","2":"0.19"},{"1":"9","2":"0.21"},{"1":"10","2":"0.28"},{"1":"NA","2":"0.26"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $OP.2$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(OP.OK, !is.na(Size))
```

### $OP.2$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, OP.2)

breakpoints = nyse %>% arrange(Period, OP.2) %>% group_by(Period) %>% summarize(
    D10=OP.2[.1*n()], D20=OP.2[.2*n()],
    D30=OP.2[.3*n()], D40=OP.2[.4*n()],
    D50=OP.2[.5*n()], D60=OP.2[.6*n()],
    D70=OP.2[.7*n()], D80=OP.2[.8*n()],
    D90=OP.2[.9*n()], D100=OP.2[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/OP_RnD_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "OP.2")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "OP.2", quantiles)
```


```r
df %>% group_by(OP.2.B, Date) %>% summarize(N=n()) %>% group_by(OP.2.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.2.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"845.93458"},{"1":"2","2":"399.42679"},{"1":"3","2":"323.89252"},{"1":"4","2":"329.38162"},{"1":"5","2":"327.38162"},{"1":"6","2":"321.81620"},{"1":"7","2":"309.15265"},{"1":"8","2":"302.54517"},{"1":"9","2":"312.36760"},{"1":"10","2":"343.50467"},{"1":"NA","2":"14.91756"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(OP.2.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ OP.2.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/OP_RnD_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.56 0.77 0.83 0.83 0.93 0.93 0.92 0.95 0.99 0.99 1.56
```

## $GP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(GP), !is.na(Size))
```

### $GP$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, GP)

breakpoints = nyse %>% arrange(Period, GP) %>% group_by(Period) %>% summarize(
    D10=GP[.1*n()], D20=GP[.2*n()],
    D30=GP[.3*n()], D40=GP[.4*n()],
    D50=GP[.5*n()], D60=GP[.6*n()],
    D70=GP[.7*n()], D80=GP[.8*n()],
    D90=GP[.9*n()], D100=GP[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/GP_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "GP")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "GP", quantiles)
```


```r
df %>% group_by(GP.B, Date) %>% summarize(N=n()) %>% group_by(GP.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["GP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"677.44393"},{"1":"2","2":"295.55919"},{"1":"3","2":"273.59657"},{"1":"4","2":"304.75389"},{"1":"5","2":"331.67757"},{"1":"6","2":"336.87695"},{"1":"7","2":"375.40810"},{"1":"8","2":"397.84112"},{"1":"9","2":"443.85202"},{"1":"10","2":"490.00156"},{"1":"NA","2":"17.83162"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(GP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ GP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/GP_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.80 0.82 0.84 0.81 0.89 0.96 0.80 1.00 0.96 1.07 1.31
```

### $BM$


```r
df %>% group_by(GP.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(GP.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["GP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.96"},{"1":"2","2":"1.02"},{"1":"3","2":"1.05"},{"1":"4","2":"1.02"},{"1":"5","2":"0.95"},{"1":"6","2":"0.90"},{"1":"7","2":"0.82"},{"1":"8","2":"0.76"},{"1":"9","2":"0.71"},{"1":"10","2":"0.64"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
x= df %>% filter(Month==7) %>%
    dcast(Date ~ GP.B, fun.aggregate=mean, value.var="BM", na.rm=TRUE)
x$diff = x$`1` - x$`10`
mean(x$diff, na.rm=TRUE)
```

```
## [1] 0.3128495
```

```r
se(x$diff)
```

```
## [1] 0.02681206
```

### Monthly $BM$


```r
df %>% group_by(GP.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(GP.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["GP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.01"},{"1":"2","2":"1.11"},{"1":"3","2":"1.12"},{"1":"4","2":"1.07"},{"1":"5","2":"0.96"},{"1":"6","2":"0.91"},{"1":"7","2":"0.82"},{"1":"8","2":"0.76"},{"1":"9","2":"0.68"},{"1":"10","2":"0.63"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $CP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(OP.OK, !is.na(Size))
```

### $CP$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, CP)

breakpoints = nyse %>% arrange(Period, CP) %>% group_by(Period) %>% summarize(
    D10=CP[.1*n()], D20=CP[.2*n()],
    D30=CP[.3*n()], D40=CP[.4*n()],
    D50=CP[.5*n()], D60=CP[.6*n()],
    D70=CP[.7*n()], D80=CP[.8*n()],
    D90=CP[.9*n()], D100=CP[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/CP_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "CP")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "CP", quantiles)
```


```r
df %>% group_by(CP.B, Date) %>% summarize(N=n()) %>% group_by(CP.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["CP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"813.30374"},{"1":"2","2":"447.93146"},{"1":"3","2":"340.13551"},{"1":"4","2":"299.06075"},{"1":"5","2":"298.37383"},{"1":"6","2":"301.14019"},{"1":"7","2":"301.60748"},{"1":"8","2":"307.32710"},{"1":"9","2":"322.66822"},{"1":"10","2":"384.04050"},{"1":"NA","2":"15.27933"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(CP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ CP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/CP_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.69 0.70 0.79 0.83 0.90 0.87 0.90 0.95 1.01 1.07 1.42
```

## $INV$

* Annual rebalance

### Assets


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(INV), !is.na(Size))
```

### INV Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, INV)

breakpoints = nyse %>% arrange(Period, INV) %>% group_by(Period) %>% summarize(
    D10=INV[.1*n()], D20=INV[.2*n()],
    D30=INV[.3*n()], D40=INV[.4*n()],
    D50=INV[.5*n()], D60=INV[.6*n()],
    D70=INV[.7*n()], D80=INV[.8*n()],
    D90=INV[.9*n()], D100=INV[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/INV_Assets_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "INV")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "INV", quantiles)
```


```r
df %>% group_by(INV.B, Date) %>% summarize(N=n()) %>% group_by(INV.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"579.55452"},{"1":"2","2":"337.81931"},{"1":"3","2":"288.20249"},{"1":"4","2":"280.82243"},{"1":"5","2":"274.88162"},{"1":"6","2":"289.75389"},{"1":"7","2":"297.07788"},{"1":"8","2":"326.87072"},{"1":"9","2":"378.65265"},{"1":"10","2":"538.16511"},{"1":"NA","2":"21.98544"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(INV.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ INV.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/INV_Assets_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##     1     2     3     4     5     6     7     8     9    10    NA 
##  1.11  1.11  1.08  0.96  0.97  0.89  0.91  0.82  0.88  0.78 -0.44
```

### Assets


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(INV.ppe), !is.na(Size))
```

### INV Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, INV.ppe)

breakpoints = nyse %>% arrange(Period, INV.ppe) %>% group_by(Period) %>% summarize(
    D10=INV.ppe[.1*n()], D20=INV.ppe[.2*n()],
    D30=INV.ppe[.3*n()], D40=INV.ppe[.4*n()],
    D50=INV.ppe[.5*n()], D60=INV.ppe[.6*n()],
    D70=INV.ppe[.7*n()], D80=INV.ppe[.8*n()],
    D90=INV.ppe[.9*n()], D100=INV.ppe[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/INV_PPE_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "INV.ppe")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "INV.ppe", quantiles)
```


```r
df %>% group_by(INV.ppe.B, Date) %>% summarize(N=n()) %>% group_by(INV.ppe.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.ppe.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"387.94393"},{"1":"2","2":"300.78349"},{"1":"3","2":"271.30997"},{"1":"4","2":"263.94704"},{"1":"5","2":"263.94237"},{"1":"6","2":"276.01402"},{"1":"7","2":"296.19626"},{"1":"8","2":"334.36760"},{"1":"9","2":"403.87850"},{"1":"10","2":"629.35202"},{"1":"NA","2":"27.09622"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(INV.ppe.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ INV.ppe.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/INV_PPE_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##     1     2     3     4     5     6     7     8     9    10    NA 
##  1.05  0.97  0.93  0.92  0.94  0.95  0.89  0.97  0.79  0.81 -0.17
```


```r
df = combine.sources(crsp, comp)
#df$INV = df$INV
df = df %>% filter(!is.na(INV), !is.na(Size))
```

### INV Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, INV)

breakpoints = nyse %>% arrange(Period, INV) %>% group_by(Period) %>% summarize(
    D10=INV[.1*n()], D20=INV[.2*n()],
    D30=INV[.3*n()], D40=INV[.4*n()],
    D50=INV[.5*n()], D60=INV[.6*n()],
    D70=INV[.7*n()], D80=INV[.8*n()],
    D90=INV[.9*n()], D100=INV[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/INV_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "INV")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "INV", quantiles)
```


```r
df %>% group_by(INV.B, Date) %>% summarize(N=n()) %>% group_by(INV.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"579.55452"},{"1":"2","2":"337.81931"},{"1":"3","2":"288.20249"},{"1":"4","2":"280.82243"},{"1":"5","2":"274.88162"},{"1":"6","2":"289.75389"},{"1":"7","2":"297.07788"},{"1":"8","2":"326.87072"},{"1":"9","2":"378.65265"},{"1":"10","2":"538.16511"},{"1":"NA","2":"21.98544"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(INV.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ INV.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/INV_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##     1     2     3     4     5     6     7     8     9    10    NA 
##  1.11  1.11  1.08  0.96  0.97  0.89  0.91  0.82  0.88  0.78 -0.44
```

![French Website $INV$ Decile Returns](C:/Data/FrenchDartmouth/10_INV.JPG)

### $ME$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"528.31"},{"1":"2","2":"1229.19"},{"1":"3","2":"1824.21"},{"1":"4","2":"1876.61"},{"1":"5","2":"2145.13"},{"1":"6","2":"2345.08"},{"1":"7","2":"2267.46"},{"1":"8","2":"2172.78"},{"1":"9","2":"1826.48"},{"1":"10","2":"1379.89"},{"1":"NA","2":"405.31"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"506.82"},{"1":"2","2":"1186.17"},{"1":"3","2":"1763.79"},{"1":"4","2":"1822.95"},{"1":"5","2":"2039.37"},{"1":"6","2":"2268.08"},{"1":"7","2":"2201.62"},{"1":"8","2":"2113.91"},{"1":"9","2":"1740.55"},{"1":"10","2":"1336.36"},{"1":"NA","2":"413.08"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $BM$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.94"},{"1":"2","2":"1.14"},{"1":"3","2":"1.09"},{"1":"4","2":"1.00"},{"1":"5","2":"0.95"},{"1":"6","2":"0.89"},{"1":"7","2":"0.85"},{"1":"8","2":"0.78"},{"1":"9","2":"0.71"},{"1":"10","2":"0.60"},{"1":"NA","2":"0.59"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Monthly $BM$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.95"},{"1":"2","2":"1.18"},{"1":"3","2":"1.10"},{"1":"4","2":"0.99"},{"1":"5","2":"0.94"},{"1":"6","2":"0.88"},{"1":"7","2":"0.86"},{"1":"8","2":"0.78"},{"1":"9","2":"0.74"},{"1":"10","2":"0.69"},{"1":"NA","2":"0.75"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $OP$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.OP=mean(OP, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.OP=mean(avg.OP)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.OP"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"NaN"},{"1":"2","2":"0.20"},{"1":"3","2":"0.12"},{"1":"4","2":"0.14"},{"1":"5","2":"0.24"},{"1":"6","2":"0.23"},{"1":"7","2":"0.22"},{"1":"8","2":"0.24"},{"1":"9","2":"0.32"},{"1":"10","2":"0.02"},{"1":"NA","2":"0.22"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $GP$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.GP=mean(GP, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.GP=mean(avg.GP)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.GP"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.28"},{"1":"2","2":"0.33"},{"1":"3","2":"0.33"},{"1":"4","2":"0.32"},{"1":"5","2":"0.34"},{"1":"6","2":"0.34"},{"1":"7","2":"0.36"},{"1":"8","2":"0.37"},{"1":"9","2":"0.38"},{"1":"10","2":"0.33"},{"1":"NA","2":"0.17"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $CP$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.CP=mean(CP, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.CP=mean(avg.CP)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.CP"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"NaN"},{"1":"2","2":"0.31"},{"1":"3","2":"0.22"},{"1":"4","2":"0.15"},{"1":"5","2":"0.23"},{"1":"6","2":"0.24"},{"1":"7","2":"0.22"},{"1":"8","2":"0.24"},{"1":"9","2":"0.31"},{"1":"10","2":"-0.02"},{"1":"NA","2":"-0.47"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Acc$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.Acc=mean(Acc, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.Acc=mean(avg.Acc)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Acc"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"-0.17"},{"1":"2","2":"-0.05"},{"1":"3","2":"-0.03"},{"1":"4","2":"0.00"},{"1":"5","2":"0.06"},{"1":"6","2":"0.04"},{"1":"7","2":"0.06"},{"1":"8","2":"0.07"},{"1":"9","2":"0.17"},{"1":"10","2":"0.22"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.17"},{"1":"2","2":"0.15"},{"1":"3","2":"0.14"},{"1":"4","2":"0.14"},{"1":"5","2":"0.13"},{"1":"6","2":"0.14"},{"1":"7","2":"0.14"},{"1":"8","2":"0.14"},{"1":"9","2":"0.14"},{"1":"10","2":"0.11"},{"1":"NA","2":"0.04"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $Prior$

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Prior), !is.na(L13.price), !is.na(L1.ri), !is.na(L1.ME))
```

### $Prior$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, Prior)

breakpoints = nyse %>% arrange(Date, Prior) %>% group_by(Date) %>% summarize(
    D10=Prior[.1*n()], D20=Prior[.2*n()],
    D30=Prior[.3*n()], D40=Prior[.4*n()],
    D50=Prior[.5*n()], D60=Prior[.6*n()],
    D70=Prior[.7*n()], D80=Prior[.8*n()],
    D90=Prior[.9*n()], D100=Prior[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/Prior_Decile_Breakpoints.csv")

# breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "Prior")] = NULL

df = left_join(df, breakpoints, by=c("Date"))

df = assign.bkts(df, "Prior", quantiles)
```


```r
df %>% group_by(Prior.B, Date) %>% summarize(N=n()) %>% group_by(Prior.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"680.42813"},{"1":"2","2":"422.48165"},{"1":"3","2":"366.39450"},{"1":"4","2":"339.83486"},{"1":"5","2":"326.88379"},{"1":"6","2":"322.19113"},{"1":"7","2":"329.89144"},{"1":"8","2":"349.79664"},{"1":"9","2":"393.90673"},{"1":"10","2":"636.18196"},{"1":"NA","2":"23.77796"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Prior.B, Date) %>% mutate(
    bkt.L1.ME=sum(L1.ME, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$L1.ME / df$bkt.L1.ME
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ Prior.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/Prior_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 0.27 0.80 0.89 0.88 0.85 0.87 0.99 1.10 1.17 1.52 1.10
```

### $ME$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"427.69"},{"1":"2","2":"1226.94"},{"1":"3","2":"1647.59"},{"1":"4","2":"1984.27"},{"1":"5","2":"2210.60"},{"1":"6","2":"2285.69"},{"1":"7","2":"2262.65"},{"1":"8","2":"2149.39"},{"1":"9","2":"1797.02"},{"1":"10","2":"954.14"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"530.41"},{"1":"2","2":"1320.21"},{"1":"3","2":"1694.45"},{"1":"4","2":"1989.07"},{"1":"5","2":"2162.13"},{"1":"6","2":"2195.36"},{"1":"7","2":"2125.98"},{"1":"8","2":"1961.72"},{"1":"9","2":"1575.87"},{"1":"10","2":"757.73"},{"1":"NA","2":"120.55"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $BM$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.78"},{"1":"2","2":"0.86"},{"1":"3","2":"0.88"},{"1":"4","2":"0.87"},{"1":"5","2":"0.88"},{"1":"6","2":"0.90"},{"1":"7","2":"0.89"},{"1":"8","2":"0.90"},{"1":"9","2":"0.89"},{"1":"10","2":"0.89"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Monthly $BM$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.45"},{"1":"2","2":"1.06"},{"1":"3","2":"0.97"},{"1":"4","2":"0.91"},{"1":"5","2":"0.86"},{"1":"6","2":"0.82"},{"1":"7","2":"0.78"},{"1":"8","2":"0.73"},{"1":"9","2":"0.67"},{"1":"10","2":"0.52"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $Acc$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Acc), !is.na(Size))
```

### $Acc$ Decile Breakpoints


```r
quantiles = 1:10/10

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Acc)

breakpoints = nyse %>% arrange(Period, Acc) %>% group_by(Period) %>% summarize(
    D10=Acc[.1*n()], D20=Acc[.2*n()],
    D30=Acc[.3*n()], D40=Acc[.4*n()],
    D50=Acc[.5*n()], D60=Acc[.6*n()],
    D70=Acc[.7*n()], D80=Acc[.8*n()],
    D90=Acc[.9*n()], D100=Acc[n()]) %>% as.data.frame
write.csv(breakpoints, "C:/Data/Thesis/Acc_Decile_Breakpoints.csv")

breakpoints$Period = breakpoints$Period + 1

breakpoints[, c("PERMNO", "Acc")] = NULL

df = left_join(df, breakpoints, by=c("Period"))
```


```r
df = assign.bkts(df, "Acc", quantiles)
```


```r
df %>% group_by(Acc.B, Date) %>% summarize(N=n()) %>% group_by(Acc.B) %>%
    summarise(avg.N=mean(N))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Acc.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"482.09346"},{"1":"2","2":"303.23520"},{"1":"3","2":"251.31776"},{"1":"4","2":"227.50156"},{"1":"5","2":"214.10592"},{"1":"6","2":"233.16355"},{"1":"7","2":"252.49533"},{"1":"8","2":"280.93925"},{"1":"9","2":"336.46106"},{"1":"10","2":"496.31153"},{"1":"NA","2":"14.50267"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Acc.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
decile.returns = df %>%
    dcast(Date ~ Acc.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(decile.returns, "C:/Data/Thesis/Acc_Decile_Returns.csv")
round(colMeans(decile.returns %>% select(-Date)) * 100, 2)
```

```
##    1    2    3    4    5    6    7    8    9   10   NA 
## 1.13 0.98 0.95 0.96 0.88 0.88 0.89 0.91 0.98 0.75 1.08
```

### $ME$


```r
df %>% group_by(Acc.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(Acc.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Acc.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"795.50"},{"1":"2","2":"1321.09"},{"1":"3","2":"2193.63"},{"1":"4","2":"2556.02"},{"1":"5","2":"2734.75"},{"1":"6","2":"2590.20"},{"1":"7","2":"1843.49"},{"1":"8","2":"1762.73"},{"1":"9","2":"1341.05"},{"1":"10","2":"951.51"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $Vol$

* Daily data
* Residuals from three factor model

### Returns

# Multivariate Sorts

## $Size$ and $BM$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(BM), !is.na(Size), BE>0)
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, BM)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $BM$ Quintiles


```r
breakpoints = nyse %>% arrange(Period, BM) %>% group_by(Period) %>% summarize(
    D10=BM[.1*n()], D20=BM[.2*n()],
    D30=BM[.3*n()], D40=BM[.4*n()],
    D50=BM[.5*n()], D60=BM[.6*n()],
    D70=BM[.7*n()], D80=BM[.8*n()],
    D90=BM[.9*n()], D100=BM[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "BM")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "BM", quantiles)
```


```r
df %>% group_by(Size.B, BM.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, BM.B) %>%
    dcast(Size.B ~ BM.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"486","3":"324","4":"317","5":"380","6":"662","7":"4"},{"1":"2","2":"162","3":"120","4":"113","5":"98","6":"91","7":"1"},{"1":"3","2":"124","3":"93","4":"79","5":"66","6":"51","7":"1"},{"1":"4","2":"109","3":"80","4":"60","5":"52","6":"37","7":"1"},{"1":"5","2":"118","3":"70","4":"51","5":"41","6":"29","7":"NaN"},{"1":"NA","2":"1","3":"1","4":"1","5":"1","6":"1","7":"516"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, BM.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.BM.B = interaction(df$Size.B, df$BM.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.BM.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns, "C:/Data/Thesis/25_Size_BM_Returns.csv")

df %>% group_by(Size.B, BM.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ BM.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.66","3":"1.19","4":"1.26","5":"1.45","6":"1.48"},{"1":"2","2":"0.91","3":"1.20","4":"1.20","5":"1.39","6":"1.21"},{"1":"3","2":"0.87","3":"1.09","4":"1.19","5":"1.21","6":"1.32"},{"1":"4","2":"0.98","3":"1.00","4":"1.12","5":"1.12","6":"1.18"},{"1":"5","2":"0.87","3":"0.88","4":"0.96","5":"0.94","6":"0.91"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

![French Website 25 $Size-BM$ Returns](C:/Data/FrenchDartmouth/25_Size_BM.JPG)

## $Size$ and $BM_M$

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(L1.ME), !is.na(BMM), BE>0)
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, L1.ME, BMM)

breakpoints = nyse %>% arrange(Date, L1.ME) %>% group_by(Date) %>% summarize(
    D10=L1.ME[.1*n()], D20=L1.ME[.2*n()],
    D30=L1.ME[.3*n()], D40=L1.ME[.4*n()],
    D50=L1.ME[.5*n()], D60=L1.ME[.6*n()],
    D70=L1.ME[.7*n()], D80=L1.ME[.8*n()],
    D90=L1.ME[.9*n()], D100=L1.ME[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "L1.ME")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "L1.ME", quantiles)
```

### $BM_m$ Quintiles


```r
breakpoints = nyse %>% arrange(Date, BMM) %>% group_by(Date) %>% summarize(
    D10=BMM[.1*n()], D20=BMM[.2*n()],
    D30=BMM[.3*n()], D40=BMM[.4*n()],
    D50=BMM[.5*n()], D60=BMM[.6*n()],
    D70=BMM[.7*n()], D80=BMM[.8*n()],
    D90=BMM[.9*n()], D100=BMM[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "BMM")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "BMM", quantiles)
```


```r
df$L1.ME.B = factor(df$L1.ME.B, labels=c("ME1", "ME2", "ME3", "ME4", "ME5"))
df$BMM.B = factor(df$BMM.B, labels=c("BMM1", "BMM2", "BMM3", "BMM4", "BMM5"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(L1.ME.B, BMM.B, Date) %>% mutate(
    bkt.L1.ME=sum(L1.ME, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$L1.ME / df$bkt.L1.ME
```

### Returns


```r
df$L1.ME.BMM.B = interaction(df$L1.ME.B, df$BMM.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ L1.ME.BMM.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
colnames(returns)
```

```
##  [1] "Date"     "ME1.BMM1" "ME2.BMM1" "ME3.BMM1" "ME4.BMM1" "ME5.BMM1"
##  [7] "ME1.BMM2" "ME2.BMM2" "ME3.BMM2" "ME4.BMM2" "ME5.BMM2" "ME1.BMM3"
## [13] "ME2.BMM3" "ME3.BMM3" "ME4.BMM3" "ME5.BMM3" "ME1.BMM4" "ME2.BMM4"
## [19] "ME3.BMM4" "ME4.BMM4" "ME5.BMM4" "ME1.BMM5" "ME2.BMM5" "ME3.BMM5"
## [25] "ME4.BMM5" "ME5.BMM5" "NA"
```

```r
write.csv(returns %>% select(-`NA`), "C:/Data/Thesis/25_L1ME_BMM_Returns.csv")
```

## $Size$ and $OP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), OP.OK)
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, OP)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $OP$ Quintiles


```r
breakpoints = nyse %>% arrange(Period, OP) %>% group_by(Period) %>% summarize(
    D10=OP[.1*n()], D20=OP[.2*n()],
    D30=OP[.3*n()], D40=OP[.4*n()],
    D50=OP[.5*n()], D60=OP[.6*n()],
    D70=OP[.7*n()], D80=OP[.8*n()],
    D90=OP[.9*n()], D100=OP[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "OP")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "OP", quantiles)
```


```r
df %>% group_by(Size.B, OP.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, OP.B) %>%
    dcast(Size.B ~ OP.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1053","3":"338","4":"297","5":"236","6":"254","7":"3"},{"1":"2","2":"149","3":"110","4":"113","5":"106","6":"101","7":"1"},{"1":"3","2":"78","3":"75","4":"86","5":"87","6":"85","7":"1"},{"1":"4","2":"51","3":"61","4":"69","5":"77","6":"78","7":"1"},{"1":"5","2":"36","3":"51","4":"57","5":"75","6":"90","7":"1"},{"1":"NA","2":"NaN","3":"1","4":"1","5":"1","6":"1","7":"516"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, OP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.OP.B = interaction(df$Size.B, df$OP.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.OP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns, "C:/Data/Thesis/25_Size_OP_Returns.csv")

df %>% group_by(Size.B, OP.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ OP.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.95","3":"1.32","4":"1.33","5":"1.26","6":"1.32"},{"1":"2","2":"0.91","3":"1.20","4":"1.20","5":"1.23","6":"1.33"},{"1":"3","2":"0.94","3":"1.16","4":"1.10","5":"1.10","6":"1.26"},{"1":"4","2":"0.90","3":"1.00","4":"1.09","5":"1.12","6":"1.18"},{"1":"5","2":"0.75","3":"0.80","4":"0.85","5":"0.92","6":"0.94"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

![French Website 25 $Size-OP$ Returns](C:/Data/FrenchDartmouth/25_Size_OP.JPG)

## $Size$ and $GP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), !is.na(GP))
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, GP)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $GP$ Quintiles


```r
breakpoints = nyse %>% arrange(Period, GP) %>% group_by(Period) %>% summarize(
    D10=GP[.1*n()], D20=GP[.2*n()],
    D30=GP[.3*n()], D40=GP[.4*n()],
    D50=GP[.5*n()], D60=GP[.6*n()],
    D70=GP[.7*n()], D80=GP[.8*n()],
    D90=GP[.9*n()], D100=GP[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "GP")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "GP", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("ME1", "ME2", "ME3", "ME4", "ME5"))
df$GP.B = factor(df$GP.B, labels=c("GP1", "GP2", "GP3", "GP4", "GP5"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, GP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.GP.B = interaction(df$Size.B, df$GP.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.GP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns %>% select(-`NA`), "C:/Data/Thesis/25_Size_GP_Returns.csv")
```

## $Size$ and $CP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), OP.OK)
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, CP)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $CP$ Quintiles


```r
breakpoints = nyse %>% arrange(Period, CP) %>% group_by(Period) %>% summarize(
    D10=CP[.1*n()], D20=CP[.2*n()],
    D30=CP[.3*n()], D40=CP[.4*n()],
    D50=CP[.5*n()], D60=CP[.6*n()],
    D70=CP[.7*n()], D80=CP[.8*n()],
    D90=CP[.9*n()], D100=CP[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "CP")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "CP", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("ME1", "ME2", "ME3", "ME4", "ME5"))
df$CP.B = factor(df$CP.B, labels=c("CP1", "CP2", "CP3", "CP4", "CP5"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, CP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.CP.B = interaction(df$Size.B, df$CP.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.CP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns %>% select(-`NA`), "C:/Data/Thesis/25_Size_CP_Returns.csv")
```

## $Size$ and $INV$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), !is.na(INV))
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, INV)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $INV$ Quintiles


```r
breakpoints = nyse %>% arrange(Period, INV) %>% group_by(Period) %>% summarize(
    D10=INV[.1*n()], D20=INV[.2*n()],
    D30=INV[.3*n()], D40=INV[.4*n()],
    D50=INV[.5*n()], D60=INV[.6*n()],
    D70=INV[.7*n()], D80=INV[.8*n()],
    D90=INV[.9*n()], D100=INV[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "INV")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "INV", quantiles)
```


```r
df %>% group_by(Size.B, INV.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, INV.B) %>%
    dcast(Size.B ~ INV.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"674","3":"296","4":"263","5":"283","6":"470","7":"8"},{"1":"2","2":"104","3":"89","4":"91","5":"108","6":"172","7":"3"},{"1":"3","2":"59","3":"65","4":"74","5":"84","6":"120","7":"2"},{"1":"4","2":"45","3":"60","4":"66","5":"74","6":"87","7":"2"},{"1":"5","2":"35","3":"59","4":"70","5":"74","6":"68","7":"2"},{"1":"NA","2":"1","3":"1","4":"1","5":"1","6":"1","7":"541"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, INV.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.INV.B = interaction(df$Size.B, df$INV.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.INV.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns, "C:/Data/Thesis/25_Size_INV_Returns.csv")

df %>% group_by(Size.B, INV.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ INV.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.31","3":"1.38","4":"1.39","5":"1.25","6":"0.83"},{"1":"2","2":"1.30","3":"1.35","4":"1.27","5":"1.30","6":"0.91"},{"1":"3","2":"1.25","3":"1.20","4":"1.29","5":"1.15","6":"0.91"},{"1":"4","2":"1.04","3":"1.21","4":"1.08","5":"1.13","6":"0.94"},{"1":"5","2":"1.07","3":"0.96","4":"0.91","5":"0.81","6":"0.83"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

![French Website 25 $Size-INV$ Returns](C:/Data/FrenchDartmouth/25_Size_INV.JPG)

## $Size$ and $Prior$

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(L1.ME), !is.na(Prior), !is.na(L13.price), !is.na(L1.ri))
```

### $Size$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, L1.ME, Prior)

breakpoints = nyse %>% arrange(Date, L1.ME) %>% group_by(Date) %>% summarize(
    D10=L1.ME[.1*n()], D20=L1.ME[.2*n()],
    D30=L1.ME[.3*n()], D40=L1.ME[.4*n()],
    D50=L1.ME[.5*n()], D60=L1.ME[.6*n()],
    D70=L1.ME[.7*n()], D80=L1.ME[.8*n()],
    D90=L1.ME[.9*n()], D100=L1.ME[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "L1.ME")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "L1.ME", quantiles)
```

### $Prior$ Quintiles


```r
breakpoints = nyse %>% arrange(Date, Prior) %>% group_by(Date) %>% summarize(
    D10=Prior[.1*n()], D20=Prior[.2*n()],
    D30=Prior[.3*n()], D40=Prior[.4*n()],
    D50=Prior[.5*n()], D60=Prior[.6*n()],
    D70=Prior[.7*n()], D80=Prior[.8*n()],
    D90=Prior[.9*n()], D100=Prior[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Prior")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "Prior", quantiles)
```


```r
df %>% group_by(L1.ME.B, Prior.B, Date) %>% summarize(N=n()) %>% group_by(L1.ME.B, Prior.B) %>%
    dcast(L1.ME.B ~ Prior.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"828","3":"385","4":"314","5":"328","6":"602","7":"20"},{"1":"2","2":"120","3":"110","4":"106","5":"114","6":"176","7":"3"},{"1":"3","2":"67","3":"80","4":"83","5":"87","6":"114","7":"2"},{"1":"4","2":"49","3":"68","4":"74","5":"77","6":"80","7":"2"},{"1":"5","2":"38","3":"63","4":"73","5":"73","6":"59","7":"3"},{"1":"NA","2":"1","3":"1","4":"1","5":"1","6":"1","7":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(L1.ME.B, Prior.B, Date) %>% mutate(
    bkt.L1.ME=sum(L1.ME, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$L1.ME / df$bkt.L1.ME
```

### Returns


```r
df$L1.ME.Prior.B = interaction(df$L1.ME.B, df$Prior.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ L1.ME.Prior.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns, "C:/Data/Thesis/25_L1ME_Prior_Returns.csv")

df %>% group_by(L1.ME.B, Prior.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(L1.ME.B ~ Prior.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(L1.ME.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.48","3":"1.04","4":"1.27","5":"1.42","6":"1.65"},{"1":"2","2":"0.63","3":"1.03","4":"1.25","5":"1.35","6":"1.59"},{"1":"3","2":"0.73","3":"1.02","4":"1.11","5":"1.15","6":"1.52"},{"1":"4","2":"0.73","3":"0.97","4":"1.06","5":"1.18","6":"1.39"},{"1":"5","2":"0.68","3":"0.88","4":"0.78","5":"1.00","6":"1.21"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

![French Website 25 $ME-Prior$ Returns](C:/Data/FrenchDartmouth/25_L1ME_Prior.JPG)

## Value and Momentum

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(L1.ME), !is.na(Prior), !is.na(L13.price), !is.na(L1.ri),
                   !is.infinite(CP))
```

### Value Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, BMM, Prior)

breakpoints = nyse %>% arrange(Date, BMM) %>% group_by(Date) %>% summarize(
    D10=BMM[.1*n()], D20=BMM[.2*n()],
    D30=BMM[.3*n()], D40=BMM[.4*n()],
    D50=BMM[.5*n()], D60=BMM[.6*n()],
    D70=BMM[.7*n()], D80=BMM[.8*n()],
    D90=BMM[.9*n()], D100=BMM[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "BMM")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "BMM", quantiles)
```

### $Prior$ Quintiles


```r
breakpoints = nyse %>% arrange(Date, Prior) %>% group_by(Date) %>% summarize(
    D10=Prior[.1*n()], D20=Prior[.2*n()],
    D30=Prior[.3*n()], D40=Prior[.4*n()],
    D50=Prior[.5*n()], D60=Prior[.6*n()],
    D70=Prior[.7*n()], D80=Prior[.8*n()],
    D90=Prior[.9*n()], D100=Prior[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Prior")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "Prior", quantiles)
```

### ME


```r
df %>% group_by(BMM.B, Prior.B, Date) %>% summarise(Avg.ME=mean(ME, na.rm=TRUE)) %>%
    dcast(BMM.B ~ Prior.B, fun.aggregate=mean, value.var="Avg.ME", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(BMM.B)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1436.27","3":"3466.80","4":"4146.99","5":"3858.76","6":"1880.67"},{"1":"2","2":"1336.60","3":"2892.78","4":"3264.48","5":"2848.84","6":"1263.80"},{"1":"3","2":"900.91","3":"1909.73","4":"2179.94","5":"1986.00","6":"982.28"},{"1":"4","2":"705.60","3":"1325.19","4":"1550.62","5":"1399.65","6":"766.84"},{"1":"5","2":"1260.40","3":"1830.79","4":"4722.79","5":"5712.31","6":"3763.93"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Investment


```r
df %>% group_by(BMM.B, Prior.B, Date) %>% summarise(Avg.INV=mean(INV, na.rm=TRUE)) %>%
    dcast(BMM.B ~ Prior.B, fun.aggregate=mean, value.var="Avg.INV", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(BMM.B)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.30","3":"0.26","4":"0.25","5":"0.25","6":"0.26"},{"1":"2","2":"0.32","3":"0.23","4":"0.18","5":"0.17","6":"0.16"},{"1":"3","2":"0.25","3":"0.17","4":"0.13","5":"0.12","6":"0.10"},{"1":"4","2":"0.22","3":"0.13","4":"0.11","5":"0.09","6":"0.08"},{"1":"5","2":"0.12","3":"0.07","4":"0.09","5":"0.04","6":"0.02"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### CP


```r
df %>% group_by(BMM.B, Prior.B, Date) %>% summarise(Avg.CP=mean(CP, na.rm=TRUE)) %>%
    dcast(BMM.B ~ Prior.B, fun.aggregate=mean, value.var="Avg.CP", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(BMM.B)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.21","3":"0.41","4":"0.13","5":"-0.12","6":"0.65"},{"1":"2","2":"0.20","3":"0.28","4":"0.30","5":"0.28","6":"0.23"},{"1":"3","2":"0.20","3":"0.25","4":"0.24","5":"0.23","6":"0.21"},{"1":"4","2":"0.20","3":"0.21","4":"0.20","5":"0.20","6":"0.17"},{"1":"5","2":"0.10","3":"0.22","4":"0.21","5":"0.14","6":"0.18"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Accruals


```r
df %>% group_by(BMM.B, Prior.B, Date) %>% summarise(Avg.Acc=mean(Acc, na.rm=TRUE)) %>%
    dcast(BMM.B ~ Prior.B, fun.aggregate=mean, value.var="Avg.Acc", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(BMM.B)) %>% round(4)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.1552","3":"0.1101","4":"0.2349","5":"0.0908","6":"0.0861"},{"1":"2","2":"0.0333","3":"0.0424","4":"0.0328","5":"0.0318","6":"0.0162"},{"1":"3","2":"0.0475","3":"0.0329","4":"0.0247","5":"0.0196","6":"0.0015"},{"1":"4","2":"0.0263","3":"0.0142","4":"0.0080","5":"0.0004","6":"-0.0178"},{"1":"5","2":"-0.0014","3":"0.0158","4":"0.0178","5":"0.0009","6":"0.0109"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $Acc$ and $INV$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Acc), !is.na(INV))
```

### $Acc$ Quintiles


```r
quantiles = 1:5/5

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Acc, INV)

breakpoints = nyse %>% arrange(Period, Acc) %>% group_by(Period) %>% summarize(
    D10=Acc[.1*n()], D20=Acc[.2*n()],
    D30=Acc[.3*n()], D40=Acc[.4*n()],
    D50=Acc[.5*n()], D60=Acc[.6*n()],
    D70=Acc[.7*n()], D80=Acc[.8*n()],
    D90=Acc[.9*n()], D100=Acc[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Acc")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Acc", quantiles)
```

### $INV$ Quintiles


```r
breakpoints = nyse %>% arrange(Period, INV) %>% group_by(Period) %>% summarize(
    D10=INV[.1*n()], D20=INV[.2*n()],
    D30=INV[.3*n()], D40=INV[.4*n()],
    D50=INV[.5*n()], D60=INV[.6*n()],
    D70=INV[.7*n()], D80=INV[.8*n()],
    D90=INV[.9*n()], D100=INV[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "INV")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "INV", quantiles)
```


```r
df %>% group_by(Acc.B, INV.B, Date) %>% summarize(N=n()) %>% group_by(Acc.B, INV.B) %>%
    dcast(Acc.B ~ INV.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Acc.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"417","3":"107","4":"74","5":"72","6":"119","7":"4"},{"1":"2","2":"124","3":"111","4":"88","5":"77","6":"79","7":"3"},{"1":"3","2":"80","3":"100","4":"99","5":"89","6":"79","7":"3"},{"1":"4","2":"76","3":"84","4":"108","5":"131","6":"135","7":"3"},{"1":"5","2":"129","3":"66","4":"82","5":"146","6":"408","7":"9"},{"1":"NA","2":"4","3":"1","4":"2","5":"2","6":"2","7":"48"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Acc.B, INV.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Acc.INV.B = interaction(df$Acc.B, df$INV.B)
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Acc.INV.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)
write.csv(returns, "C:/Data/Thesis/25_Acc_INV_Returns.csv")

df %>% group_by(Acc.B, INV.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Acc.B ~ INV.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Acc.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Acc.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.30","3":"1.00","4":"1.04","5":"1.06","6":"0.91"},{"1":"2","2":"1.08","3":"1.02","4":"0.86","5":"0.95","6":"1.07"},{"1":"3","2":"1.03","3":"1.08","4":"0.94","5":"0.89","6":"0.78"},{"1":"4","2":"0.88","3":"1.13","4":"0.93","5":"0.89","6":"0.76"},{"1":"5","2":"1.13","3":"1.22","4":"1.26","5":"0.97","6":"0.86"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Create Factors

## Market Excess Return, $R_M$


```r
rm = crsp %>% select(Date, rmkt) %>% filter(!duplicated(c(Date)))

rm$rmkt = rm$rmkt * 100
summary(rm)
```

```
##       Date                 rmkt         
##  Min.   :1960-01-29   Min.   :-22.5363  
##  1st Qu.:1974-04-22   1st Qu.: -1.7139  
##  Median :1988-07-14   Median :  1.2109  
##  Mean   :1988-07-15   Mean   :  0.8754  
##  3rd Qu.:2002-10-07   3rd Qu.:  3.7938  
##  Max.   :2016-12-30   Max.   : 16.5585
```


```r
rf = fread("C:/Data/CRSP/RF_196307_201612.csv")
```


```r
rf$Date = as.Date(as.character(rf$caldt), "%Y%m%d")
rf$caldt = NULL
rf$rf = rf$t30ret * 100
rf$t30ret = NULL
summary(rf)
```

```
##       Date                  rf         
##  Min.   :1963-07-31   Min.   :-0.0037  
##  1st Qu.:1976-12-07   1st Qu.: 0.2360  
##  Median :1990-04-14   Median : 0.3924  
##  Mean   :1990-04-15   Mean   : 0.3920  
##  3rd Qu.:2003-08-21   3rd Qu.: 0.5245  
##  Max.   :2016-12-30   Max.   : 1.5158
```


```r
rm = left_join(rm, rf, by=c("Date"))
```


```r
rm$rf[is.na(rm$rf)] = mean(rm$rf, na.rm=TRUE)
rm$rmkt[is.na(rm$rmkt)] = mean(rm$rmkt, na.rm=TRUE)
rm$Rm = rm$rmkt - rm$rf
rm = rm %>% filter(Date>as.Date("1963-06-30"))
summary(rm)
```

```
##       Date                 rmkt                rf         
##  Min.   :1963-07-31   Min.   :-22.5363   Min.   :-0.0037  
##  1st Qu.:1976-12-07   1st Qu.: -1.6861   1st Qu.: 0.2360  
##  Median :1990-04-14   Median :  1.1947   Median : 0.3921  
##  Mean   :1990-04-15   Mean   :  0.8877   Mean   : 0.3919  
##  3rd Qu.:2003-08-21   3rd Qu.:  3.8163   3rd Qu.: 0.5245  
##  Max.   :2016-12-30   Max.   : 16.5585   Max.   : 1.5158  
##        Rm          
##  Min.   :-23.1403  
##  1st Qu.: -2.1159  
##  Median :  0.8472  
##  Mean   :  0.4959  
##  3rd Qu.:  3.4806  
##  Max.   : 16.0331
```

```r
rbind(sapply(rm[,-1], mean), sapply(rm[,-1], se), sapply(rm[,-1], t.stat)) %>% round(2)
```

```
##      rmkt    rf   Rm
## [1,] 0.89  0.39 0.50
## [2,] 0.17  0.01 0.17
## [3,] 5.11 37.13 2.84
```

```r
rm$rmkt = NULL
rm$rf = NULL
```

## $Size$ and $BM$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(BM), !is.na(Size), BE>0)
```


```r
df %>% select(Date, PERMNO, Size, BM) %>% arrange(Date) %>% head
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Date"],"name":[1],"type":["date"],"align":["right"]},{"label":["PERMNO"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Size"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["BM"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1962-07-31","2":"10006","3":"84.1930","4":"1.2828831","_rn_":"1"},{"1":"1962-07-31","2":"10014","3":"11.2770","4":"0.8882029","_rn_":"2"},{"1":"1962-07-31","2":"10102","3":"217.9530","4":"0.5732686","_rn_":"3"},{"1":"1962-07-31","2":"10137","3":"393.0705","4":"0.4435748","_rn_":"4"},{"1":"1962-07-31","2":"10153","3":"137.6526","4":"1.7926768","_rn_":"5"},{"1":"1962-07-31","2":"10161","3":"417.7193","4":"0.4312827","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, BM)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $BM$ Quantiles


```r
quantiles = c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, BM) %>% group_by(Period) %>% summarize(
    D10=BM[.1*n()], D20=BM[.2*n()],
    D30=BM[.3*n()], D40=BM[.4*n()],
    D50=BM[.5*n()], D60=BM[.6*n()],
    D70=BM[.7*n()], D80=BM[.8*n()],
    D90=BM[.9*n()], D100=BM[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "BM")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "BM", quantiles)
```


```r
summary(df %>% select(Size.B, BM.B))
```

```
##      Size.B           BM.B      
##  Min.   :1.000   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.:1.000  
##  Median :1.000   Median :2.000  
##  Mean   :1.219   Mean   :1.959  
##  3rd Qu.:1.000   3rd Qu.:3.000  
##  Max.   :2.000   Max.   :3.000  
##  NA's   :6918    NA's   :7923
```

```r
typeof(df$Size.B)
```

```
## [1] "double"
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$BM.B = factor(df$BM.B, labels=c("L", "M", "H"))
```


```r
summary(df %>% select(Size.B, BM.B))
```

```
##   Size.B          BM.B       
##  S   :1912027   L   :868836  
##  B   : 537449   M   :811576  
##  NA's:   6918   H   :768059  
##                 NA's:  7923
```

```r
typeof(df$Size.B)
```

```
## [1] "integer"
```


```r
#df %>% group_by(Size.B, BM.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, BM.B) %>%
#    dcast(Size.B ~ BM.B, value.var="N", fun.aggregate=mean, na.rm=TRUE)
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, BM.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.BM.B = interaction(df$Size.B, df$BM.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.BM.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

#df %>% group_by(Size.B, BM.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
#    dcast(Size.B ~ BM.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
#    select(-`NA`) %>% filter(!is.na(Size.B))

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$HMLs = returns$SH - returns$SL
returns$HMLb = returns$BH - returns$BL
returns$HML = (returns$SH+returns$BH)/2 - (returns$SL+returns$BL)/2
returns$SMB.HML = ((returns$SL+returns$SM+returns$SH)/3 -
               (returns$BL+returns$BM+returns$BH)/3)
HML = returns %>% select(Date, HML, SMB.HML)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##        SL   BL   SM   BM   SH   BH HMLs HMLb  HML SMB.HML
## [1,] 0.88 0.88 1.27 0.94 1.37 1.04 0.49 0.16 0.32    0.22
## [2,] 0.27 0.18 0.21 0.17 0.21 0.18 0.13 0.12 0.11    0.12
## [3,] 3.32 4.81 6.04 5.56 6.48 5.77 3.76 1.34 2.89    1.88
## [4,] 0.13 0.19 0.24 0.22 0.26 0.23 0.15 0.05 0.11    0.07
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_BM_Returns.csv")
```

![French Website 6 $Size-BM$ Returns](C:/Data/FrenchDartmouth/6_Size_BM.JPG)



## $Size$ and $BM_M$

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(L1.ME), !is.na(BMM), BE>0)
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, L1.ME, BMM)

breakpoints = nyse %>% arrange(Date, L1.ME) %>% group_by(Date) %>% summarize(
    D10=L1.ME[.1*n()], D20=L1.ME[.2*n()],
    D30=L1.ME[.3*n()], D40=L1.ME[.4*n()],
    D50=L1.ME[.5*n()], D60=L1.ME[.6*n()],
    D70=L1.ME[.7*n()], D80=L1.ME[.8*n()],
    D90=L1.ME[.9*n()], D100=L1.ME[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "L1.ME")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "L1.ME", quantiles)
```

### $BM_M$ Quantiles


```r
quantiles = c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Date, BMM) %>% group_by(Date) %>% summarize(
    D10=BMM[.1*n()], D20=BMM[.2*n()],
    D30=BMM[.3*n()], D40=BMM[.4*n()],
    D50=BMM[.5*n()], D60=BMM[.6*n()],
    D70=BMM[.7*n()], D80=BMM[.8*n()],
    D90=BMM[.9*n()], D100=BMM[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "BMM")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "BMM", quantiles)
```


```r
df$L1.ME.B = factor(df$L1.ME.B, labels=c("S", "B"))
df$BMM.B = factor(df$BMM.B, labels=c("Lm", "Mm", "Hm"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(L1.ME.B, BMM.B, Date) %>% mutate(
    bkt.L1.ME=sum(L1.ME, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$L1.ME / df$bkt.L1.ME
```

### Returns


```r
df$L1.ME.BMM.B = interaction(df$L1.ME.B, df$BMM.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ L1.ME.BMM.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$HMLms = returns$SHm - returns$SLm
returns$HMLmb = returns$BHm - returns$BLm
returns$HMLm = (returns$SHm+returns$BHm)/2 - (returns$SLm+returns$BLm)/2
returns$SMB.HMLm = ((returns$SLm+returns$SMm+returns$SHm)/3 -
               (returns$BLm+returns$BMm+returns$BHm)/3)
HMLm = returns %>% select(Date, HMLm, SMB.HMLm)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##       SLm  BLm  SMm  BMm  SHm  BHm HMLms HMLmb HMLm SMB.HMLm
## [1,] 0.97 0.86 1.17 0.93 1.43 1.14  0.46  0.29 0.37     0.22
## [2,] 0.27 0.18 0.21 0.17 0.25 0.20  0.15  0.14 0.14     0.12
## [3,] 3.67 4.73 5.51 5.41 5.85 5.85  3.02  2.04 2.73     1.81
## [4,] 0.14 0.19 0.22 0.21 0.23 0.23  0.12  0.08 0.11     0.07
```

```r
write.csv(returns, "C:/Data/Thesis/6_L1ME_BMM_Returns.csv")
```





## $Size$ and $OP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), OP.OK)
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, OP)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $OP$ Quantiles


```r
quantiles =c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, OP) %>% group_by(Period) %>% summarize(
    D10=OP[.1*n()], D20=OP[.2*n()],
    D30=OP[.3*n()], D40=OP[.4*n()],
    D50=OP[.5*n()], D60=OP[.6*n()],
    D70=OP[.7*n()], D80=OP[.8*n()],
    D90=OP[.9*n()], D100=OP[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "OP")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "OP", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$OP.B = factor(df$OP.B, labels=c("Wo", "Mo", "Ro"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, OP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```





### Returns


```r
df$Size.OP.B = interaction(df$Size.B, df$OP.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.OP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$RMWos = returns$SRo - returns$SWo
returns$RMWob = returns$BRo - returns$BWo
returns$RMWo = (returns$SRo+returns$BRo)/2 - (returns$SWo+returns$BWo)/2
returns$SMB.RMWo = ((returns$SWo+returns$SMo+returns$SRo)/3 -
               (returns$BWo+returns$BMo+returns$BRo)/3)
RMWo = returns %>% select(Date, RMWo, SMB.RMWo)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##       SWo  BWo  SMo  BMo  SRo  BRo RMWos RMWob RMWo SMB.RMWo
## [1,] 0.97 0.76 1.25 0.89 1.28 0.97  0.31  0.21 0.26     0.29
## [2,] 0.25 0.20 0.20 0.17 0.23 0.17  0.11  0.10 0.09     0.11
## [3,] 3.82 3.87 6.09 5.19 5.62 5.73  2.95  2.22 3.03     2.57
## [4,] 0.15 0.15 0.24 0.20 0.22 0.23  0.12  0.09 0.12     0.10
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_OP_Returns.csv")
```

![French Website 6 $Size-OP$ Returns](C:/Data/FrenchDartmouth/6_Size_OP.JPG)

## $Size$ and $OP.2$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), OP.OK)
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, OP.2)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $OP.2$ Quantiles


```r
quantiles =c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, OP.2) %>% group_by(Period) %>% summarize(
    D10=OP.2[.1*n()], D20=OP.2[.2*n()],
    D30=OP.2[.3*n()], D40=OP.2[.4*n()],
    D50=OP.2[.5*n()], D60=OP.2[.6*n()],
    D70=OP.2[.7*n()], D80=OP.2[.8*n()],
    D90=OP.2[.9*n()], D100=OP.2[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "OP.2")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "OP.2", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$OP.2.B = factor(df$OP.2.B, labels=c("Wor", "Mor", "Ror"))

df = df %>% group_by(Size.B, OP.2.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```





### Returns


```r
df$Size.OP.2.B = interaction(df$Size.B, df$OP.2.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.OP.2.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$RMWors = returns$SRor - returns$SWor
returns$RMWorb = returns$BRor - returns$BWor
returns$RMWor = (returns$SRor+returns$BRor)/2 - (returns$SWor+returns$BWor)/2
returns$SMB.RMWor = ((returns$SWor+returns$SMor+returns$SRor)/3 -
               (returns$BWor+returns$BMor+returns$BRor)/3)
RMWor = returns %>% select(Date, RMWor, SMB.RMWor)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##      SWor BWor SMor BMor SRor BRor RMWors RMWorb RMWor SMB.RMWor
## [1,] 0.93 0.73 1.21 0.89 1.35 0.97   0.43   0.24  0.33      0.30
## [2,] 0.24 0.19 0.21 0.17 0.24 0.17   0.07   0.09  0.07      0.12
## [3,] 3.82 3.92 5.85 5.20 5.60 5.59   5.77   2.71  4.95      2.52
## [4,] 0.15 0.15 0.23 0.21 0.22 0.22   0.23   0.11  0.20      0.10
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_OP_RnD_Returns.csv")
```

## $Size$ and $GP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), !is.na(GP))
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, GP)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $GP$ Quantiles


```r
quantiles =c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, GP) %>% group_by(Period) %>% summarize(
    D10=GP[.1*n()], D20=GP[.2*n()],
    D30=GP[.3*n()], D40=GP[.4*n()],
    D50=GP[.5*n()], D60=GP[.6*n()],
    D70=GP[.7*n()], D80=GP[.8*n()],
    D90=GP[.9*n()], D100=GP[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "GP")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "GP", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$GP.B = factor(df$GP.B, labels=c("Wg", "Mg", "Rg"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, GP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.GP.B = interaction(df$Size.B, df$GP.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.GP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$RMWgs = returns$SRg - returns$SWg
returns$RMWgb = returns$BRg - returns$BWg
returns$RMWg = (returns$SRg+returns$BRg)/2 - (returns$SWg+returns$BWg)/2
returns$SMB.RMWg = ((returns$SWg+returns$SMg+returns$SRg)/3 -
               (returns$BWg+returns$BMg+returns$BRg)/3)
RMWg = returns %>% select(Date, RMWg, SMB.RMWg)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##       SWg  BWg  SMg  BMg  SRg  BRg RMWgs RMWgb RMWg SMB.RMWg
## [1,] 1.01 0.83 1.15 0.85 1.29 1.02  0.28  0.19 0.24     0.25
## [2,] 0.21 0.18 0.24 0.18 0.24 0.18  0.09  0.11 0.09     0.12
## [3,] 4.74 4.49 4.78 4.80 5.32 5.76  2.96  1.71 2.66     2.04
## [4,] 0.19 0.18 0.19 0.19 0.21 0.23  0.12  0.07 0.11     0.08
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_GP_Returns.csv")
```


## $Size$ and $CP$

* Annual rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), OP.OK)
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, CP)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $CP$ Quantiles


```r
quantiles =c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, CP) %>% group_by(Period) %>% summarize(
    D10=CP[.1*n()], D20=CP[.2*n()],
    D30=CP[.3*n()], D40=CP[.4*n()],
    D50=CP[.5*n()], D60=CP[.6*n()],
    D70=CP[.7*n()], D80=CP[.8*n()],
    D90=CP[.9*n()], D100=CP[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "CP")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "CP", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$CP.B = factor(df$CP.B, labels=c("Wc", "Mc", "Rc"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, CP.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```





### Returns


```r
df$Size.CP.B = interaction(df$Size.B, df$CP.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.CP.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$RMWcs = returns$SRc - returns$SWc
returns$RMWcb = returns$BRc - returns$BWc
returns$RMWc = (returns$SRc+returns$BRc)/2 - (returns$SWc+returns$BWc)/2
returns$SMB.RMWc = ((returns$SWc+returns$SMc+returns$SRc)/3 -
               (returns$BWc+returns$BMc+returns$BRc)/3)
RMWc = returns %>% select(Date, RMWc, SMB.RMWc)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##       SWc  BWc  SMc  BMc  SRc  BRc RMWcs RMWcb RMWc SMB.RMWc
## [1,] 0.87 0.73 1.24 0.84 1.40 1.03  0.53  0.29 0.41     0.30
## [2,] 0.24 0.20 0.21 0.17 0.23 0.17  0.06  0.08 0.06     0.12
## [3,] 3.58 3.75 5.81 5.01 6.13 5.97  8.75  3.53 7.38     2.57
## [4,] 0.14 0.15 0.23 0.20 0.24 0.24  0.35  0.14 0.29     0.10
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_CP_Returns.csv")
```

## $Size$ and $INV$

* Annual rebalance

### PPE


```r
df = combine.sources(crsp, comp)
df$INV = df$INV.ppe
df = df %>% filter(!is.na(Size), !is.na(INV))
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, INV)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $INV$ Quantiles


```r
quantiles = c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, INV) %>% group_by(Period) %>% summarize(
    D10=INV[.1*n()], D20=INV[.2*n()],
    D30=INV[.3*n()], D40=INV[.4*n()],
    D50=INV[.5*n()], D60=INV[.6*n()],
    D70=INV[.7*n()], D80=INV[.8*n()],
    D90=INV[.9*n()], D100=INV[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "INV")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "INV", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$INV.B = factor(df$INV.B, labels=c("C", "M", "A"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, INV.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.INV.B = interaction(df$Size.B, df$INV.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.INV.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$CMAs = returns$SC - returns$SA
returns$CMAb = returns$BC - returns$BA
returns$CMA = (returns$SC+returns$BC)/2 - (returns$SA+returns$BA)/2
returns$SMB.CMA = ((returns$SC+returns$SM+returns$SA)/3 -
               (returns$BC+returns$BM+returns$BA)/3)
CMA = returns %>% select(Date, CMA, SMB.CMA)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##        SC   BC   SM   BM   SA   BA CMAs CMAb  CMA SMB.CMA
## [1,] 1.28 0.93 1.30 0.93 1.01 0.87 0.27 0.05 0.16    0.29
## [2,] 0.21 0.16 0.22 0.16 0.26 0.20 0.09 0.12 0.09    0.12
## [3,] 6.05 5.86 6.01 5.77 3.95 4.26 3.19 0.47 1.78    2.39
## [4,] 0.24 0.23 0.24 0.23 0.16 0.17 0.13 0.02 0.07    0.09
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_INV_PPE_Returns.csv")
```

### Assets


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(Size), !is.na(INV))
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE", Month==7) %>% select(Period, PERMNO, Size, INV)

breakpoints = nyse %>% arrange(Period, Size) %>% group_by(Period) %>% summarize(
    D10=Size[.1*n()], D20=Size[.2*n()],
    D30=Size[.3*n()], D40=Size[.4*n()],
    D50=Size[.5*n()], D60=Size[.6*n()],
    D70=Size[.7*n()], D80=Size[.8*n()],
    D90=Size[.9*n()], D100=Size[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Size")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "Size", quantiles)
```

### $INV$ Quantiles


```r
quantiles = c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Period, INV) %>% group_by(Period) %>% summarize(
    D10=INV[.1*n()], D20=INV[.2*n()],
    D30=INV[.3*n()], D40=INV[.4*n()],
    D50=INV[.5*n()], D60=INV[.6*n()],
    D70=INV[.7*n()], D80=INV[.8*n()],
    D90=INV[.9*n()], D100=INV[n()]) %>% as.data.frame
breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "INV")] = NULL
df = left_join(df, breakpoints, by=c("Period"))
df = assign.bkts(df, "INV", quantiles)
```


```r
df$Size.B = factor(df$Size.B, labels=c("S", "B"))
df$INV.B = factor(df$INV.B, labels=c("C", "M", "A"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(Size.B, INV.B, Date) %>% mutate(
    adj.bkt.Size=sum(adj.Size, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$adj.Size / df$adj.bkt.Size
```

### Returns


```r
df$Size.INV.B = interaction(df$Size.B, df$INV.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ Size.INV.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns = returns %>% filter(Date>as.Date("1963-06-30"))

returns$CMAs = returns$SC - returns$SA
returns$CMAb = returns$BC - returns$BA
returns$CMA = (returns$SC+returns$BC)/2 - (returns$SA+returns$BA)/2
returns$SMB.CMA = ((returns$SC+returns$SM+returns$SA)/3 -
               (returns$BC+returns$BM+returns$BA)/3)
CMA = returns %>% select(Date, CMA, SMB.CMA)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##        SC   BC   SM   BM   SA   BA CMAs CMAb  CMA SMB.CMA
## [1,] 1.30 1.08 1.29 0.92 0.99 0.83 0.31 0.25 0.28    0.25
## [2,] 0.24 0.17 0.20 0.16 0.25 0.20 0.07 0.10 0.07    0.12
## [3,] 5.50 6.27 6.45 5.79 4.03 4.18 4.23 2.56 3.90    2.11
## [4,] 0.22 0.25 0.25 0.23 0.16 0.17 0.17 0.10 0.15    0.08
```

```r
write.csv(returns, "C:/Data/Thesis/6_Size_INV_Returns.csv")
```

![French Website 6 $Size-INV$ Returns](C:/Data/FrenchDartmouth/6_Size_INV.JPG)





## $Size$ and $Prior$

* Monthly rebalance


```r
df = combine.sources(crsp, comp)
df = df %>% filter(!is.na(L1.ME), !is.na(Prior), !is.na(L13.price), !is.na(L1.ri))
```

### $Size$ Median


```r
quantiles = 1:2/2

nyse = df %>% filter(Exchange=="NYSE") %>% select(Date, PERMNO, L1.ME, Prior)

breakpoints = nyse %>% arrange(Date, L1.ME) %>% group_by(Date) %>% summarize(
    D10=L1.ME[.1*n()], D20=L1.ME[.2*n()],
    D30=L1.ME[.3*n()], D40=L1.ME[.4*n()],
    D50=L1.ME[.5*n()], D60=L1.ME[.6*n()],
    D70=L1.ME[.7*n()], D80=L1.ME[.8*n()],
    D90=L1.ME[.9*n()], D100=L1.ME[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "L1.ME")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "L1.ME", quantiles)
```

### $Prior$ Quantiles


```r
quantiles = c(.3, .7, 1.)

breakpoints = nyse %>% arrange(Date, Prior) %>% group_by(Date) %>% summarize(
    D10=Prior[.1*n()], D20=Prior[.2*n()],
    D30=Prior[.3*n()], D40=Prior[.4*n()],
    D50=Prior[.5*n()], D60=Prior[.6*n()],
    D70=Prior[.7*n()], D80=Prior[.8*n()],
    D90=Prior[.9*n()], D100=Prior[n()]) %>% as.data.frame
# breakpoints$Period = breakpoints$Period + 1
breakpoints[, c("PERMNO", "Prior")] = NULL
df = left_join(df, breakpoints, by=c("Date"))
df = assign.bkts(df, "Prior", quantiles)
```


```r
df$L1.ME.B = factor(df$L1.ME.B, labels=c("S", "B"))
df$Prior.B = factor(df$Prior.B, labels=c("L", "M", "W"))
```


```r
# df = ri.adj.Size(df)

df = df %>% group_by(L1.ME.B, Prior.B, Date) %>% mutate(
    bkt.L1.ME=sum(L1.ME, na.rm=TRUE)
) %>% as.data.frame
df$wt.ri = df$ri * df$L1.ME / df$bkt.L1.ME
```

### Returns


```r
df$L1.ME.Prior.B = interaction(df$L1.ME.B, df$Prior.B, sep="")
df$wt.ri = df$wt.ri * 100

returns = df %>% dcast(Date ~ L1.ME.Prior.B, fun.aggregate=sum, value.var="wt.ri", na.rm=TRUE)

returns$WMLs = returns$SW - returns$SL
returns$WMLb = returns$BW - returns$BL
returns$WML = (returns$SW+returns$BW)/2 - (returns$SL+returns$BL)/2
returns$SMB.WML = ((returns$SL+returns$SM+returns$SW)/3 -
               (returns$BL+returns$BM+returns$BW)/3)
WML = returns %>% select(Date, WML, SMB.WML)

stat.summary(returns %>% select(-c(Date, `NA`))) %>% round(2)
```

```
##        SL   BL   SM   BM   SW   BW WMLs WMLb  WML SMB.WML
## [1,] 0.71 0.79 1.20 0.86 1.55 1.16 0.84 0.38 0.61    0.22
## [2,] 0.27 0.22 0.20 0.16 0.24 0.19 0.15 0.17 0.16    0.11
## [3,] 2.66 3.61 5.93 5.27 6.33 6.04 5.42 2.15 3.87    1.92
## [4,] 0.10 0.14 0.23 0.21 0.25 0.24 0.21 0.08 0.15    0.08
```

```r
write.csv(returns, "C:/Data/Thesis/25_L1ME_Prior_Returns.csv")
```

![French Website 6 $Size-Prior$ Returns](C:/Data/FrenchDartmouth/6_Size_Prior.JPG)





# Summarise Factors


```r
f = left_join(HML, HMLm, by="Date")
f = left_join(f, RMWo, by="Date")
f = left_join(f, RMWor, by="Date")
f = left_join(f, RMWg, by="Date")
f = left_join(f, RMWc, by="Date")
f = left_join(f, CMA, by="Date")
f = left_join(f, WML, by="Date")
f = left_join(f, rm, by="Date")
f$SMB = (f$SMB.HML + f$SMB.RMWo + f$SMB.CMA) / 3

stat.summary(
    f %>% select(
        -c(Date, SMB.HML, SMB.HMLm, SMB.RMWo, SMB.RMWor, SMB.RMWc, SMB.RMWg, SMB.CMA, SMB.WML)
    )
) %>% round(2)
```

```
##       HML HMLm RMWo RMWor RMWg RMWc  CMA  WML   Rm  SMB
## [1,] 0.32 0.37 0.26  0.33 0.24 0.41 0.28 0.61 0.50 0.26
## [2,] 0.11 0.14 0.09  0.07 0.09 0.06 0.07 0.16 0.17 0.12
## [3,] 2.89 2.73 3.03  4.95 2.66 7.38 3.90 3.82 2.84 2.21
## [4,] 0.11 0.11 0.12  0.20 0.11 0.29 0.15 0.15 0.11 0.09
```

```r
write.csv(f, "C:/Data/Thesis/factors.csv")
```


```r
cor(
    f %>% select(
        -c(Date, SMB.HML, SMB.HMLm, SMB.RMWo, SMB.RMWor, SMB.RMWg, SMB.CMA, SMB.WML)
    )
) %>% round(2) %>% as.data.frame
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["HML"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["HMLm"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["RMWo"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["RMWor"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMWg"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["RMWc"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["SMB.RMWc"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["CMA"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["WML"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Rm"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["SMB"],"name":[11],"type":["dbl"],"align":["right"]}],"data":[{"1":"1.00","2":"0.79","3":"0.12","4":"-0.33","5":"-0.59","6":"-0.22","7":"-0.12","8":"0.65","9":"-0.22","10":"-0.33","11":"-0.12","_rn_":"HML"},{"1":"0.79","2":"1.00","3":"0.03","4":"-0.37","5":"-0.51","6":"-0.34","7":"-0.08","8":"0.48","9":"-0.68","10":"-0.16","11":"-0.06","_rn_":"HMLm"},{"1":"0.12","2":"0.03","3":"1.00","4":"0.78","5":"0.24","6":"0.63","7":"-0.35","8":"-0.14","9":"0.01","10":"-0.23","11":"-0.35","_rn_":"RMWo"},{"1":"-0.33","2":"-0.37","3":"0.78","4":"1.00","5":"0.62","6":"0.78","7":"-0.11","8":"-0.36","9":"0.16","10":"-0.01","11":"-0.13","_rn_":"RMWor"},{"1":"-0.59","2":"-0.51","3":"0.24","4":"0.62","5":"1.00","6":"0.54","7":"0.17","8":"-0.41","9":"0.14","10":"0.14","11":"0.16","_rn_":"RMWg"},{"1":"-0.22","2":"-0.34","3":"0.63","4":"0.78","5":"0.54","6":"1.00","7":"-0.19","8":"-0.14","9":"0.24","10":"-0.24","11":"-0.24","_rn_":"RMWc"},{"1":"-0.12","2":"-0.08","3":"-0.35","4":"-0.11","5":"0.17","6":"-0.19","7":"1.00","8":"-0.01","9":"0.07","10":"0.26","11":"0.99","_rn_":"SMB.RMWc"},{"1":"0.65","2":"0.48","3":"-0.14","4":"-0.36","5":"-0.41","6":"-0.14","7":"-0.01","8":"1.00","9":"-0.08","10":"-0.35","11":"-0.03","_rn_":"CMA"},{"1":"-0.22","2":"-0.68","3":"0.01","4":"0.16","5":"0.14","6":"0.24","7":"0.07","8":"-0.08","9":"1.00","10":"-0.05","11":"0.04","_rn_":"WML"},{"1":"-0.33","2":"-0.16","3":"-0.23","4":"-0.01","5":"0.14","6":"-0.24","7":"0.26","8":"-0.35","9":"-0.05","10":"1.00","11":"0.27","_rn_":"Rm"},{"1":"-0.12","2":"-0.06","3":"-0.35","4":"-0.13","5":"0.16","6":"-0.24","7":"0.99","8":"-0.03","9":"0.04","10":"0.27","11":"1.00","_rn_":"SMB"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


# Asness and Frazzini (2013) Exhibit 6 Regression 1


```r
summary(lm(HML~Rm+SMB+HMLm+WML, data=f))
```

```
## 
## Call:
## lm(formula = HML ~ Rm + SMB + HMLm + WML, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.8804 -0.6302  0.0226  0.5929  6.3982 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.22514    0.04953  -4.546 6.56e-06 ***
## Rm          -0.07078    0.01128  -6.274 6.52e-10 ***
## SMB         -0.04243    0.01644  -2.581   0.0101 *  
## HMLm         0.94606    0.01889  50.070  < 2e-16 ***
## WML          0.39874    0.01615  24.695  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.171 on 637 degrees of freedom
## Multiple R-squared:  0.8311,	Adjusted R-squared:   0.83 
## F-statistic: 783.5 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+WML, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + WML, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.2879  -1.3268   0.0088   1.3362  13.2740 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.80531    0.09876   8.154 1.87e-15 ***
## Rm          -0.15470    0.02283  -6.776 2.82e-11 ***
## SMB          0.02543    0.03444   0.738    0.461    
## WML         -0.59701    0.02421 -24.662  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.454 on 638 degrees of freedom
## Multiple R-squared:  0.5014,	Adjusted R-squared:  0.499 
## F-statistic: 213.8 on 3 and 638 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+HML+WML, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + HML + WML, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4562 -0.5916 -0.0191  0.5563  7.3550 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.35293    0.04540   7.774 3.06e-14 ***
## Rm           0.02831    0.01092   2.594  0.00971 ** 
## SMB          0.04092    0.01552   2.637  0.00857 ** 
## HML          0.84285    0.01683  50.070  < 2e-16 ***
## WML         -0.45704    0.01126 -40.599  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.106 on 637 degrees of freedom
## Multiple R-squared:  0.899,	Adjusted R-squared:  0.8983 
## F-statistic:  1417 on 4 and 637 DF,  p-value: < 2.2e-16
```

# Asness and Frazzini (2013) Exhibit 6 Regression 4


```r
summary(lm(HMLm~Rm+SMB+HML+WML, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + HML + WML, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4562 -0.5916 -0.0191  0.5563  7.3550 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.35293    0.04540   7.774 3.06e-14 ***
## Rm           0.02831    0.01092   2.594  0.00971 ** 
## SMB          0.04092    0.01552   2.637  0.00857 ** 
## HML          0.84285    0.01683  50.070  < 2e-16 ***
## WML         -0.45704    0.01126 -40.599  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.106 on 637 degrees of freedom
## Multiple R-squared:  0.899,	Adjusted R-squared:  0.8983 
## F-statistic:  1417 on 4 and 637 DF,  p-value: < 2.2e-16
```

