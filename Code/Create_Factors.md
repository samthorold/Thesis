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
```


```r
combine.sources = function(crsp, comp){

    df = left_join(
        crsp,
        comp %>% select(PERMNO, Period, BE, D1.BE, OP, OP.OK, GP, INV),
        by=c("PERMNO", "Period"))

    df$BM = df$BE/df$M              # Annual, Lagged
    df$bm = log(df$BM)              # Log Annual, Lagged
    df$BMC = df$BE/df$Size          # Annual, Current
    df$BMM = df$BE/df$L1.ME         # Monthly, Current
    
    df$BM[is.infinite(df$BM)] = NA
    df$bm[is.infinite(df$bm)] = NA
    df$BMC[is.infinite(df$BMC)] = NA
    df$BMM[is.infinite(df$BMM)] = NA

    return(df %>% filter(Date>as.Date("1963-06-30")))
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
comp = fread("C:/Data/CRSP/COMP_196001_201612_c.csv", showProgress=FALSE)
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
CRSP data has 287 067 rows and
27 columns.


```r
# duplicated will omit return FALSE for the first instance of a duplicated row
# calling from the top and the bottom ensure all instances are caught
pairs.ix = duplicated(comp[, c("PERMNO", "fyear")]) |
           duplicated(comp[, c("PERMNO", "fyear")], fromLast=TRUE)
comp[pairs.ix] %>% arrange(PERMNO, fyear, fyr)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["PERMNO"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["datadate"],"name":[2],"type":["int"],"align":["right"]},{"label":["fyear"],"name":[3],"type":["int"],"align":["right"]},{"label":["indfmt"],"name":[4],"type":["chr"],"align":["left"]},{"label":["consol"],"name":[5],"type":["chr"],"align":["left"]},{"label":["popsrc"],"name":[6],"type":["chr"],"align":["left"]},{"label":["datafmt"],"name":[7],"type":["chr"],"align":["left"]},{"label":["curcd"],"name":[8],"type":["chr"],"align":["left"]},{"label":["fyr"],"name":[9],"type":["int"],"align":["right"]},{"label":["at"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["ceq"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["cogs"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["gp"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["lt"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["pstk"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["pstkl"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["pstkrv"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["revt"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["seq"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["txditc"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["upstk"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["xint"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["xintd"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["xsga"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["costat"],"name":[25],"type":["chr"],"align":["left"]},{"label":["fyrc"],"name":[26],"type":["int"],"align":["right"]},{"label":["Date"],"name":[27],"type":["date"],"align":["right"]}],"data":[{"1":"22074","2":"19870630","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"149.379","11":"63.262","12":"341.350","13":"79.229","14":"86.117","15":"0.000","16":"0.000","17":"0.000","18":"420.579","19":"63.262","20":"0","21":"NA","22":"12.969","23":"NA","24":"67.039","25":"I","26":"6","27":"1987-06-30"},{"1":"22074","2":"19871231","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"1056.500","11":"153.900","12":"NA","13":"NA","14":"902.600","15":"0.000","16":"0.000","17":"0.000","18":"NA","19":"153.900","20":"0","21":"NA","22":"NA","23":"NA","24":"NA","25":"I","26":"12","27":"1987-12-31"},{"1":"67563","2":"19980630","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"18116.737","11":"1234.060","12":"716.058","13":"546.775","14":"16878.060","15":"4.617","16":"115.425","17":"4.617","18":"1262.833","19":"1238.677","20":"NA","21":"NA","22":"NA","23":"NA","24":"185.259","25":"I","26":"6","27":"1998-06-30"},{"1":"67563","2":"19981231","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"54868.984","11":"1581.778","12":"1859.545","13":"1057.442","14":"52693.768","15":"0.000","16":"0.000","17":"0.000","18":"2916.987","19":"1581.778","20":"NA","21":"NA","22":"NA","23":"NA","24":"393.728","25":"I","26":"12","27":"1998-12-31"},{"1":"75228","2":"19871031","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"10","10":"17.970","11":"7.217","12":"8.045","13":"5.446","14":"10.753","15":"0.000","16":"0.000","17":"0.000","18":"13.491","19":"7.217","20":"0","21":"NA","22":"0.184","23":"NA","24":"4.193","25":"I","26":"10","27":"1987-10-31"},{"1":"75228","2":"19871231","3":"1987","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"45.769","11":"33.247","12":"2.044","13":"2.196","14":"12.522","15":"0.000","16":"0.000","17":"0.000","18":"4.240","19":"33.247","20":"0","21":"NA","22":"0.147","23":"NA","24":"3.478","25":"I","26":"12","27":"1987-12-31"},{"1":"85531","2":"19980630","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"NA","11":"NA","12":"90.868","13":"16.265","14":"NA","15":"NA","16":"NA","17":"NA","18":"107.133","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"12.021","25":"I","26":"6","27":"1998-06-30"},{"1":"85531","2":"19981231","3":"1998","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"91.463","11":"53.380","12":"177.740","13":"31.632","14":"38.083","15":"0.000","16":"0.000","17":"0.000","18":"209.372","19":"53.380","20":"0","21":"NA","22":"NA","23":"NA","24":"18.972","25":"A","26":"12","27":"1998-12-31"},{"1":"88031","2":"19900630","3":"1990","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"6","10":"1.613","11":"1.569","12":"0.089","13":"0.043","14":"0.044","15":"0.000","16":"0.000","17":"0.000","18":"0.132","19":"1.569","20":"0","21":"NA","22":"NA","23":"NA","24":"NA","25":"I","26":"6","27":"1990-06-30"},{"1":"88031","2":"19901231","3":"1990","4":"INDL","5":"C","6":"D","7":"STD","8":"USD","9":"12","10":"7.139","11":"6.924","12":"NA","13":"NA","14":"0.215","15":"0.000","16":"0.000","17":"0.000","18":"NA","19":"6.924","20":"0","21":"NA","22":"NA","23":"NA","24":"NA","25":"A","26":"12","27":"1990-12-31"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
##  1st Qu.:    13.6   1st Qu.:    -1.01  
##  Median :    63.3   Median :     2.92  
##  Mean   :  1036.6   Mean   :    67.66  
##  3rd Qu.:   309.8   3rd Qu.:    26.02  
##  Max.   :359960.0   Max.   :223210.69  
##  NA's   :19539      NA's   :43665
```

## Operating Profit, $OP$


```r
exp.OK = !is.na(comp$cogs) | !is.na(!comp$xsga) | !is.na(comp$xint)
comp$cogs[is.na(comp$cogs)] = 0
comp$xsga[is.na(comp$xsga)] = 0
comp$xint[is.na(comp$xint)] = 0

comp$op.OK = !is.na(comp$revt) & exp.OK
comp$OP.OK = (comp$BE > 0) & comp$op.OK
comp$OP.OK[is.na(comp$OP.OK)] = FALSE
comp$OP = (comp$revt - comp$cogs - comp$xsga - comp$xint)/comp$BE
summary(comp$OP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -Inf   0.057   0.196     NaN   0.315     Inf   20722
```

```r
summary(comp$OP[comp$OP.OK])
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -1941.000     0.062     0.196     0.132     0.310  9424.000
```

## Gross Profit, $GP$


```r
comp$GP = comp$gp / comp$at
summary(comp$GP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -Inf   0.097   0.266     NaN   0.460     Inf   20234
```

```r
ix = is.na(comp$GP)
comp$GP[ix] = comp$revt[ix] - comp$cogs[ix]
summary(comp$GP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -Inf   0.097   0.266     NaN   0.460     Inf   19878
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
##   -1.00   -0.03    0.07    0.22    0.19 7898.00   42927
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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2135.2778"},{"1":"2","2":"567.7730"},{"1":"3","2":"376.7571"},{"1":"4","2":"302.8016"},{"1":"5","2":"249.8349"},{"1":"6","2":"212.4508"},{"1":"7","2":"192.8397"},{"1":"8","2":"183.2619"},{"1":"9","2":"167.4667"},{"1":"10","2":"169.9619"},{"1":"NA","2":"294.2118"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 1.16 1.11 1.12 1.10 1.10 1.05 1.06 1.03 0.96 0.81 0.39
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
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"611.34762"},{"1":"2","2":"405.57619"},{"1":"3","2":"358.21905"},{"1":"4","2":"338.89841"},{"1":"5","2":"314.99683"},{"1":"6","2":"314.39048"},{"1":"7","2":"313.62540"},{"1":"8","2":"331.68413"},{"1":"9","2":"367.40317"},{"1":"10","2":"512.98730"},{"1":"NA","2":"27.38312"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 0.79 0.91 0.88 0.88 1.03 0.98 0.98 1.09 1.04 1.15 1.20
```

![French Website $BM$ Decile Returns](C:/Data/FrenchDartmouth/10_BM.JPG)

### $ME$


```r
df %>% group_by(BM.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(BM.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2912.77"},{"1":"2","2":"2747.47"},{"1":"3","2":"2262.03"},{"1":"4","2":"1917.49"},{"1":"5","2":"1511.77"},{"1":"6","2":"1421.29"},{"1":"7","2":"1070.03"},{"1":"8","2":"989.19"},{"1":"9","2":"829.94"},{"1":"10","2":"538.97"},{"1":"NA","2":"86.83"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(BM.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(BM.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2822.42"},{"1":"2","2":"2637.25"},{"1":"3","2":"2182.41"},{"1":"4","2":"1851.00"},{"1":"5","2":"1441.03"},{"1":"6","2":"1380.53"},{"1":"7","2":"1031.46"},{"1":"8","2":"952.41"},{"1":"9","2":"798.66"},{"1":"10","2":"510.55"},{"1":"NA","2":"73.10"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(BM.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(BM.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.14"},{"1":"2","2":"0.13"},{"1":"3","2":"0.13"},{"1":"4","2":"0.12"},{"1":"5","2":"0.13"},{"1":"6","2":"0.13"},{"1":"7","2":"0.14"},{"1":"8","2":"0.14"},{"1":"9","2":"0.15"},{"1":"10","2":"0.17"},{"1":"NA","2":"0.46"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
##  Length:219415      Min.   :-906.639   Min.   :-222.335  
##  Class :character   1st Qu.:   0.349   1st Qu.:   0.326  
##  Mode  :character   Median :   0.654   Median :   0.623  
##                     Mean   :   0.861   Mean   :   0.817  
##                     3rd Qu.:   1.116   3rd Qu.:   1.063  
##                     Max.   : 294.428   Max.   : 202.201  
##                     NA's   :20089      NA's   :20248     
##   L1.BMC_L1.BM     
##  Min.   :-194.602  
##  1st Qu.:  -0.144  
##  Median :  -0.024  
##  Mean   :  -0.043  
##  3rd Qu.:   0.066  
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
## [1,]  0.04856159  0.97066786    0.8661963 0.10447156
## [2,]  0.04379398  0.06168039    0.0938937 0.06856958
## [3,]  1.10886447 15.73705791    9.2252863 1.52358478
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
##  Length:214378      Min.   :1963   Min.   :-10.017   Min.   :-6.190  
##  Class :character   1st Qu.:1983   1st Qu.: -0.090   1st Qu.:-0.194  
##  Mode  :character   Median :1994   Median :  0.052   Median : 0.062  
##                     Mean   :1993   Mean   :  0.005   Mean   : 0.034  
##                     3rd Qu.:2003   3rd Qu.:  0.143   3rd Qu.: 0.292  
##                     Max.   :2016   Max.   :  8.555   Max.   : 3.819  
##                                    NA's   :19932     NA's   :29626
```


```r
df %>% group_by(Period) %>% summarise(N=n())
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Period"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["N"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"1963","2":"916"},{"1":"1964","2":"1002"},{"1":"1965","2":"1097"},{"1":"1966","2":"1231"},{"1":"1967","2":"1453"},{"1":"1968","2":"1546"},{"1":"1969","2":"1704"},{"1":"1970","2":"1869"},{"1":"1971","2":"1999"},{"1":"1972","2":"2116"},{"1":"1973","2":"3132"},{"1":"1974","2":"3641"},{"1":"1975","2":"3892"},{"1":"1976","2":"3871"},{"1":"1977","2":"3844"},{"1":"1978","2":"3756"},{"1":"1979","2":"3786"},{"1":"1980","2":"3880"},{"1":"1981","2":"3974"},{"1":"1982","2":"4226"},{"1":"1983","2":"4261"},{"1":"1984","2":"4640"},{"1":"1985","2":"4658"},{"1":"1986","2":"4621"},{"1":"1987","2":"4817"},{"1":"1988","2":"4916"},{"1":"1989","2":"4752"},{"1":"1990","2":"4645"},{"1":"1991","2":"4614"},{"1":"1992","2":"4655"},{"1":"1993","2":"4957"},{"1":"1994","2":"5933"},{"1":"1995","2":"6181"},{"1":"1996","2":"6372"},{"1":"1997","2":"6732"},{"1":"1998","2":"6535"},{"1":"1999","2":"6101"},{"1":"2000","2":"5977"},{"1":"2001","2":"5619"},{"1":"2002","2":"5074"},{"1":"2003","2":"4731"},{"1":"2004","2":"4541"},{"1":"2005","2":"4485"},{"1":"2006","2":"4401"},{"1":"2007","2":"4316"},{"1":"2008","2":"4265"},{"1":"2009","2":"3948"},{"1":"2010","2":"3761"},{"1":"2011","2":"3633"},{"1":"2012","2":"3535"},{"1":"2013","2":"3413"},{"1":"2014","2":"3430"},{"1":"2015","2":"3519"},{"1":"2016","2":"3405"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
##       (Intercept)    L1.Prior
## [1,] 0.0007513482  0.18009355
## [2,] 0.0060705228  0.01173963
## [3,] 0.1237699282 15.34065448
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
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"618.85556"},{"1":"2","2":"388.32222"},{"1":"3","2":"354.12063"},{"1":"4","2":"333.82857"},{"1":"5","2":"327.19524"},{"1":"6","2":"324.37302"},{"1":"7","2":"325.54286"},{"1":"8","2":"340.77937"},{"1":"9","2":"372.85079"},{"1":"10","2":"499.86825"},{"1":"NA","2":"32.41723"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 0.82 0.87 0.92 0.91 0.85 1.00 0.92 1.07 1.13 1.17 0.29
```

### $ME$


```r
df %>% group_by(BMC.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(BMC.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2879.79"},{"1":"2","2":"2705.06"},{"1":"3","2":"2449.31"},{"1":"4","2":"1848.84"},{"1":"5","2":"1713.81"},{"1":"6","2":"1327.75"},{"1":"7","2":"1176.55"},{"1":"8","2":"942.88"},{"1":"9","2":"699.57"},{"1":"10","2":"531.17"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(BMC.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(BMC.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2788.38"},{"1":"2","2":"2609.42"},{"1":"3","2":"2367.69"},{"1":"4","2":"1784.54"},{"1":"5","2":"1657.69"},{"1":"6","2":"1291.81"},{"1":"7","2":"1138.31"},{"1":"8","2":"910.01"},{"1":"9","2":"681.65"},{"1":"10","2":"499.14"},{"1":"NA","2":"133.07"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(BMC.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(BMC.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMC.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.28"},{"1":"2","2":"0.20"},{"1":"3","2":"0.16"},{"1":"4","2":"0.15"},{"1":"5","2":"0.13"},{"1":"6","2":"0.12"},{"1":"7","2":"0.10"},{"1":"8","2":"0.09"},{"1":"9","2":"0.07"},{"1":"10","2":"0.01"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"554.802181"},{"1":"2","2":"373.105919"},{"1":"3","2":"343.476636"},{"1":"4","2":"330.459502"},{"1":"5","2":"329.211838"},{"1":"6","2":"334.370717"},{"1":"7","2":"338.433022"},{"1":"8","2":"354.616822"},{"1":"9","2":"389.926791"},{"1":"10","2":"489.973520"},{"1":"NA","2":"3.621495"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 0.87 0.85 0.88 0.91 0.94 1.03 1.00 1.16 1.26 1.29 4.03
```

### $ME$


```r
df %>% group_by(BMM.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(BMM.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2910.91"},{"1":"2","2":"2801.63"},{"1":"3","2":"2285.51"},{"1":"4","2":"1976.97"},{"1":"5","2":"1599.42"},{"1":"6","2":"1419.61"},{"1":"7","2":"1134.28"},{"1":"8","2":"943.04"},{"1":"9","2":"715.49"},{"1":"10","2":"426.77"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(BMM.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(BMM.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"2695.28"},{"1":"2","2":"2667.33"},{"1":"3","2":"2191.21"},{"1":"4","2":"1913.16"},{"1":"5","2":"1555.42"},{"1":"6","2":"1390.16"},{"1":"7","2":"1122.45"},{"1":"8","2":"950.14"},{"1":"9","2":"741.14"},{"1":"10","2":"458.65"},{"1":"NA","2":"49.66"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(BMM.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(BMM.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["BMM.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.41"},{"1":"2","2":"0.26"},{"1":"3","2":"0.21"},{"1":"4","2":"0.17"},{"1":"5","2":"0.14"},{"1":"6","2":"0.11"},{"1":"7","2":"0.09"},{"1":"8","2":"0.06"},{"1":"9","2":"0.01"},{"1":"10","2":"-0.13"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"970.95873"},{"1":"2","2":"419.43968"},{"1":"3","2":"328.84444"},{"1":"4","2":"316.16508"},{"1":"5","2":"314.91905"},{"1":"6","2":"317.07937"},{"1":"7","2":"298.27937"},{"1":"8","2":"289.59841"},{"1":"9","2":"292.37460"},{"1":"10","2":"323.49841"},{"1":"NA","2":"23.95506"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 0.69 0.80 0.79 0.91 0.80 0.94 0.91 0.97 0.99 0.94 1.55
```

![French Website $OP$ Decile Returns](C:/Data/FrenchDartmouth/10_OP.JPG)

### $BM$


```r
df %>% group_by(OP.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(OP.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.19"},{"1":"2","2":"1.29"},{"1":"3","2":"1.14"},{"1":"4","2":"1.02"},{"1":"5","2":"0.93"},{"1":"6","2":"0.85"},{"1":"7","2":"0.75"},{"1":"8","2":"0.68"},{"1":"9","2":"0.58"},{"1":"10","2":"0.48"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Monthly $BM$


```r
df %>% group_by(OP.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(OP.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.29"},{"1":"2","2":"1.30"},{"1":"3","2":"1.13"},{"1":"4","2":"1.00"},{"1":"5","2":"0.89"},{"1":"6","2":"0.82"},{"1":"7","2":"0.73"},{"1":"8","2":"0.65"},{"1":"9","2":"0.62"},{"1":"10","2":"0.45"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["OP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.INV"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.18"},{"1":"2","2":"0.15"},{"1":"3","2":"0.18"},{"1":"4","2":"0.17"},{"1":"5","2":"0.16"},{"1":"6","2":"0.16"},{"1":"7","2":"0.17"},{"1":"8","2":"0.19"},{"1":"9","2":"0.21"},{"1":"10","2":"0.28"},{"1":"NA","2":"0.26"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

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
{"columns":[{"label":["GP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"688.37619"},{"1":"2","2":"300.24286"},{"1":"3","2":"277.24603"},{"1":"4","2":"309.23968"},{"1":"5","2":"336.13810"},{"1":"6","2":"342.24603"},{"1":"7","2":"380.61429"},{"1":"8","2":"403.84921"},{"1":"9","2":"449.83333"},{"1":"10","2":"497.57460"},{"1":"NA","2":"24.39347"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 0.80 0.82 0.84 0.79 0.88 0.96 0.77 0.97 0.97 1.07 1.03
```

### $BM$


```r
df %>% group_by(GP.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(GP.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["GP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.96"},{"1":"2","2":"1.03"},{"1":"3","2":"1.05"},{"1":"4","2":"1.02"},{"1":"5","2":"0.95"},{"1":"6","2":"0.90"},{"1":"7","2":"0.82"},{"1":"8","2":"0.76"},{"1":"9","2":"0.71"},{"1":"10","2":"0.64"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
x= df %>% filter(Month==7) %>%
    dcast(Date ~ GP.B, fun.aggregate=mean, value.var="BM", na.rm=TRUE)
x$diff = x$`1` - x$`10`
mean(x$diff, na.rm=TRUE)
```

```
## [1] 0.3134514
```

```r
se(x$diff)
```

```
## [1] 0.02726606
```

### Monthly $BM$


```r
df %>% group_by(GP.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(GP.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["GP.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.02"},{"1":"2","2":"1.12"},{"1":"3","2":"1.12"},{"1":"4","2":"1.07"},{"1":"5","2":"0.96"},{"1":"6","2":"0.90"},{"1":"7","2":"0.82"},{"1":"8","2":"0.76"},{"1":"9","2":"0.68"},{"1":"10","2":"0.63"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## $CP$

* Annual rebalance?


```r
df = combine.sources(crsp, comp)
```

### Returns

## $INV$

* Annual rebalance

* Annual rebalance


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
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"589.37460"},{"1":"2","2":"343.32063"},{"1":"3","2":"292.83492"},{"1":"4","2":"284.89524"},{"1":"5","2":"279.06984"},{"1":"6","2":"294.33968"},{"1":"7","2":"301.65079"},{"1":"8","2":"332.08730"},{"1":"9","2":"384.62698"},{"1":"10","2":"547.25397"},{"1":"NA","2":"22.89604"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
df = ri.adj.Size(df)

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
##  1.11  1.10  1.07  0.95  0.96  0.89  0.90  0.80  0.86  0.77 -0.45
```

![French Website $INV$ Decile Returns](C:/Data/FrenchDartmouth/10_INV.JPG)

### $Size$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"510.08"},{"1":"2","2":"1205.41"},{"1":"3","2":"1793.38"},{"1":"4","2":"1850.65"},{"1":"5","2":"2070.19"},{"1":"6","2":"2300.44"},{"1":"7","2":"2230.63"},{"1":"8","2":"2130.10"},{"1":"9","2":"1758.19"},{"1":"10","2":"1357.06"},{"1":"NA","2":"423.03"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $BM$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.93"},{"1":"2","2":"1.13"},{"1":"3","2":"1.08"},{"1":"4","2":"1.00"},{"1":"5","2":"0.95"},{"1":"6","2":"0.89"},{"1":"7","2":"0.85"},{"1":"8","2":"0.78"},{"1":"9","2":"0.71"},{"1":"10","2":"0.60"},{"1":"NA","2":"0.58"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Monthly $BM$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.94"},{"1":"2","2":"1.18"},{"1":"3","2":"1.10"},{"1":"4","2":"1.00"},{"1":"5","2":"0.95"},{"1":"6","2":"0.88"},{"1":"7","2":"0.86"},{"1":"8","2":"0.78"},{"1":"9","2":"0.74"},{"1":"10","2":"0.69"},{"1":"NA","2":"0.74"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.GP"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.35"},{"1":"2","2":"0.44"},{"1":"3","2":"0.43"},{"1":"4","2":"0.54"},{"1":"5","2":"0.77"},{"1":"6","2":"0.36"},{"1":"7","2":"0.36"},{"1":"8","2":"0.77"},{"1":"9","2":"0.38"},{"1":"10","2":"0.37"},{"1":"NA","2":"0.19"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Prior$


```r
df %>% group_by(INV.B, Date) %>% summarise(avg.Prior=mean(Prior, na.rm=TRUE)) %>%
    group_by(INV.B) %>% summarise(avg.Prior=mean(avg.Prior)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["INV.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Prior"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.17"},{"1":"2","2":"0.15"},{"1":"3","2":"0.14"},{"1":"4","2":"0.14"},{"1":"5","2":"0.13"},{"1":"6","2":"0.14"},{"1":"7","2":"0.14"},{"1":"8","2":"0.14"},{"1":"9","2":"0.13"},{"1":"10","2":"0.11"},{"1":"NA","2":"0.05"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.N"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"691.14798"},{"1":"2","2":"428.36760"},{"1":"3","2":"371.23209"},{"1":"4","2":"344.17757"},{"1":"5","2":"330.98287"},{"1":"6","2":"326.20872"},{"1":"7","2":"334.04984"},{"1":"8","2":"354.32243"},{"1":"9","2":"399.26012"},{"1":"10","2":"646.05296"},{"1":"NA","2":"23.77796"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
## 0.23 0.77 0.88 0.86 0.83 0.85 0.97 1.07 1.14 1.49 1.12
```

### $ME$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.ME=mean(ME, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.ME=mean(avg.ME)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.ME"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"433.54"},{"1":"2","2":"1246.24"},{"1":"3","2":"1674.49"},{"1":"4","2":"2016.21"},{"1":"5","2":"2246.23"},{"1":"6","2":"2321.67"},{"1":"7","2":"2297.18"},{"1":"8","2":"2182.77"},{"1":"9","2":"1822.22"},{"1":"10","2":"965.14"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $Size$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.Size=mean(Size, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.Size=mean(avg.Size)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.Size"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"538.17"},{"1":"2","2":"1341.52"},{"1":"3","2":"1722.57"},{"1":"4","2":"2021.78"},{"1":"5","2":"2197.67"},{"1":"6","2":"2230.63"},{"1":"7","2":"2159.09"},{"1":"8","2":"1992.74"},{"1":"9","2":"1598.74"},{"1":"10","2":"766.32"},{"1":"NA","2":"120.55"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### $BM$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.BM=mean(BM, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.BM=mean(avg.BM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.78"},{"1":"2","2":"0.86"},{"1":"3","2":"0.88"},{"1":"4","2":"0.88"},{"1":"5","2":"0.89"},{"1":"6","2":"0.90"},{"1":"7","2":"0.90"},{"1":"8","2":"0.90"},{"1":"9","2":"0.89"},{"1":"10","2":"0.89"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Monthly $BM$


```r
df %>% group_by(Prior.B, Date) %>% summarise(avg.BMM=mean(BMM, na.rm=TRUE)) %>%
    group_by(Prior.B) %>% summarise(avg.BMM=mean(avg.BMM)) %>% round(2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Prior.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["avg.BMM"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.45"},{"1":"2","2":"1.06"},{"1":"3","2":"0.97"},{"1":"4","2":"0.91"},{"1":"5","2":"0.86"},{"1":"6","2":"0.82"},{"1":"7","2":"0.78"},{"1":"8","2":"0.73"},{"1":"9","2":"0.66"},{"1":"10","2":"0.51"},{"1":"NA","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Accruals

* Annual rebalance


```r
df = combine.sources(crsp, comp)
```

### Returns

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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"495","3":"330","4":"322","5":"386","6":"672","7":"4"},{"1":"2","2":"165","3":"122","4":"114","5":"99","6":"91","7":"1"},{"1":"3","2":"126","3":"94","4":"80","5":"66","6":"51","7":"1"},{"1":"4","2":"111","3":"80","4":"61","5":"52","6":"37","7":"1"},{"1":"5","2":"120","3":"70","4":"52","5":"42","6":"29","7":"NaN"},{"1":"NA","2":"1","3":"1","4":"1","5":"1","6":"1","7":"911"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.65","3":"1.22","4":"1.26","5":"1.47","6":"1.48"},{"1":"2","2":"0.93","3":"1.20","4":"1.20","5":"1.39","6":"1.20"},{"1":"3","2":"0.89","3":"1.10","4":"1.20","5":"1.20","6":"1.31"},{"1":"4","2":"0.98","3":"1.00","4":"1.11","5":"1.12","6":"1.16"},{"1":"5","2":"0.85","3":"0.87","4":"0.95","5":"0.93","6":"0.89"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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

### $Prior$ Quintiles


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
df %>% group_by(L1.ME.B, BMM.B, Date) %>% summarize(N=n()) %>% group_by(L1.ME.B, BMM.B) %>%
    dcast(L1.ME.B ~ BMM.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"436","3":"314","4":"347","5":"426","6":"704","7":"4"},{"1":"2","2":"159","3":"120","4":"120","5":"106","6":"77","7":"1"},{"1":"3","2":"121","3":"94","4":"83","5":"67","6":"43","7":"1"},{"1":"4","2":"104","3":"79","4":"63","5":"52","6":"32","7":"NaN"},{"1":"5","2":"108","3":"67","4":"51","5":"41","6":"24","7":"NaN"},{"1":"NA","2":"1","3":"1","4":"NaN","5":"NaN","6":"NaN","7":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


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
write.csv(returns, "C:/Data/Thesis/25_L1ME_BMM_Returns.csv")

df %>% group_by(L1.ME.B, BMM.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(L1.ME.B ~ BMM.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(L1.ME.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.76","3":"1.02","4":"1.10","5":"1.31","6":"1.46"},{"1":"2","2":"1.06","3":"1.08","4":"1.19","5":"1.40","6":"1.40"},{"1":"3","2":"0.92","3":"1.06","4":"1.15","5":"1.31","6":"1.39"},{"1":"4","2":"0.98","3":"0.95","4":"1.12","5":"1.17","6":"1.34"},{"1":"5","2":"0.85","3":"0.85","4":"0.90","5":"0.95","6":"1.07"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1072","3":"343","4":"302","5":"240","6":"258","7":"3"},{"1":"2","2":"151","3":"112","4":"114","5":"108","6":"102","7":"1"},{"1":"3","2":"79","3":"76","4":"88","5":"88","6":"86","7":"1"},{"1":"4","2":"52","3":"62","4":"70","5":"78","6":"78","7":"1"},{"1":"5","2":"37","3":"52","4":"58","5":"75","6":"90","7":"1"},{"1":"NA","2":"NaN","3":"1","4":"1","5":"1","6":"1","7":"908"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.94","3":"1.33","4":"1.34","5":"1.26","6":"1.33"},{"1":"2","2":"0.89","3":"1.19","4":"1.20","5":"1.23","6":"1.32"},{"1":"3","2":"0.91","3":"1.15","4":"1.11","5":"1.11","6":"1.28"},{"1":"4","2":"0.89","3":"0.99","4":"1.09","5":"1.12","6":"1.18"},{"1":"5","2":"0.75","3":"0.78","4":"0.83","5":"0.91","6":"0.93"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

![French Website 25 $Size-OP$ Returns](C:/Data/FrenchDartmouth/25_Size_OP.JPG)

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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"686","3":"301","4":"268","5":"288","6":"478","7":"8"},{"1":"2","2":"105","3":"90","4":"92","5":"110","6":"174","7":"3"},{"1":"3","2":"60","3":"66","4":"76","5":"86","6":"122","7":"2"},{"1":"4","2":"46","3":"61","4":"67","5":"75","6":"88","7":"2"},{"1":"5","2":"35","3":"59","4":"71","5":"75","6":"68","7":"2"},{"1":"NA","2":"1","3":"1","4":"1","5":"1","6":"1","7":"566"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.32","3":"1.39","4":"1.40","5":"1.26","6":"0.83"},{"1":"2","2":"1.28","3":"1.35","4":"1.26","5":"1.27","6":"0.90"},{"1":"3","2":"1.25","3":"1.20","4":"1.26","5":"1.15","6":"0.91"},{"1":"4","2":"1.02","3":"1.21","4":"1.08","5":"1.12","6":"0.95"},{"1":"5","2":"1.06","3":"0.95","4":"0.90","5":"0.80","6":"0.81"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"842","3":"392","4":"319","5":"334","6":"612","7":"20"},{"1":"2","2":"122","3":"111","4":"107","5":"115","6":"178","7":"3"},{"1":"3","2":"67","3":"80","4":"84","5":"88","6":"115","7":"2"},{"1":"4","2":"49","3":"69","4":"74","5":"77","6":"81","7":"2"},{"1":"5","2":"39","3":"63","4":"73","5":"74","6":"59","7":"3"},{"1":"NA","2":"1","3":"1","4":"1","5":"1","6":"1","7":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["4"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["5"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.46","3":"1.02","4":"1.24","5":"1.39","6":"1.64"},{"1":"2","2":"0.60","3":"1.01","4":"1.23","5":"1.32","6":"1.57"},{"1":"3","2":"0.70","3":"1.00","4":"1.08","5":"1.14","6":"1.50"},{"1":"4","2":"0.69","3":"0.96","4":"1.04","5":"1.15","6":"1.37"},{"1":"5","2":"0.65","3":"0.85","4":"0.76","5":"0.97","6":"1.18"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

![French Website 25 $ME-Prior$ Returns](C:/Data/FrenchDartmouth/25_L1ME_Prior.JPG)

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
df %>% group_by(Size.B, BM.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, BM.B) %>%
    dcast(Size.B ~ BM.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"983","3":"976","4":"1062","5":"4"},{"1":"2","2":"391","3":"305","4":"150","5":"1"},{"1":"NA","2":"2","3":"1","4":"1","5":"911"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
write.csv(returns, "C:/Data/Thesis/6_Size_BM_Returns.csv")

df %>% group_by(Size.B, BM.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ BM.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.89","3":"1.28","4":"1.36"},{"1":"2","2":"0.86","3":"0.93","4":"1.02"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
returns$HML = (returns$`1.3`+returns$`2.3`)/2 - (returns$`1.1`+returns$`2.1`)/2
returns$SMB.HML = ((returns$`1.3`+returns$`1.2`+returns$`1.1`)/3 -
               (returns$`2.3`+returns$`2.2`+returns$`2.1`)/3)
HML = returns %>% select(Date, HML, SMB.HML)
returns = returns %>% select(-c(Date, `NA`))
rbind(sapply(returns, mean), sapply(returns, se), sapply(returns, t.stat)) %>% round(2)
```

```
##       1.1  2.1  1.2  2.2  1.3  2.3  HML SMB.HML
## [1,] 0.88 0.85 1.25 0.91 1.34 1.00 0.31    0.24
## [2,] 0.27 0.18 0.21 0.17 0.21 0.18 0.11    0.12
## [3,] 3.31 4.66 5.94 5.42 6.35 5.58 2.76    1.99
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
df %>% group_by(L1.ME.B, BMM.B, Date) %>% summarize(N=n()) %>% group_by(L1.ME.B, BMM.B) %>%
    dcast(L1.ME.B ~ BMM.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"903","3":"1026","4":"1101","5":"4"},{"1":"2","2":"369","3":"307","4":"134","5":"NaN"},{"1":"NA","2":"1","3":"NaN","4":"NaN","5":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


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
write.csv(returns, "C:/Data/Thesis/6_L1ME_BMM_Returns.csv")

df %>% group_by(L1.ME.B, BMM.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(L1.ME.B ~ BMM.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(L1.ME.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.97","3":"1.17","4":"1.43"},{"1":"2","2":"0.86","3":"0.93","4":"1.14"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
returns$HMLm = (returns$`1.3`+returns$`2.3`)/2 - (returns$`1.1`+returns$`2.1`)/2
returns$SMB.HMLm = ((returns$`1.3`+returns$`1.2`+returns$`1.1`)/3 -
               (returns$`2.3`+returns$`2.2`+returns$`2.1`)/3)
HMLm = returns %>% select(Date, HMLm, SMB.HMLm)
returns = returns %>% select(-c(Date, `NA`))
rbind(sapply(returns, mean), sapply(returns, se), sapply(returns, t.stat)) %>% round(2)
```

```
##       1.1  2.1  1.2  2.2  1.3  2.3 HMLm SMB.HMLm
## [1,] 0.97 0.86 1.17 0.93 1.43 1.14 0.37     0.22
## [2,] 0.27 0.18 0.21 0.17 0.25 0.20 0.14     0.12
## [3,] 3.67 4.73 5.51 5.41 5.85 5.85 2.73     1.81
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
df %>% group_by(Size.B, OP.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, OP.B) %>%
    dcast(Size.B ~ OP.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1525","3":"908","4":"594","5":"3"},{"1":"2","2":"195","3":"338","4":"311","5":"2"},{"1":"NA","2":"1","3":"1","4":"1","5":"908"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
write.csv(returns, "C:/Data/Thesis/6_Size_OP_Returns.csv")

df %>% group_by(Size.B, OP.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ OP.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.96","3":"1.25","4":"1.29"},{"1":"2","2":"0.75","3":"0.88","4":"0.96"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
returns$RMWo = (returns$`1.3`+returns$`2.3`)/2 - (returns$`1.1`+returns$`2.1`)/2
returns$SMB.RMWo = ((returns$`1.3`+returns$`1.2`+returns$`1.1`)/3 -
               (returns$`2.3`+returns$`2.2`+returns$`2.1`)/3)
RMWo = returns %>% select(Date, RMWo, SMB.RMWo)
returns = returns %>% select(-c(Date, `NA`))
rbind(sapply(returns, mean), sapply(returns, se), sapply(returns, t.stat))  %>% round(2)
```

```
##       1.1  2.1  1.2  2.2  1.3  2.3 RMWo SMB.RMWo
## [1,] 0.94 0.73 1.23 0.86 1.26 0.94 0.27     0.30
## [2,] 0.25 0.20 0.20 0.17 0.23 0.17 0.09     0.11
## [3,] 3.70 3.74 6.00 5.03 5.54 5.56 3.09     2.63
```

![French Website 6 $Size-OP$ Returns](C:/Data/FrenchDartmouth/6_Size_OP.JPG)

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
df %>% group_by(Size.B, GP.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, GP.B) %>%
    dcast(Size.B ~ GP.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"979","3":"1051","4":"1083","5":"6"},{"1":"2","2":"287","3":"317","4":"267","5":"1"},{"1":"NA","2":"1","3":"1","4":"1","5":"930"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


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
write.csv(returns, "C:/Data/Thesis/6_Size_GP_Returns.csv")

df %>% group_by(Size.B, GP.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ GP.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.01","3":"1.14","4":"1.29"},{"1":"2","2":"0.82","3":"0.84","4":"1.00"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
returns$RMWg = (returns$`1.3`+returns$`2.3`)/2 - (returns$`1.1`+returns$`2.1`)/2
returns$SMB.RMWg = ((returns$`1.3`+returns$`1.2`+returns$`1.1`)/3 -
               (returns$`2.3`+returns$`2.2`+returns$`2.1`)/3)
RMWg = returns %>% select(Date, RMWg, SMB.RMWg)
returns = returns %>% select(-c(Date, `NA`))
rbind(sapply(returns, mean), sapply(returns, se), sapply(returns, t.stat))  %>% round(2)
```

```
##       1.1  2.1  1.2  2.2  1.3  2.3 RMWg SMB.RMWg
## [1,] 0.99 0.81 1.12 0.82 1.27 0.99 0.23     0.25
## [2,] 0.21 0.18 0.24 0.18 0.24 0.18 0.09     0.12
## [3,] 4.66 4.39 4.66 4.63 5.24 5.59 2.59     2.09
```

## $Size$ and $INV$

* Annual rebalance


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
df %>% group_by(Size.B, INV.B, Date) %>% summarize(N=n()) %>% group_by(Size.B, INV.B) %>%
    dcast(Size.B ~ INV.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1046","3":"813","4":"955","5":"10"},{"1":"2","2":"180","3":"346","4":"309","5":"4"},{"1":"NA","2":"1","3":"1","4":"1","5":"566"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
write.csv(returns, "C:/Data/Thesis/6_Size_INV_Returns.csv")

df %>% group_by(Size.B, INV.B, Date) %>% summarise(rp=sum(wt.ri, na.rm=TRUE)) %>%
    dcast(Size.B ~ INV.B, fun.aggregate=mean, value.var="rp", na.rm=TRUE) %>%
    select(-`NA`) %>% filter(!is.na(Size.B)) %>% round(digits=2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Size.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.30","3":"1.29","4":"0.98"},{"1":"2","2":"1.07","3":"0.92","4":"0.80"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
returns$CMA = (returns$`1.1`+returns$`2.1`)/2 - (returns$`1.3`+returns$`2.3`)/2
returns$SMB.CMA = ((returns$`1.3`+returns$`1.2`+returns$`1.1`)/3 -
               (returns$`2.3`+returns$`2.2`+returns$`2.1`)/3)
CMA = returns %>% select(Date, CMA, SMB.CMA)
returns = returns %>% select(-c(Date, `NA`))
rbind(sapply(returns, mean), sapply(returns, se), sapply(returns, t.stat)) %>% round(2)
```

```
##       1.1  2.1  1.2  2.2  1.3  2.3  CMA SMB.CMA
## [1,] 1.27 1.05 1.27 0.90 0.97 0.79 0.28    0.26
## [2,] 0.24 0.17 0.20 0.16 0.25 0.20 0.07    0.12
## [3,] 5.37 6.12 6.36 5.65 3.93 4.01 3.93    2.14
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
df %>% group_by(L1.ME.B, Prior.B, Date) %>% summarize(N=n()) %>% group_by(L1.ME.B, Prior.B) %>%
    dcast(L1.ME.B ~ Prior.B, value.var="N", fun.aggregate=mean, na.rm=TRUE) %>%
    round(0)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NA"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1292","3":"964","4":"1113","5":"23"},{"1":"2","2":"199","3":"371","4":"287","5":"3"},{"1":"NA","2":"1","3":"1","4":"1","5":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
{"columns":[{"label":["L1.ME.B"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["1"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["3"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.69","3":"1.18","4":"1.53"},{"1":"2","2":"0.76","3":"0.84","4":"1.13"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
returns$WML = (returns$`1.3`+returns$`2.3`)/2 - (returns$`1.1`+returns$`2.1`)/2
returns$SMB.WML = ((returns$`1.3`+returns$`1.2`+returns$`1.1`)/3 -
               (returns$`2.3`+returns$`2.2`+returns$`2.1`)/3)
WML = returns %>% select(Date, WML, SMB.WML)
returns = returns %>% select(-c(Date, `NA`))
rbind(sapply(returns, mean), sapply(returns, se), sapply(returns, t.stat))  %>% round(2)
```

```
##       1.1  2.1  1.2  2.2  1.3  2.3  WML SMB.WML
## [1,] 0.69 0.76 1.18 0.84 1.53 1.13 0.61    0.22
## [2,] 0.27 0.22 0.20 0.16 0.25 0.20 0.16    0.11
## [3,] 2.55 3.46 5.77 5.09 6.16 5.81 3.82    1.93
```

![French Website 6 $Size-Prior$ Returns](C:/Data/FrenchDartmouth/6_Size_Prior.JPG)

# Summarise Factors


```r
f = left_join(HML, HMLm, by="Date")
f = left_join(f, RMWo, by="Date")
f = left_join(f, RMWg, by="Date")
f = left_join(f, CMA, by="Date")
f = left_join(f, WML, by="Date")
f = left_join(f, rm, by="Date")
f$SMB = (f$SMB.HML + f$SMB.RMWo + f$SMB.CMA) / 3
```


```r
write.csv(f, "C:/Data/Thesis/factors.csv")
f = f %>% select(-c(Date, SMB.HML, SMB.HMLm, SMB.RMWo, SMB.RMWg, SMB.CMA, SMB.WML))
rbind(sapply(f, mean), sapply(f, se), sapply(f, t.stat))  %>% round(2)
```

```
##       HML HMLm RMWo RMWg  CMA  WML   Rm  SMB
## [1,] 0.31 0.37 0.27 0.23 0.28 0.61 0.50 0.26
## [2,] 0.11 0.14 0.09 0.09 0.07 0.16 0.17 0.12
## [3,] 2.76 2.73 3.09 2.59 3.93 3.82 2.84 2.28
```


```r
cor(f) %>% round(2) %>% as.data.frame
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["HML"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["HMLm"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["RMWo"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["RMWg"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["CMA"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["WML"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Rm"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["SMB"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"1.00","2":"0.79","3":"0.13","4":"-0.59","5":"0.65","6":"-0.22","7":"-0.33","8":"-0.12","_rn_":"HML"},{"1":"0.79","2":"1.00","3":"0.03","4":"-0.51","5":"0.48","6":"-0.68","7":"-0.16","8":"-0.06","_rn_":"HMLm"},{"1":"0.13","2":"0.03","3":"1.00","4":"0.24","5":"-0.14","6":"0.01","7":"-0.23","8":"-0.35","_rn_":"RMWo"},{"1":"-0.59","2":"-0.51","3":"0.24","4":"1.00","5":"-0.41","6":"0.13","7":"0.14","8":"0.16","_rn_":"RMWg"},{"1":"0.65","2":"0.48","3":"-0.14","4":"-0.41","5":"1.00","6":"-0.07","7":"-0.35","8":"-0.04","_rn_":"CMA"},{"1":"-0.22","2":"-0.68","3":"0.01","4":"0.13","5":"-0.07","6":"1.00","7":"-0.05","8":"0.04","_rn_":"WML"},{"1":"-0.33","2":"-0.16","3":"-0.23","4":"0.14","5":"-0.35","6":"-0.05","7":"1.00","8":"0.27","_rn_":"Rm"},{"1":"-0.12","2":"-0.06","3":"-0.35","4":"0.16","5":"-0.04","6":"0.04","7":"0.27","8":"1.00","_rn_":"SMB"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
summary(lm(SMB~Rm+HML+RMWo+CMA, data=f))
```

```
## 
## Call:
## lm(formula = SMB ~ Rm + HML + RMWo + CMA, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.7844  -1.6237  -0.1829   1.5452   9.6914 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.30777    0.11126   2.766  0.00584 ** 
## Rm           0.13284    0.02694   4.931 1.05e-06 ***
## HML         -0.02475    0.05136  -0.482  0.63012    
## RMWo        -0.40109    0.05324  -7.533 1.70e-13 ***
## CMA          0.01386    0.08295   0.167  0.86740    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.689 on 637 degrees of freedom
## Multiple R-squared:  0.1617,	Adjusted R-squared:  0.1564 
## F-statistic: 30.72 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HML~Rm+SMB+RMWo+CMA, data=f))
```

```
## 
## Call:
## lm(formula = HML ~ Rm + SMB + RMWo + CMA, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.1806 -1.3132  0.0842  1.3023  8.2127 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.02635    0.08632  -0.305   0.7602    
## Rm          -0.03481    0.02113  -1.648   0.0999 .  
## SMB         -0.01472    0.03055  -0.482   0.6301    
## RMWo         0.25957    0.04160   6.240    8e-10 ***
## CMA          1.01983    0.04960  20.560   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.074 on 637 degrees of freedom
## Multiple R-squared:  0.4677,	Adjusted R-squared:  0.4643 
## F-statistic: 139.9 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(RMWo~Rm+SMB+HML+CMA, data=f))
```

```
## 
## Call:
## lm(formula = RMWo ~ Rm + SMB + HML + CMA, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.2814  -1.0167  -0.0550   0.9572  13.2037 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.43698    0.07792   5.608 3.05e-08 ***
## Rm          -0.09764    0.01919  -5.088 4.76e-07 ***
## SMB         -0.20396    0.02707  -7.533 1.70e-13 ***
## HML          0.22190    0.03556   6.240 8.00e-10 ***
## CMA         -0.48663    0.05592  -8.702  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.918 on 637 degrees of freedom
## Multiple R-squared:  0.2331,	Adjusted R-squared:  0.2283 
## F-statistic:  48.4 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(CMA~Rm+SMB+HML+RMWo, data=f))
```

```
## 
## Call:
## lm(formula = CMA ~ Rm + SMB + HML + RMWo, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.8992 -0.8241 -0.0380  0.7211  5.0107 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.260830   0.052454   4.973 8.51e-07 ***
## Rm          -0.085870   0.012662  -6.781 2.72e-11 ***
## SMB          0.003161   0.018924   0.167    0.867    
## HML          0.391137   0.019024  20.560  < 2e-16 ***
## RMWo        -0.218322   0.025089  -8.702  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.284 on 637 degrees of freedom
## Multiple R-squared:  0.5038,	Adjusted R-squared:  0.5007 
## F-statistic: 161.7 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(RMWg~Rm+SMB+HML+CMA, data=f))
```

```
## 
## Call:
## lm(formula = RMWg ~ Rm + SMB + HML + CMA, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.1599 -1.0990 -0.0922  1.0735  7.3794 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.39922    0.07224   5.527 4.76e-08 ***
## Rm          -0.05660    0.01779  -3.182 0.001536 ** 
## SMB          0.09027    0.02510   3.596 0.000348 ***
## HML         -0.44208    0.03297 -13.408  < 2e-16 ***
## CMA         -0.10594    0.05185  -2.043 0.041436 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.778 on 637 degrees of freedom
## Multiple R-squared:  0.3715,	Adjusted R-squared:  0.3675 
## F-statistic: 94.12 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HML~Rm+SMB+RMWg+CMA, data=f))
```

```
## 
## Call:
## lm(formula = HML ~ Rm + SMB + RMWg + CMA, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.5899  -0.9739  -0.0326   0.9441  11.5949 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.27083    0.07774   3.484 0.000528 ***
## Rm          -0.07796    0.01878  -4.152 3.75e-05 ***
## SMB         -0.01105    0.02690  -0.411 0.681478    
## RMWg        -0.49790    0.03713 -13.408  < 2e-16 ***
## CMA          0.68669    0.04803  14.297  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.887 on 637 degrees of freedom
## Multiple R-squared:  0.5595,	Adjusted R-squared:  0.5567 
## F-statistic: 202.2 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+RMWo+CMA, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + RMWo + CMA, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -19.0098  -1.5739  -0.1471   1.3854  21.2073 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.04079    0.12579   0.324  0.74584    
## Rm           0.04014    0.03079   1.304  0.19272    
## SMB         -0.02872    0.04452  -0.645  0.51917    
## RMWo         0.16454    0.06062   2.714  0.00682 ** 
## CMA          0.98238    0.07228  13.591  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.022 on 637 degrees of freedom
## Multiple R-squared:  0.2452,	Adjusted R-squared:  0.2404 
## F-statistic: 51.73 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+RMWo+CMA+WML, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + RMWo + CMA + WML, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.5971 -1.1546  0.0676  1.1452  8.7403 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.43449    0.08543   5.086 4.82e-07 ***
## Rm          -0.01306    0.02071  -0.630 0.528603    
## SMB          0.02259    0.02988   0.756 0.449864    
## RMWo         0.15618    0.04061   3.846 0.000132 ***
## CMA          0.84545    0.04867  17.372  < 2e-16 ***
## WML         -0.56183    0.02007 -27.991  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.025 on 636 degrees of freedom
## Multiple R-squared:  0.6618,	Adjusted R-squared:  0.6591 
## F-statistic: 248.9 on 5 and 636 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+RMWg+CMA, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + RMWg + CMA, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.0359  -1.2403  -0.1273   1.0338  21.0813 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.322160   0.115811   2.782  0.00557 ** 
## Rm           0.005425   0.027972   0.194  0.84627    
## SMB          0.005270   0.040077   0.131  0.89543    
## RMWg        -0.575081   0.055318 -10.396  < 2e-16 ***
## CMA          0.634964   0.071552   8.874  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.811 on 637 degrees of freedom
## Multiple R-squared:  0.3472,	Adjusted R-squared:  0.3431 
## F-statistic:  84.7 on 4 and 637 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+RMWg+CMA+WML, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + RMWg + CMA + WML, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.9615  -0.8948  -0.0017   0.9045   8.7770 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.65633    0.07598   8.638   <2e-16 ***
## Rm          -0.04153    0.01822  -2.280   0.0229 *  
## SMB          0.04278    0.02603   1.643   0.1008    
## RMWg        -0.46519    0.03608 -12.893   <2e-16 ***
## CMA          0.56368    0.04649  12.126   <2e-16 ***
## WML         -0.53830    0.01817 -29.619   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.823 on 636 degrees of freedom
## Multiple R-squared:  0.7256,	Adjusted R-squared:  0.7235 
## F-statistic: 336.4 on 5 and 636 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB+HML+RMWg+CMA+WML, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB + HML + RMWg + CMA + WML, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4714 -0.5801 -0.0130  0.5565  7.4709 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.39406    0.04683   8.415 2.61e-16 ***
## Rm           0.02667    0.01126   2.369 0.018115 *  
## SMB          0.04591    0.01581   2.903 0.003822 ** 
## HML          0.78570    0.02381  32.998  < 2e-16 ***
## RMWg        -0.09026    0.02469  -3.656 0.000277 ***
## CMA          0.03470    0.03247   1.069 0.285565    
## WML         -0.45860    0.01130 -40.583  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.108 on 635 degrees of freedom
## Multiple R-squared:  0.8989,	Adjusted R-squared:  0.898 
## F-statistic: 941.4 on 6 and 635 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HML~Rm+SMB, data=f))
```

```
## 
## Call:
## lm(formula = HML ~ Rm + SMB, data = f)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.911  -1.585  -0.012   1.540  10.415 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.42090    0.10638   3.956 8.46e-05 ***
## Rm          -0.20788    0.02483  -8.372 3.58e-16 ***
## SMB         -0.03332    0.03750  -0.889    0.375    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.674 on 639 degrees of freedom
## Multiple R-squared:  0.1125,	Adjusted R-squared:  0.1097 
## F-statistic:  40.5 on 2 and 639 DF,  p-value: < 2.2e-16
```


```r
summary(lm(HMLm~Rm+SMB, data=f))
```

```
## 
## Call:
## lm(formula = HMLm ~ Rm + SMB, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.3962  -1.8219  -0.1775   1.4485  22.6779 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.44069    0.13638   3.231 0.001295 ** 
## Rm          -0.12060    0.03183  -3.789 0.000166 ***
## SMB         -0.02687    0.04807  -0.559 0.576320    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.428 on 639 degrees of freedom
## Multiple R-squared:  0.02607,	Adjusted R-squared:  0.02302 
## F-statistic: 8.551 on 2 and 639 DF,  p-value: 0.0002164
```


```r
summary(lm(HML~Rm+SMB+WML, data=f))
```

```
## 
## Call:
## lm(formula = HML ~ Rm + SMB + WML, data = f)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.5542  -1.4940   0.0336   1.5003  11.4728 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.52212    0.10430   5.006 7.20e-07 ***
## Rm          -0.21726    0.02411  -9.010  < 2e-16 ***
## SMB         -0.01926    0.03641  -0.529    0.597    
## WML         -0.16568    0.02556  -6.481 1.82e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.592 on 638 degrees of freedom
## Multiple R-squared:  0.1673,	Adjusted R-squared:  0.1634 
## F-statistic: 42.73 on 3 and 638 DF,  p-value: < 2.2e-16
```

Asness and Frazzini (2013) Exhibit 6 Regression 1


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
## -8.8646 -0.6489  0.0308  0.6028  6.4212 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.23493    0.04997  -4.702 3.17e-06 ***
## Rm          -0.07212    0.01138  -6.338 4.42e-10 ***
## SMB         -0.04162    0.01661  -2.506   0.0125 *  
## HMLm         0.93999    0.01906  49.322  < 2e-16 ***
## WML          0.39544    0.01629  24.280  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.182 on 637 degrees of freedom
## Multiple R-squared:  0.8272,	Adjusted R-squared:  0.8261 
## F-statistic: 762.4 on 4 and 637 DF,  p-value: < 2.2e-16
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
##     Min      1Q  Median      3Q     Max 
## -11.261  -1.323   0.010   1.337  13.281 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.80537    0.09878   8.153 1.88e-15 ***
## Rm          -0.15440    0.02284  -6.761 3.09e-11 ***
## SMB          0.02379    0.03448   0.690    0.491    
## WML         -0.59694    0.02421 -24.658  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.455 on 638 degrees of freedom
## Multiple R-squared:  0.5013,	Adjusted R-squared:  0.499 
## F-statistic: 213.8 on 3 and 638 DF,  p-value: < 2.2e-16
```

Asness and Frazzini (2013) Exhibit 6 Regression 4


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
## -5.4729 -0.6059 -0.0213  0.6019  7.3425 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.36519    0.04591   7.955 8.23e-15 ***
## Rm           0.02876    0.01105   2.602  0.00948 ** 
## SMB          0.04003    0.01572   2.545  0.01115 *  
## HML          0.84308    0.01709  49.322  < 2e-16 ***
## WML         -0.45726    0.01139 -40.130  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.119 on 637 degrees of freedom
## Multiple R-squared:  0.8965,	Adjusted R-squared:  0.8959 
## F-statistic:  1380 on 4 and 637 DF,  p-value: < 2.2e-16
```

