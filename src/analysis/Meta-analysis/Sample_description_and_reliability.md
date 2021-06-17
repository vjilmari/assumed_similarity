---
title: "Sample description"
output: 
  html_document: 
    toc: yes
    keep_md: yes
    number_sections: yes
---



# Preparations

## Packages


```r
library(rio)
library(psych)
library(dplyr)
```

## Data


```r
Dan<-
  import("../../../data/processed/Danish/Dan_fdat.xlsx")
Brit<-
  import("../../../data/processed/British/Brit_fdat.xlsx")
Chi1<-
  import("../../../data/processed/Chinese 1/Chi1_fdat.xlsx")
Chi2<-
  import("../../../data/processed/Chinese 2/Chi2_fdat.xlsx")
Chi3comb<-
  import("../../../data/processed/Chinese 3/Chi3comb_fdat.xlsx")
```

# Descriptives

## Danish


```r
nrow(Dan)
```

```
## [1] 92
```

```r
round(100*table(Dan$sex,useNA="always")/nrow(Dan),1)
```

```
## 
##    1    2 <NA> 
## 15.2 84.8  0.0
```

```r
describe(Dan$age,fast=T)
```

```
##    vars  n  mean    sd min max range   se
## X1    1 92 15.71 10.28   5  60    55 1.07
```

```r
Dan$real.age<-Dan$age+17
describe(Dan$real.age,fast=T)
```

```
##    vars  n  mean    sd min max range   se
## X1    1 92 32.71 10.28  22  77    55 1.07
```

```r
describe(Dan$length,fast=T)
```

```
##    vars  n  mean    sd min max range   se
## X1    1 92 39.37 39.19   1 296   295 4.09
```

## British


```r
nrow(Brit)
```

```
## [1] 292
```

```r
table(Brit$sex,useNA="always")/nrow(Brit)
```

```
## 
##     Female       Male       <NA> 
## 0.90068493 0.09931507 0.00000000
```

```r
describe(Brit$age,fast=T)
```

```
##    vars   n  mean    sd min max range   se
## X1    1 292 26.59 10.09  18  65    47 0.59
```

```r
round(100*table(Brit$rela_status,useNA="always")/nrow(Brit),1)
```

```
## 
##                   Engaged In a domestic partnership         In a relationship 
##                       5.1                       6.2                      74.7 
##       Married/Civil Union                      <NA> 
##                      14.0                       0.0
```

```r
describe(Brit$length,fast=T)
```

```
##    vars   n  mean    sd  min max  range   se
## X1    1 292 54.33 80.54 0.06 547 546.94 4.71
```
## Chinese 1


```r
nrow(Chi1)
```

```
## [1] 141
```

```r
table(Chi1$sex,useNA="always")/nrow(Chi1)
```

```
## 
##        1        2     <NA> 
## 0.177305 0.822695 0.000000
```

```r
describe(Chi1$age,fast=T)
```

```
##    vars   n  mean   sd min max range   se
## X1    1 141 23.22 3.29  18  40    22 0.28
```

```r
round(100*table(Chi1$relation_type,useNA="always")/nrow(Chi1),1)
```

```
## 
##    2    3    4    5 <NA> 
##  4.3 87.2  4.3  4.3  0.0
```

```r
describe(Chi1$length,fast=T)
```

```
##    vars   n mean    sd min max range   se
## X1    1 141 34.6 56.85   1 420   419 4.79
```

## Chinese 2


```r
nrow(Chi2)
```

```
## [1] 356
```

```r
table(Chi2$sex,useNA="always")/nrow(Chi2)
```

```
## 
##         1         2      <NA> 
## 0.2780899 0.7219101 0.0000000
```

```r
describe(Chi2$age,fast=T)
```

```
##    vars   n  mean   sd min max range   se
## X1    1 356 23.33 4.41  18  45    27 0.23
```

```r
round(100*table(Chi2$relation_type,
                useNA="always")/nrow(Chi2),1)
```

```
## 
##    2    3    4    5 <NA> 
##  6.2 80.9  2.5 10.4  0.0
```

```r
describe(Chi2$length,fast=T)
```

```
##    vars   n  mean    sd min max range  se
## X1    1 356 29.75 34.05   1 228   227 1.8
```


## Chinese couple sample


```r
nrow(Chi3comb)
```

```
## [1] 315
```

```r
table(Chi3comb$sex,useNA="always")/nrow(Chi3comb)
```

```
## 
##         1         2      <NA> 
## 0.4952381 0.5047619 0.0000000
```

```r
describe(Chi3comb$age,fast=T)
```

```
##    vars   n  mean   sd min max range   se
## X1    1 315 22.07 2.65  18  30    12 0.15
```

```r
#round(100*table(Chi3comb$relation_type,
#                useNA="always")/nrow(Chi3comb),1)

describe(Chi3comb$length,fast=T)
```

```
##    vars   n  mean    sd min max range   se
## X1    1 315 21.67 18.51   1  87    86 1.04
```

## Total


```r
Dan$age<-Dan$real.age

total.dat<-
  rbind(Dan[,c("sex","age","length")],
        Brit[,c("sex","age","length")],
        Chi1[,c("sex","age","length")],
        Chi2[,c("sex","age","length")],
        Chi3comb[,c("sex","age","length")])


nrow(total.dat)
```

```
## [1] 1196
```

```r
round(100*table(total.dat$sex,useNA="always")/
        nrow(total.dat),1)
```

```
## 
##      1      2 Female   Male   <NA> 
##   24.6   51.0   22.0    2.4    0.0
```

```r
describe(total.dat$age,fast=T)
```

```
##    vars    n mean   sd min max range  se
## X1    1 1196 24.5 7.08  18  77    59 0.2
```

```r
describe(total.dat$length,fast=T)
```

```
##    vars    n  mean    sd  min max  range   se
## X1    1 1196 34.94 51.55 0.06 547 546.94 1.49
```

# Scale reliability

## Variable names in each scale


```r
OP.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=01, to =60,by=6))<10,
                paste0("0",c(seq(from=01, to =60,by=6))),
                c(seq(from=01, to =60,by=6))))

OP.sr.vars
```

```
##  [1] "hex01" "hex07" "hex13" "hex19" "hex25" "hex31" "hex37" "hex43" "hex49"
## [10] "hex55"
```

```r
OP.sr.revs<-c(1,4,6,9,10)

OP.pr.vars1<-paste0("cp",OP.sr.vars)
OP.pr.vars2<-paste0("p",OP.sr.vars)

CO.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=02, to =60,by=6))<10,
                paste0("0",c(seq(from=02, to =60,by=6))),
                c(seq(from=02, to =60,by=6))))

CO.sr.vars
```

```
##  [1] "hex02" "hex08" "hex14" "hex20" "hex26" "hex32" "hex38" "hex44" "hex50"
## [10] "hex56"
```

```r
CO.sr.revs<-c(3,4,5,6,8,10)

CO.pr.vars1<-paste0("cp",CO.sr.vars)
CO.pr.vars2<-paste0("p",CO.sr.vars)

AG.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=03, to =60,by=6))<10,
                paste0("0",c(seq(from=03, to =60,by=6))),
                c(seq(from=03, to =60,by=6))))

AG.sr.vars
```

```
##  [1] "hex03" "hex09" "hex15" "hex21" "hex27" "hex33" "hex39" "hex45" "hex51"
## [10] "hex57"
```

```r
AG.sr.revs<-c(2,3,4,10)

AG.pr.vars1<-paste0("cp",AG.sr.vars)
AG.pr.vars2<-paste0("p",AG.sr.vars)

EX.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=04, to =60,by=6))<10,
                paste0("0",c(seq(from=04, to =60,by=6))),
                c(seq(from=04, to =60,by=6))))

EX.sr.vars
```

```
##  [1] "hex04" "hex10" "hex16" "hex22" "hex28" "hex34" "hex40" "hex46" "hex52"
## [10] "hex58"
```

```r
EX.sr.revs<-c(2,5,8,9)

EX.pr.vars1<-paste0("cp",EX.sr.vars)
EX.pr.vars2<-paste0("p",EX.sr.vars)

EM.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=05, to =60,by=6))<10,
                paste0("0",c(seq(from=05, to =60,by=6))),
                c(seq(from=05, to =60,by=6))))

EM.sr.vars
```

```
##  [1] "hex05" "hex11" "hex17" "hex23" "hex29" "hex35" "hex41" "hex47" "hex53"
## [10] "hex59"
```

```r
EM.sr.revs<-c(6,7,9,10)

EM.pr.vars1<-paste0("cp",EM.sr.vars)
EM.pr.vars2<-paste0("p",EM.sr.vars)

HH.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=06, to =60,by=6))<10,
                paste0("0",c(seq(from=06, to =60,by=6))),
                c(seq(from=06, to =60,by=6))))

HH.sr.vars
```

```
##  [1] "hex06" "hex12" "hex18" "hex24" "hex30" "hex36" "hex42" "hex48" "hex54"
## [10] "hex60"
```

```r
HH.sr.revs<-c(2,4,5,7,8,10)

HH.pr.vars1<-paste0("cp",HH.sr.vars)
HH.pr.vars2<-paste0("p",HH.sr.vars)
```

## Self-ratings

### HH


```r
round(alpha(Dan[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```

```r
round(alpha(Brit[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.78
```

```r
round(alpha(Chi1[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.74
```

```r
round(alpha(Chi2[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.7
```

```r
round(alpha(Chi3comb[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.67
```

### EM


```r
round(alpha(Dan[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.8
```

```r
round(alpha(Brit[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```

```r
round(alpha(Chi1[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.7
```

```r
round(alpha(Chi2[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.69
```

```r
round(alpha(Chi3comb[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.69
```

### EX


```r
round(alpha(Dan[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.84
```

```r
round(alpha(Brit[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.83
```

```r
round(alpha(Chi1[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.76
```

```r
round(alpha(Chi2[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```

```r
round(alpha(Chi3comb[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.7
```

### AG


```r
round(alpha(Dan[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.73
```

```r
round(alpha(Brit[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.76
```

```r
round(alpha(Chi1[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.75
```

```r
round(alpha(Chi2[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.67
```

```r
round(alpha(Chi3comb[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.6
```

### CO


```r
round(alpha(Dan[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.8
```

```r
round(alpha(Brit[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.78
```

```r
round(alpha(Chi1[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.73
```

```r
round(alpha(Chi2[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.74
```

```r
round(alpha(Chi3comb[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.68
```

### OP


```r
round(alpha(Dan[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.71
```

```r
round(alpha(Brit[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```

```r
round(alpha(Chi1[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.67
```

```r
round(alpha(Chi2[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.68
```

```r
round(alpha(Chi3comb[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.63
```

## Partner ratings


### HH


```r
round(alpha(Dan[,HH.pr.vars1],
      keys = HH.pr.vars1[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```

```r
round(alpha(Brit[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.84
```

```r
round(alpha(Chi1[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.78
```

```r
round(alpha(Chi2[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.79
```

```r
round(alpha(Chi3comb[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.74
```


### EM


```r
round(alpha(Dan[,EM.pr.vars1],
      keys = EM.pr.vars1[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.79
```

```r
round(alpha(Brit[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.84
```

```r
round(alpha(Chi1[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.73
```

```r
round(alpha(Chi2[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.76
```

```r
round(alpha(Chi3comb[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.81
```


### EX


```r
round(alpha(Dan[,EX.pr.vars1],
      keys = EX.pr.vars1[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.79
```

```r
round(alpha(Brit[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.83
```

```r
round(alpha(Chi1[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.7
```

```r
round(alpha(Chi2[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.71
```

```r
round(alpha(Chi3comb[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.73
```


### AG


```r
round(alpha(Dan[,AG.pr.vars1],
      keys = AG.pr.vars1[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.84
```

```r
round(alpha(Brit[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.85
```

```r
round(alpha(Chi1[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.82
```

```r
round(alpha(Chi2[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.81
```

```r
round(alpha(Chi3comb[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```



### CO


```r
round(alpha(Dan[,CO.pr.vars1],
      keys = CO.pr.vars1[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.83
```

```r
round(alpha(Brit[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.85
```

```r
round(alpha(Chi1[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.83
```

```r
round(alpha(Chi2[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.81
```

```r
round(alpha(Chi3comb[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```


### OP


```r
round(alpha(Dan[,OP.pr.vars1],
      keys = OP.pr.vars1[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.76
```

```r
round(alpha(Brit[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.83
```

```r
round(alpha(Chi1[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.79
```

```r
round(alpha(Chi2[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.73
```

```r
round(alpha(Chi3comb[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)
```

```
## [1] 0.77
```

## Relationship satisfaction

### British


```r
satis.vars<-paste0("satis0",1:7)
satis.revs<-c("satis04","satis07")

round(alpha(Brit[,satis.vars],
      keys = satis.revs)$total$raw_alpha,2)
```

```
## [1] 0.9
```

### Chinese 1


```r
satis.vars<-paste0("satis0",1:7)
satis.revs<-c("satis04","satis07")

round(alpha(Chi1[,satis.vars],
      keys = satis.revs)$total$raw_alpha,2)
```

```
## [1] 0.87
```


### Chinese 2


```r
satis.vars<-paste0("satis0",1:5)
round(alpha(Chi2[,satis.vars])$total$raw_alpha,2)
```

```
## [1] 0.9
```

### Chinese 3a


```r
Chi3a<-
  import("../../../data/processed/Chinese 3/Chi3a_fdat_relscales.xlsx")

satis.vars<-paste0("satis0",1:5)
round(alpha(Chi3a[,satis.vars])$total$raw_alpha,2)
```

```
## [1] 0.89
```

### Chinese 3b


```r
Chi3b<-
  import("../../../data/processed/Chinese 3/Chi3b_fdat_relscales.xlsx")

satis.vars<-paste0("satis",1:7)
satis.keys<-c("satis3","satis4")
round(alpha(Chi3b[,satis.vars],
            keys=satis.keys)$total$raw_alpha,2)
```

```
## [1] 0.7
```


## Relationship commitment

### British


```r
commit.vars<-paste0("commit0",1:7)
commit.revs<-c("commit03","commit04")

round(alpha(Brit[,commit.vars],
      keys = commit.revs)$total$raw_alpha,2)
```

```
## [1] 0.89
```

### Chinese 1


```r
commit.vars<-paste0("comit0",1:7)
commit.revs<-c("comit03","comit04")

round(alpha(Chi1[,commit.vars],
      keys = commit.revs)$total$raw_alpha,2)
```

```
## [1] 0.88
```


### Chinese 2


```r
commit.vars<-paste0("commit0",1:7)
commit.revs<-c("commit03","commit04")


round(alpha(Chi2[,commit.vars],
      keys = commit.revs)$total$raw_alpha,2)
```

```
## [1] 0.8
```

### Chinese 3a


```r
commit.vars<-paste0("commit0",1:7)

round(alpha(Chi3a[,commit.vars])$total$raw_alpha,2)
```

```
## [1] 0.83
```

### Chi3b


```r
commit.vars<-paste0("commit",1:7)
#recode -99 to NA
Chi3b[,commit.vars][Chi3b[,commit.vars]==-99]<-NA

commit.vars<-paste0("commit",1:7)
commit.keys<-c("commit4","commit7")
round(alpha(Chi3b[,commit.vars],
            keys=commit.keys)$total$raw_alpha,2)
```

```
## [1] 0.8
```


# Session information


```r
s<-sessionInfo()
print(s,locale=F)
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19041)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] dplyr_1.0.2 psych_2.0.9 rio_0.5.16 
## 
## loaded via a namespace (and not attached):
##  [1] zip_2.1.1         Rcpp_1.0.5        pillar_1.5.1      compiler_4.0.3   
##  [5] cellranger_1.1.0  forcats_0.5.0     tools_4.0.3       digest_0.6.25    
##  [9] evaluate_0.14     lifecycle_1.0.0   tibble_3.0.3      nlme_3.1-149     
## [13] lattice_0.20-41   pkgconfig_2.0.3   rlang_0.4.10      openxlsx_4.2.2   
## [17] curl_4.3          yaml_2.2.1        parallel_4.0.3    haven_2.3.1      
## [21] xfun_0.19         stringr_1.4.0     knitr_1.30        generics_0.0.2   
## [25] vctrs_0.3.7       hms_0.5.3         tidyselect_1.1.0  grid_4.0.3       
## [29] glue_1.4.2        data.table_1.14.0 R6_2.4.1          fansi_0.4.1      
## [33] readxl_1.3.1      foreign_0.8-80    rmarkdown_2.5     purrr_0.3.4      
## [37] magrittr_1.5      ellipsis_0.3.1    htmltools_0.5.0   mnormt_2.0.2     
## [41] utf8_1.2.1        stringi_1.5.3     tmvnsim_1.0-2     crayon_1.4.1
```
