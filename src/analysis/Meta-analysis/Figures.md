---
title: "Figures"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Packages


```r
library(meta)
```

```
## Loading 'meta' package (version 4.15-1).
## Type 'help(meta)' for a brief overview.
```

```r
#library(devtools)
#install.packages("devtools")
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
```

```
## Registered S3 methods overwritten by 'tibble':
##   method     from  
##   format.tbl pillar
##   print.tbl  pillar
```

```
## Extensive documentation for the dmetar package can be found at: 
##  www.bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(rio)
```

# Figure 1

## Read effects


```r
HEXACO.Dist<-
  import("../../../data/processed/Meta-analysis/HEXACO.Dist.xlsx")

#rename British to British Isles
HEXACO.Dist$sample[HEXACO.Dist$sample=="British"]<-"British Isles"
```


## Add identifiers


```r
HEXACO.Dist$sample.HEXACO<-paste0(HEXACO.Dist$sample," ",HEXACO.Dist$domain)
HEXACO.Dist$domain.name<-case_when(
  HEXACO.Dist$domain=="HH"~"Honesty-Humility",
  HEXACO.Dist$domain=="EM"~"Emotionality",
  HEXACO.Dist$domain=="EX"~"Extraversion",
  HEXACO.Dist$domain=="AG"~"Agreeableness",
  HEXACO.Dist$domain=="CO"~"Conscientiousness",
  HEXACO.Dist$domain=="OE"~"Openness to Experience")

HEXACO.Dist
```

```
##    domain SRc.zc.trend         SE        df    t.ratio      p.value
## 1      AG -0.007680249 0.02094441 1724.4336 -0.3666968 7.138902e-01
## 2      AG  0.137454097 0.02902296  739.1222  4.7360473 2.612987e-06
## 3      AG  0.115028818 0.01782365 2477.4853  6.4537171 1.308423e-10
## 4      AG  0.100454395 0.01860622 2737.2051  5.3989672 7.277379e-08
## 5      AG  0.067042888 0.03414813  647.0618  1.9632962 5.003965e-02
## 6      CO -0.006614990 0.02122840 1807.3170 -0.3116105 7.553725e-01
## 7      CO  0.019968290 0.03037824  869.5941  0.6573222 5.111477e-01
## 8      CO  0.091214616 0.01824837 2676.1572  4.9985084 6.150363e-07
## 9      CO  0.009800431 0.01869551 2755.6042  0.5242129 6.001726e-01
## 10     CO -0.064349179 0.03534270  708.4848 -1.8207207 6.907102e-02
## 11     EM -0.023281061 0.02085042 1679.2053 -1.1165750 2.643358e-01
## 12     EM  0.034872483 0.02987868  809.3970  1.1671358 2.434992e-01
## 13     EM  0.019155052 0.01772078 2401.4799  1.0809375 2.798334e-01
## 14     EM  0.009943412 0.01758900 2207.1940  0.5653198 5.719137e-01
## 15     EM  0.009726220 0.03384490  617.0192  0.2873762 7.739208e-01
## 16     EX  0.036001852 0.02007011 1472.3102  1.7938043 7.304954e-02
## 17     EX  0.097596372 0.02912852  745.8255  3.3505436 8.471233e-04
## 18     EX  0.063766120 0.01782460 2458.5423  3.5774214 3.537032e-04
## 19     EX  0.092269327 0.01865088 2728.2170  4.9471831 7.986808e-07
## 20     EX  0.109751537 0.03433969  632.7955  3.1960551 1.462552e-03
## 21     HH  0.304440030 0.02026497 1532.2309 15.0229678 1.084194e-47
## 22     HH  0.287466439 0.02815926  653.4009 10.2085951 8.394194e-23
## 23     HH  0.302905536 0.01682765 2016.1783 18.0004668 2.697426e-67
## 24     HH  0.281014010 0.01702223 1952.9289 16.5086511 2.051176e-57
## 25     HH  0.318631904 0.03299672  559.5606  9.6564706 1.651352e-20
## 26     OE  0.210026687 0.02123324 1801.3902  9.8914105 1.678739e-22
## 27     OE  0.190288798 0.02796946  642.4294  6.8034484 2.342945e-11
## 28     OE  0.197946349 0.01646716 1866.8453 12.0206762 4.104128e-32
## 29     OE  0.190111973 0.01678702 1888.4648 11.3249419 8.197652e-29
## 30     OE  0.208207981 0.03497105  685.7775  5.9537244 4.184305e-09
##           sample country    sample.HEXACO            domain.name
## 1  British Isles Britain British Isles AG          Agreeableness
## 2      Chinese 1   China     Chinese 1 AG          Agreeableness
## 3      Chinese 2   China     Chinese 2 AG          Agreeableness
## 4      Chinese 3   China     Chinese 3 AG          Agreeableness
## 5         Danish Denmark        Danish AG          Agreeableness
## 6  British Isles Britain British Isles CO      Conscientiousness
## 7      Chinese 1   China     Chinese 1 CO      Conscientiousness
## 8      Chinese 2   China     Chinese 2 CO      Conscientiousness
## 9      Chinese 3   China     Chinese 3 CO      Conscientiousness
## 10        Danish Denmark        Danish CO      Conscientiousness
## 11 British Isles Britain British Isles EM           Emotionality
## 12     Chinese 1   China     Chinese 1 EM           Emotionality
## 13     Chinese 2   China     Chinese 2 EM           Emotionality
## 14     Chinese 3   China     Chinese 3 EM           Emotionality
## 15        Danish Denmark        Danish EM           Emotionality
## 16 British Isles Britain British Isles EX           Extraversion
## 17     Chinese 1   China     Chinese 1 EX           Extraversion
## 18     Chinese 2   China     Chinese 2 EX           Extraversion
## 19     Chinese 3   China     Chinese 3 EX           Extraversion
## 20        Danish Denmark        Danish EX           Extraversion
## 21 British Isles Britain British Isles HH       Honesty-Humility
## 22     Chinese 1   China     Chinese 1 HH       Honesty-Humility
## 23     Chinese 2   China     Chinese 2 HH       Honesty-Humility
## 24     Chinese 3   China     Chinese 3 HH       Honesty-Humility
## 25        Danish Denmark        Danish HH       Honesty-Humility
## 26 British Isles Britain British Isles OE Openness to Experience
## 27     Chinese 1   China     Chinese 1 OE Openness to Experience
## 28     Chinese 2   China     Chinese 2 OE Openness to Experience
## 29     Chinese 3   China     Chinese 3 OE Openness to Experience
## 30        Danish Denmark        Danish OE Openness to Experience
```
## Analysis model


```r
HEXACO.Dist.m <- metagen(TE = SRc.zc.trend,
                  seTE = SE,
                  data = HEXACO.Dist,
                  #subset=...1=="SRc.zc",
                  studlab = sample,
                  #id=domain,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  prediction=TRUE)

#sub-group analysis by domain
HEXACO.Dist.m.sub<-subgroup.analysis.mixed.effects(x = HEXACO.Dist.m,
                                subgroups = HEXACO.Dist$domain.name)
```

## Plot the estimates


```r
grDevices::png(file = "../../../output/Meta-analysis/Figure1.png",
               family="sans",units = "mm",
               height=297,width=210,res=300)

meta::forest(HEXACO.Dist.m.sub,
             prediction=F,
             overall=F,
             xlim=c(-.15,.40),
             zero.pval=F,
             digits.pval.Q=3,
             #addrow.overall=T,
             overall.hetstat=F,
             print.subgroup.label=T,
             bysort=F,
             smlab = "Distinctive assumed similarity",
             addrow.subgroups=T,
             col.diamond.random="black")

graphics.off()
```


# Figure 2

## Read effects


```r
HEXACO.Dist.by.satis<-
  import("../../../data/processed/Meta-analysis/HEXACO.Dist.by.satis.xlsx")

HEXACO.Dist.by.commit<-
  import("../../../data/processed/Meta-analysis/HEXACO.Dist.by.commit.xlsx")


#rename British to British Isles
HEXACO.Dist.by.satis$sample[HEXACO.Dist.by.satis$sample=="British"]<-
  "British Isles"

HEXACO.Dist.by.commit$sample[HEXACO.Dist.by.commit$sample=="British"]<-
  "British Isles"
```


## Add identifiers


```r
HEXACO.Dist.by.satis$sample.HEXACO<-paste0(HEXACO.Dist.by.satis$sample," ",HEXACO.Dist.by.satis$domain)
HEXACO.Dist.by.satis$domain.name<-case_when(
  HEXACO.Dist.by.satis$domain=="HH"~"Honesty-Humility",
  HEXACO.Dist.by.satis$domain=="EM"~"Emotionality",
  HEXACO.Dist.by.satis$domain=="EX"~"Extraversion",
  HEXACO.Dist.by.satis$domain=="AG"~"Agreeableness",
  HEXACO.Dist.by.satis$domain=="CO"~"Conscientiousness",
  HEXACO.Dist.by.satis$domain=="OE"~"Openness to Experience")

HEXACO.Dist.by.satis
```

```
##    domain satis.z_SRc.zc.trend         SE        df     lower.CL    upper.CL
## 1      AG         0.0616206949 0.02032495 1670.3677  0.021755636 0.101485754
## 2      AG         0.1354625389 0.02728944  813.3535  0.081896519 0.189028559
## 3      AG        -0.0275016265 0.01761987 2487.3654 -0.062052744 0.007049491
## 4      AG         0.1114380583 0.01762190 2286.1464  0.076881470 0.145994646
## 5      CO         0.0036156282 0.02122663 1985.2119 -0.038013187 0.045244443
## 6      CO         0.0840467879 0.02887721 1003.5795  0.027380163 0.140713413
## 7      CO         0.0296774170 0.01751556 2415.5299 -0.004669653 0.064024487
## 8      CO         0.0135795747 0.01816011 2537.2431 -0.022030566 0.049189715
## 9      EM         0.0381214253 0.02051980 1734.4417 -0.002124735 0.078367586
## 10     EM         0.0002244188 0.02728704  804.3313 -0.053337805 0.053786643
## 11     EM         0.0109326974 0.01752933 2458.3340 -0.023441074 0.045306469
## 12     EM         0.0383474648 0.01733402 2127.4573  0.004354069 0.072340860
## 13     EX         0.0845279983 0.01992886 1556.3594  0.045437759 0.123618238
## 14     EX         0.0509979588 0.02676366  760.0952 -0.001541512 0.103537430
## 15     EX         0.0159225716 0.01777670 2568.5197 -0.018935556 0.050780699
## 16     EX         0.0140080172 0.01812925 2456.0718 -0.021542182 0.049558217
## 17     HH         0.0258496411 0.01970972 1523.3917 -0.012811407 0.064510689
## 18     HH         0.0897596852 0.02542654  621.9764  0.039827409 0.139691961
## 19     HH         0.0196087048 0.01616182 1824.8873 -0.012088906 0.051306315
## 20     HH         0.0526997132 0.01668882 1852.3438  0.019968838 0.085430589
## 21     OE         0.0456172244 0.02114458 1914.1844  0.004148394 0.087086055
## 22     OE         0.1115254094 0.02546432  625.5896  0.061519522 0.161531297
## 23     OE         0.0660104397 0.01627521 1885.0592  0.034091113 0.097929766
## 24     OE         0.0103979755 0.01643381 1778.0727 -0.021833644 0.042629595
##        t.ratio      p.value        sample country    sample.HEXACO
## 1   3.03177578 2.468596e-03 British Isles Britain British Isles AG
## 2   4.96391877 8.418037e-07     Chinese 1   China     Chinese 1 AG
## 3  -1.56083052 1.186910e-01     Chinese 2   China     Chinese 2 AG
## 4   6.32383837 3.059081e-10     Chinese 3   China     Chinese 3 AG
## 5   0.17033452 8.647644e-01 British Isles Britain British Isles CO
## 6   2.91048890 3.688376e-03     Chinese 1   China     Chinese 1 CO
## 7   1.69434625 9.032841e-02     Chinese 2   China     Chinese 2 CO
## 8   0.74776959 4.546684e-01     Chinese 3   China     Chinese 3 CO
## 9   1.85778706 6.336861e-02 British Isles Britain British Isles EM
## 10  0.00822437 9.934400e-01     Chinese 1   China     Chinese 1 EM
## 11  0.62368041 5.328954e-01     Chinese 2   China     Chinese 2 EM
## 12  2.21226604 2.705415e-02     Chinese 3   China     Chinese 3 EM
## 13  4.24148773 2.351205e-05 British Isles Britain British Isles EX
## 14  1.90549269 5.709311e-02     Chinese 1   China     Chinese 1 EX
## 15  0.89569871 3.704976e-01     Chinese 2   China     Chinese 2 EX
## 16  0.77267487 4.397892e-01     Chinese 3   China     Chinese 3 EX
## 17  1.31151775 1.898805e-01 British Isles Britain British Isles HH
## 18  3.53015668 4.459979e-04     Chinese 1   China     Chinese 1 HH
## 19  1.21327323 2.251824e-01     Chinese 2   China     Chinese 2 HH
## 20  3.15778524 1.615341e-03     Chinese 3   China     Chinese 3 HH
## 21  2.15739594 3.109892e-02 British Isles Britain British Isles OE
## 22  4.37967432 1.393287e-05     Chinese 1   China     Chinese 1 OE
## 23  4.05588785 5.197923e-05     Chinese 2   China     Chinese 2 OE
## 24  0.63271843 5.269989e-01     Chinese 3   China     Chinese 3 OE
##               domain.name
## 1           Agreeableness
## 2           Agreeableness
## 3           Agreeableness
## 4           Agreeableness
## 5       Conscientiousness
## 6       Conscientiousness
## 7       Conscientiousness
## 8       Conscientiousness
## 9            Emotionality
## 10           Emotionality
## 11           Emotionality
## 12           Emotionality
## 13           Extraversion
## 14           Extraversion
## 15           Extraversion
## 16           Extraversion
## 17       Honesty-Humility
## 18       Honesty-Humility
## 19       Honesty-Humility
## 20       Honesty-Humility
## 21 Openness to Experience
## 22 Openness to Experience
## 23 Openness to Experience
## 24 Openness to Experience
```

```r
HEXACO.Dist.by.commit$sample.HEXACO<-paste0(HEXACO.Dist.by.commit$sample," ",HEXACO.Dist.by.commit$domain)
HEXACO.Dist.by.commit$domain.name<-case_when(
  HEXACO.Dist.by.commit$domain=="HH"~"Honesty-Humility",
  HEXACO.Dist.by.commit$domain=="EM"~"Emotionality",
  HEXACO.Dist.by.commit$domain=="EX"~"Extraversion",
  HEXACO.Dist.by.commit$domain=="AG"~"Agreeableness",
  HEXACO.Dist.by.commit$domain=="CO"~"Conscientiousness",
  HEXACO.Dist.by.commit$domain=="OE"~"Openness to Experience")

HEXACO.Dist.by.commit
```

```
##    domain commit.z_SRc.zc.trend         SE        df     lower.CL    upper.CL
## 1      AG           0.043044660 0.02017491 1583.7234  0.003472320  0.08261700
## 2      AG           0.071272800 0.02993643 1018.3085  0.012528660  0.13001694
## 3      AG          -0.055831845 0.01778794 2440.6505 -0.090712869 -0.02095082
## 4      AG           0.062414796 0.01815319 2771.0178  0.026819646  0.09800995
## 5      CO           0.011072543 0.02112407 1897.1353 -0.030356296  0.05250138
## 6      CO           0.071733194 0.03231076 1357.4576  0.008348753  0.13511764
## 7      CO           0.034792225 0.01779427 2433.8404 -0.000101254  0.06968570
## 8      CO           0.032106755 0.01846922 2910.6175 -0.004107310  0.06832082
## 9      EM           0.049686884 0.02053612 1700.7079  0.009408164  0.08996560
## 10     EM          -0.027926024 0.02796247  783.7898 -0.082816221  0.02696417
## 11     EM           0.013562844 0.01735519 2222.2659 -0.020471247  0.04759693
## 12     EM           0.039123484 0.01719309 2205.2306  0.005407139  0.07283983
## 13     EX           0.046228424 0.02007938 1556.9650  0.006842938  0.08561391
## 14     EX          -0.016482893 0.02920934  950.6683 -0.073805134  0.04083935
## 15     EX           0.022474648 0.01717754 2112.4192 -0.011212014  0.05616131
## 16     EX           0.021487430 0.01856469 2958.2944 -0.014913581  0.05788844
## 17     HH           0.012944485 0.01961456 1447.5886 -0.025531517  0.05142049
## 18     HH           0.128632521 0.02620914  614.1053  0.077162104  0.18010294
## 19     HH           0.020141151 0.01667445 1947.9700 -0.012560496  0.05284280
## 20     HH           0.055411115 0.01672970 2029.6275  0.022601934  0.08822030
## 21     OE           0.063973905 0.02046658 1670.3890  0.023831052  0.10411676
## 22     OE           0.149020461 0.02575771  586.1096  0.098431817  0.19960910
## 23     OE          -0.003793226 0.01671646 1957.2692 -0.036577157  0.02899070
## 24     OE           0.024989890 0.01656848 2004.3170 -0.007503352  0.05748313
##       t.ratio      p.value        sample country    sample.HEXACO
## 1   2.1335738 3.303097e-02 British Isles Britain British Isles AG
## 2   2.3808052 1.745793e-02     Chinese 1   China     Chinese 1 AG
## 3  -3.1387467 1.716988e-03     Chinese 2   China     Chinese 2 AG
## 4   3.4382271 5.941766e-04     Chinese 3   China     Chinese 3 AG
## 5   0.5241672 6.002235e-01 British Isles Britain British Isles CO
## 6   2.2201024 2.657647e-02     Chinese 1   China     Chinese 1 CO
## 7   1.9552489 5.066799e-02     Chinese 2   China     Chinese 2 CO
## 8   1.7383926 8.224740e-02     Chinese 3   China     Chinese 3 CO
## 9   2.4194875 1.564669e-02 British Isles Britain British Isles EM
## 10 -0.9986966 3.182499e-01     Chinese 1   China     Chinese 1 EM
## 11  0.7814862 4.345998e-01     Chinese 2   China     Chinese 2 EM
## 12  2.2755352 2.296939e-02     Chinese 3   China     Chinese 3 EM
## 13  2.3022830 2.145060e-02 British Isles Britain British Isles EX
## 14 -0.5643021 5.726817e-01     Chinese 1   China     Chinese 1 EX
## 15  1.3083741 1.908888e-01     Chinese 2   China     Chinese 2 EX
## 16  1.1574356 2.471879e-01     Chinese 3   China     Chinese 3 EX
## 17  0.6599427 5.093956e-01 British Isles Britain British Isles HH
## 18  4.9079256 1.180859e-06     Chinese 1   China     Chinese 1 HH
## 19  1.2079047 2.272305e-01     Chinese 2   China     Chinese 2 HH
## 20  3.3121398 9.421003e-04     Chinese 3   China     Chinese 3 HH
## 21  3.1257736 1.803909e-03 British Isles Britain British Isles OE
## 22  5.7854708 1.177789e-08     Chinese 1   China     Chinese 1 OE
## 23 -0.2269157 8.205130e-01     Chinese 2   China     Chinese 2 OE
## 24  1.5082791 1.316407e-01     Chinese 3   China     Chinese 3 OE
##               domain.name
## 1           Agreeableness
## 2           Agreeableness
## 3           Agreeableness
## 4           Agreeableness
## 5       Conscientiousness
## 6       Conscientiousness
## 7       Conscientiousness
## 8       Conscientiousness
## 9            Emotionality
## 10           Emotionality
## 11           Emotionality
## 12           Emotionality
## 13           Extraversion
## 14           Extraversion
## 15           Extraversion
## 16           Extraversion
## 17       Honesty-Humility
## 18       Honesty-Humility
## 19       Honesty-Humility
## 20       Honesty-Humility
## 21 Openness to Experience
## 22 Openness to Experience
## 23 Openness to Experience
## 24 Openness to Experience
```

## Combine satisfaction and commitment effects


```r
names(HEXACO.Dist.by.commit) == names(HEXACO.Dist.by.satis)
```

```
##  [1]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
```

```r
#rename the interaction effect

names(HEXACO.Dist.by.commit)[2]<-
  "moderated_effect"

names(HEXACO.Dist.by.satis)[2]<-
  "moderated_effect"

names(HEXACO.Dist.by.commit) == names(HEXACO.Dist.by.satis)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
#add identifiers

HEXACO.Dist.by.satis$moderator<-"Satisfaction"
HEXACO.Dist.by.commit$moderator<-"Commitment"

HEXACO.Dist.by.satis.and.commit<-
  rbind(HEXACO.Dist.by.satis,
        HEXACO.Dist.by.commit)

#exclude other traits
HEXACO.Dist.by.satis.and.commit.HH.OE<-
  HEXACO.Dist.by.satis.and.commit %>%
  filter(domain=="HH" | domain=="OE")


#add subgroup-variable that is a combination of domain and moderator

HEXACO.Dist.by.satis.and.commit.HH.OE$subgroup<-
  paste0(HEXACO.Dist.by.satis.and.commit.HH.OE$domain.name," by ",HEXACO.Dist.by.satis.and.commit.HH.OE$moderator)

HEXACO.Dist.by.satis.and.commit.HH.OE
```

```
##    domain moderated_effect         SE        df     lower.CL   upper.CL
## 1      HH      0.025849641 0.01970972 1523.3917 -0.012811407 0.06451069
## 2      HH      0.089759685 0.02542654  621.9764  0.039827409 0.13969196
## 3      HH      0.019608705 0.01616182 1824.8873 -0.012088906 0.05130632
## 4      HH      0.052699713 0.01668882 1852.3438  0.019968838 0.08543059
## 5      OE      0.045617224 0.02114458 1914.1844  0.004148394 0.08708605
## 6      OE      0.111525409 0.02546432  625.5896  0.061519522 0.16153130
## 7      OE      0.066010440 0.01627521 1885.0592  0.034091113 0.09792977
## 8      OE      0.010397976 0.01643381 1778.0727 -0.021833644 0.04262959
## 9      HH      0.012944485 0.01961456 1447.5886 -0.025531517 0.05142049
## 10     HH      0.128632521 0.02620914  614.1053  0.077162104 0.18010294
## 11     HH      0.020141151 0.01667445 1947.9700 -0.012560496 0.05284280
## 12     HH      0.055411115 0.01672970 2029.6275  0.022601934 0.08822030
## 13     OE      0.063973905 0.02046658 1670.3890  0.023831052 0.10411676
## 14     OE      0.149020461 0.02575771  586.1096  0.098431817 0.19960910
## 15     OE     -0.003793226 0.01671646 1957.2692 -0.036577157 0.02899070
## 16     OE      0.024989890 0.01656848 2004.3170 -0.007503352 0.05748313
##       t.ratio      p.value        sample country    sample.HEXACO
## 1   1.3115177 1.898805e-01 British Isles Britain British Isles HH
## 2   3.5301567 4.459979e-04     Chinese 1   China     Chinese 1 HH
## 3   1.2132732 2.251824e-01     Chinese 2   China     Chinese 2 HH
## 4   3.1577852 1.615341e-03     Chinese 3   China     Chinese 3 HH
## 5   2.1573959 3.109892e-02 British Isles Britain British Isles OE
## 6   4.3796743 1.393287e-05     Chinese 1   China     Chinese 1 OE
## 7   4.0558878 5.197923e-05     Chinese 2   China     Chinese 2 OE
## 8   0.6327184 5.269989e-01     Chinese 3   China     Chinese 3 OE
## 9   0.6599427 5.093956e-01 British Isles Britain British Isles HH
## 10  4.9079256 1.180859e-06     Chinese 1   China     Chinese 1 HH
## 11  1.2079047 2.272305e-01     Chinese 2   China     Chinese 2 HH
## 12  3.3121398 9.421003e-04     Chinese 3   China     Chinese 3 HH
## 13  3.1257736 1.803909e-03 British Isles Britain British Isles OE
## 14  5.7854708 1.177789e-08     Chinese 1   China     Chinese 1 OE
## 15 -0.2269157 8.205130e-01     Chinese 2   China     Chinese 2 OE
## 16  1.5082791 1.316407e-01     Chinese 3   China     Chinese 3 OE
##               domain.name    moderator                               subgroup
## 1        Honesty-Humility Satisfaction       Honesty-Humility by Satisfaction
## 2        Honesty-Humility Satisfaction       Honesty-Humility by Satisfaction
## 3        Honesty-Humility Satisfaction       Honesty-Humility by Satisfaction
## 4        Honesty-Humility Satisfaction       Honesty-Humility by Satisfaction
## 5  Openness to Experience Satisfaction Openness to Experience by Satisfaction
## 6  Openness to Experience Satisfaction Openness to Experience by Satisfaction
## 7  Openness to Experience Satisfaction Openness to Experience by Satisfaction
## 8  Openness to Experience Satisfaction Openness to Experience by Satisfaction
## 9        Honesty-Humility   Commitment         Honesty-Humility by Commitment
## 10       Honesty-Humility   Commitment         Honesty-Humility by Commitment
## 11       Honesty-Humility   Commitment         Honesty-Humility by Commitment
## 12       Honesty-Humility   Commitment         Honesty-Humility by Commitment
## 13 Openness to Experience   Commitment   Openness to Experience by Commitment
## 14 Openness to Experience   Commitment   Openness to Experience by Commitment
## 15 Openness to Experience   Commitment   Openness to Experience by Commitment
## 16 Openness to Experience   Commitment   Openness to Experience by Commitment
```
## Analysis model


```r
HEXACO.Dist.by.satis.and.commit.HH.OE.m <- 
  metagen(TE = moderated_effect,
                                         seTE = SE,
                                         data = HEXACO.Dist.by.satis.and.commit.HH.OE,
                                         #subset=...1=="SRc.zc",
                                         studlab = sample,
                                         #id=domain,
                                         comb.fixed = FALSE,
                                         comb.random = TRUE,
                                         prediction=TRUE)

HEXACO.Dist.by.satis.and.commit.HH.OE.m.sub<-
  subgroup.analysis.mixed.effects(x = HEXACO.Dist.by.satis.and.commit.HH.OE.m,
                                  subgroups = HEXACO.Dist.by.satis.and.commit.HH.OE$subgroup)
```



## Plot the estimates


```r
grDevices::png(file = "../../../output/Meta-analysis/Figure2.png",
               family="sans",units = "mm",
               height=297/(6/4),width=210,res=300)

meta::forest(HEXACO.Dist.by.satis.and.commit.HH.OE.m.sub,
             prediction=F,
             overall=F,
             xlim=c(-.15,.30),
             zero.pval=F,
             digits.pval.Q=3,
             #addrow.overall=T,
             overall.hetstat=F,
             print.subgroup.label=T,
             bysort=F,
             smlab = "Moderated distinctive assumed similarity",
             addrow.subgroups=T,
             col.diamond.random="black")

graphics.off()
```



# Session information


```r
sI <- sessionInfo()
print(sI, locale = FALSE)
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
## [1] rio_0.5.16      dplyr_1.0.2     dmetar_0.0.9000 meta_4.15-1    
## 
## loaded via a namespace (and not attached):
##  [1] magic_1.5-9        splines_4.0.3      statmod_1.4.34     stats4_4.0.3      
##  [5] metafor_2.4-0      cellranger_1.1.0   yaml_2.2.1         robustbase_0.93-6 
##  [9] ggrepel_0.8.2      pillar_1.5.1       lattice_0.20-41    glue_1.4.2        
## [13] digest_0.6.25      minqa_1.2.4        colorspace_1.4-1   MuMIn_1.43.17     
## [17] htmltools_0.5.0    netmeta_1.2-1      Matrix_1.2-18      pkgconfig_2.0.3   
## [21] haven_2.3.1        purrr_0.3.4        scales_1.1.1       openxlsx_4.2.2    
## [25] lme4_1.1-23        tibble_3.0.3       generics_0.0.2     ggplot2_3.3.2     
## [29] ellipsis_0.3.1     nnet_7.3-14        readxl_1.3.1       magrittr_1.5      
## [33] crayon_1.4.1       mclust_5.4.6       evaluate_0.14      fansi_0.4.1       
## [37] nlme_3.1-149       MASS_7.3-53        forcats_0.5.0      xml2_1.3.2        
## [41] foreign_0.8-80     class_7.3-17       tools_4.0.3        data.table_1.14.0 
## [45] hms_0.5.3          lifecycle_1.0.0    stringr_1.4.0      kernlab_0.9-29    
## [49] munsell_0.5.0      zip_2.1.1          cluster_2.1.0      fpc_2.2-8         
## [53] compiler_4.0.3     rlang_0.4.10       grid_4.0.3         nloptr_1.2.2.2    
## [57] CompQuadForm_1.4.3 rmarkdown_2.5      boot_1.3-25        gtable_0.3.0      
## [61] abind_1.4-5        flexmix_2.3-17     curl_4.3           R6_2.4.1          
## [65] gridExtra_2.3      knitr_1.30         prabclus_2.3-2     utf8_1.2.1        
## [69] poibin_1.5         modeltools_0.2-23  stringi_1.5.3      parallel_4.0.3    
## [73] Rcpp_1.0.5         vctrs_0.3.7        DEoptimR_1.0-8     tidyselect_1.1.0  
## [77] xfun_0.19          diptest_0.75-7
```
