---
title: "Trait aggregate correlations"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Packages


```r
library(rio)
library(psych)
library(meta)
```

## Read data


```r
Danish<-import("../../data/processed/Danish/Dan_fdat.xlsx")
British<-import("../../data/processed/British/Brit_fdat.xlsx")
Chinese1<-import("../../data/processed/Chinese 1/Chi1_fdat.xlsx")
Chinese2<-import("../../data/processed/Chinese 2/Chi2_fdat.xlsx")
Chinese3<-import("../../data/processed/Chinese 3/Chi3comb_fdat.xlsx")
```

## Variables names


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

## Calculate scale means

### Danish


```r
#AG

#reverse coded self-ratings
Danish[,AG.sr.vars[AG.sr.revs]]<-
  6-Danish[,AG.sr.vars[AG.sr.revs]]

#reverse coded couple-ratings
Danish[,AG.pr.vars1[AG.sr.revs]]<-
  6-Danish[,AG.pr.vars1[AG.sr.revs]]

#CO

#reverse coded self-ratings
Danish[,CO.sr.vars[CO.sr.revs]]<-
  6-Danish[,CO.sr.vars[CO.sr.revs]]

#reverse coded couple-ratings
Danish[,CO.pr.vars1[CO.sr.revs]]<-
  6-Danish[,CO.pr.vars1[CO.sr.revs]]

#EM

#reverse coded self-ratings
Danish[,EM.sr.vars[EM.sr.revs]]<-
  6-Danish[,EM.sr.vars[EM.sr.revs]]

#reverse coded couple-ratings
Danish[,EM.pr.vars1[EM.sr.revs]]<-
  6-Danish[,EM.pr.vars1[EM.sr.revs]]

#EX

#reverse coded self-ratings
Danish[,EX.sr.vars[EX.sr.revs]]<-
  6-Danish[,EX.sr.vars[EX.sr.revs]]

#reverse coded couple-ratings
Danish[,EX.pr.vars1[EX.sr.revs]]<-
  6-Danish[,EX.pr.vars1[EX.sr.revs]]

#HH

#reverse coded self-ratings
Danish[,HH.sr.vars[HH.sr.revs]]<-
  6-Danish[,HH.sr.vars[HH.sr.revs]]

#reverse coded couple-ratings
Danish[,HH.pr.vars1[HH.sr.revs]]<-
  6-Danish[,HH.pr.vars1[HH.sr.revs]]

#OP

#reverse coded self-ratings
Danish[,OP.sr.vars[OP.sr.revs]]<-
  6-Danish[,OP.sr.vars[OP.sr.revs]]

#reverse coded couple-ratings
Danish[,OP.pr.vars1[OP.sr.revs]]<-
  6-Danish[,OP.pr.vars1[OP.sr.revs]]

#calculate scale means
Danish$sr.AG<-rowMeans(Danish[,AG.sr.vars])
Danish$sr.CO<-rowMeans(Danish[,CO.sr.vars])
Danish$sr.EM<-rowMeans(Danish[,EM.sr.vars])
Danish$sr.EX<-rowMeans(Danish[,EX.sr.vars])
Danish$sr.HH<-rowMeans(Danish[,HH.sr.vars])
Danish$sr.OP<-rowMeans(Danish[,OP.sr.vars])

Danish$pr.AG<-rowMeans(Danish[,AG.pr.vars1])
Danish$pr.CO<-rowMeans(Danish[,CO.pr.vars1])
Danish$pr.EM<-rowMeans(Danish[,EM.pr.vars1])
Danish$pr.EX<-rowMeans(Danish[,EX.pr.vars1])
Danish$pr.HH<-rowMeans(Danish[,HH.pr.vars1])
Danish$pr.OP<-rowMeans(Danish[,OP.pr.vars1])
```


### British


```r
#AG

#reverse coded self-ratings
British[,AG.sr.vars[AG.sr.revs]]<-
  6-British[,AG.sr.vars[AG.sr.revs]]

#reverse coded couple-ratings
British[,AG.pr.vars2[AG.sr.revs]]<-
  6-British[,AG.pr.vars2[AG.sr.revs]]

#CO

#reverse coded self-ratings
British[,CO.sr.vars[CO.sr.revs]]<-
  6-British[,CO.sr.vars[CO.sr.revs]]

#reverse coded couple-ratings
British[,CO.pr.vars2[CO.sr.revs]]<-
  6-British[,CO.pr.vars2[CO.sr.revs]]

#EM

#reverse coded self-ratings
British[,EM.sr.vars[EM.sr.revs]]<-
  6-British[,EM.sr.vars[EM.sr.revs]]

#reverse coded couple-ratings
British[,EM.pr.vars2[EM.sr.revs]]<-
  6-British[,EM.pr.vars2[EM.sr.revs]]

#EX

#reverse coded self-ratings
British[,EX.sr.vars[EX.sr.revs]]<-
  6-British[,EX.sr.vars[EX.sr.revs]]

#reverse coded couple-ratings
British[,EX.pr.vars2[EX.sr.revs]]<-
  6-British[,EX.pr.vars2[EX.sr.revs]]

#HH

#reverse coded self-ratings
British[,HH.sr.vars[HH.sr.revs]]<-
  6-British[,HH.sr.vars[HH.sr.revs]]

#reverse coded couple-ratings
British[,HH.pr.vars2[HH.sr.revs]]<-
  6-British[,HH.pr.vars2[HH.sr.revs]]

#OP

#reverse coded self-ratings
British[,OP.sr.vars[OP.sr.revs]]<-
  6-British[,OP.sr.vars[OP.sr.revs]]

#reverse coded couple-ratings
British[,OP.pr.vars2[OP.sr.revs]]<-
  6-British[,OP.pr.vars2[OP.sr.revs]]

#calculate scale means
British$sr.AG<-rowMeans(British[,AG.sr.vars])
British$sr.CO<-rowMeans(British[,CO.sr.vars])
British$sr.EM<-rowMeans(British[,EM.sr.vars])
British$sr.EX<-rowMeans(British[,EX.sr.vars])
British$sr.HH<-rowMeans(British[,HH.sr.vars])
British$sr.OP<-rowMeans(British[,OP.sr.vars])

British$pr.AG<-rowMeans(British[,AG.pr.vars2])
British$pr.CO<-rowMeans(British[,CO.pr.vars2])
British$pr.EM<-rowMeans(British[,EM.pr.vars2])
British$pr.EX<-rowMeans(British[,EX.pr.vars2])
British$pr.HH<-rowMeans(British[,HH.pr.vars2])
British$pr.OP<-rowMeans(British[,OP.pr.vars2])
```

### Chinese1


```r
#AG

#reverse coded self-ratings
Chinese1[,AG.sr.vars[AG.sr.revs]]<-
  6-Chinese1[,AG.sr.vars[AG.sr.revs]]

#reverse coded couple-ratings
Chinese1[,AG.pr.vars2[AG.sr.revs]]<-
  6-Chinese1[,AG.pr.vars2[AG.sr.revs]]

#CO

#reverse coded self-ratings
Chinese1[,CO.sr.vars[CO.sr.revs]]<-
  6-Chinese1[,CO.sr.vars[CO.sr.revs]]

#reverse coded couple-ratings
Chinese1[,CO.pr.vars2[CO.sr.revs]]<-
  6-Chinese1[,CO.pr.vars2[CO.sr.revs]]

#EM

#reverse coded self-ratings
Chinese1[,EM.sr.vars[EM.sr.revs]]<-
  6-Chinese1[,EM.sr.vars[EM.sr.revs]]

#reverse coded couple-ratings
Chinese1[,EM.pr.vars2[EM.sr.revs]]<-
  6-Chinese1[,EM.pr.vars2[EM.sr.revs]]

#EX

#reverse coded self-ratings
Chinese1[,EX.sr.vars[EX.sr.revs]]<-
  6-Chinese1[,EX.sr.vars[EX.sr.revs]]

#reverse coded couple-ratings
Chinese1[,EX.pr.vars2[EX.sr.revs]]<-
  6-Chinese1[,EX.pr.vars2[EX.sr.revs]]

#HH

#reverse coded self-ratings
Chinese1[,HH.sr.vars[HH.sr.revs]]<-
  6-Chinese1[,HH.sr.vars[HH.sr.revs]]

#reverse coded couple-ratings
Chinese1[,HH.pr.vars2[HH.sr.revs]]<-
  6-Chinese1[,HH.pr.vars2[HH.sr.revs]]

#OP

#reverse coded self-ratings
Chinese1[,OP.sr.vars[OP.sr.revs]]<-
  6-Chinese1[,OP.sr.vars[OP.sr.revs]]

#reverse coded couple-ratings
Chinese1[,OP.pr.vars2[OP.sr.revs]]<-
  6-Chinese1[,OP.pr.vars2[OP.sr.revs]]

#calculate scale means
Chinese1$sr.AG<-rowMeans(Chinese1[,AG.sr.vars])
Chinese1$sr.CO<-rowMeans(Chinese1[,CO.sr.vars])
Chinese1$sr.EM<-rowMeans(Chinese1[,EM.sr.vars])
Chinese1$sr.EX<-rowMeans(Chinese1[,EX.sr.vars])
Chinese1$sr.HH<-rowMeans(Chinese1[,HH.sr.vars])
Chinese1$sr.OP<-rowMeans(Chinese1[,OP.sr.vars])

Chinese1$pr.AG<-rowMeans(Chinese1[,AG.pr.vars2])
Chinese1$pr.CO<-rowMeans(Chinese1[,CO.pr.vars2])
Chinese1$pr.EM<-rowMeans(Chinese1[,EM.pr.vars2])
Chinese1$pr.EX<-rowMeans(Chinese1[,EX.pr.vars2])
Chinese1$pr.HH<-rowMeans(Chinese1[,HH.pr.vars2])
Chinese1$pr.OP<-rowMeans(Chinese1[,OP.pr.vars2])
```

### Chinese2


```r
#AG

#reverse coded self-ratings
Chinese2[,AG.sr.vars[AG.sr.revs]]<-
  6-Chinese2[,AG.sr.vars[AG.sr.revs]]

#reverse coded couple-ratings
Chinese2[,AG.pr.vars2[AG.sr.revs]]<-
  6-Chinese2[,AG.pr.vars2[AG.sr.revs]]

#CO

#reverse coded self-ratings
Chinese2[,CO.sr.vars[CO.sr.revs]]<-
  6-Chinese2[,CO.sr.vars[CO.sr.revs]]

#reverse coded couple-ratings
Chinese2[,CO.pr.vars2[CO.sr.revs]]<-
  6-Chinese2[,CO.pr.vars2[CO.sr.revs]]

#EM

#reverse coded self-ratings
Chinese2[,EM.sr.vars[EM.sr.revs]]<-
  6-Chinese2[,EM.sr.vars[EM.sr.revs]]

#reverse coded couple-ratings
Chinese2[,EM.pr.vars2[EM.sr.revs]]<-
  6-Chinese2[,EM.pr.vars2[EM.sr.revs]]

#EX

#reverse coded self-ratings
Chinese2[,EX.sr.vars[EX.sr.revs]]<-
  6-Chinese2[,EX.sr.vars[EX.sr.revs]]

#reverse coded couple-ratings
Chinese2[,EX.pr.vars2[EX.sr.revs]]<-
  6-Chinese2[,EX.pr.vars2[EX.sr.revs]]

#HH

#reverse coded self-ratings
Chinese2[,HH.sr.vars[HH.sr.revs]]<-
  6-Chinese2[,HH.sr.vars[HH.sr.revs]]

#reverse coded couple-ratings
Chinese2[,HH.pr.vars2[HH.sr.revs]]<-
  6-Chinese2[,HH.pr.vars2[HH.sr.revs]]

#OP

#reverse coded self-ratings
Chinese2[,OP.sr.vars[OP.sr.revs]]<-
  6-Chinese2[,OP.sr.vars[OP.sr.revs]]

#reverse coded couple-ratings
Chinese2[,OP.pr.vars2[OP.sr.revs]]<-
  6-Chinese2[,OP.pr.vars2[OP.sr.revs]]

#calculate scale means
Chinese2$sr.AG<-rowMeans(Chinese2[,AG.sr.vars])
Chinese2$sr.CO<-rowMeans(Chinese2[,CO.sr.vars])
Chinese2$sr.EM<-rowMeans(Chinese2[,EM.sr.vars])
Chinese2$sr.EX<-rowMeans(Chinese2[,EX.sr.vars])
Chinese2$sr.HH<-rowMeans(Chinese2[,HH.sr.vars])
Chinese2$sr.OP<-rowMeans(Chinese2[,OP.sr.vars])

Chinese2$pr.AG<-rowMeans(Chinese2[,AG.pr.vars2])
Chinese2$pr.CO<-rowMeans(Chinese2[,CO.pr.vars2])
Chinese2$pr.EM<-rowMeans(Chinese2[,EM.pr.vars2])
Chinese2$pr.EX<-rowMeans(Chinese2[,EX.pr.vars2])
Chinese2$pr.HH<-rowMeans(Chinese2[,HH.pr.vars2])
Chinese2$pr.OP<-rowMeans(Chinese2[,OP.pr.vars2])
```

### Chinese3


```r
#AG

#reverse coded self-ratings
Chinese3[,AG.sr.vars[AG.sr.revs]]<-
  6-Chinese3[,AG.sr.vars[AG.sr.revs]]

#reverse coded couple-ratings
Chinese3[,AG.pr.vars2[AG.sr.revs]]<-
  6-Chinese3[,AG.pr.vars2[AG.sr.revs]]

#CO

#reverse coded self-ratings
Chinese3[,CO.sr.vars[CO.sr.revs]]<-
  6-Chinese3[,CO.sr.vars[CO.sr.revs]]

#reverse coded couple-ratings
Chinese3[,CO.pr.vars2[CO.sr.revs]]<-
  6-Chinese3[,CO.pr.vars2[CO.sr.revs]]

#EM

#reverse coded self-ratings
Chinese3[,EM.sr.vars[EM.sr.revs]]<-
  6-Chinese3[,EM.sr.vars[EM.sr.revs]]

#reverse coded couple-ratings
Chinese3[,EM.pr.vars2[EM.sr.revs]]<-
  6-Chinese3[,EM.pr.vars2[EM.sr.revs]]

#EX

#reverse coded self-ratings
Chinese3[,EX.sr.vars[EX.sr.revs]]<-
  6-Chinese3[,EX.sr.vars[EX.sr.revs]]

#reverse coded couple-ratings
Chinese3[,EX.pr.vars2[EX.sr.revs]]<-
  6-Chinese3[,EX.pr.vars2[EX.sr.revs]]

#HH

#reverse coded self-ratings
Chinese3[,HH.sr.vars[HH.sr.revs]]<-
  6-Chinese3[,HH.sr.vars[HH.sr.revs]]

#reverse coded couple-ratings
Chinese3[,HH.pr.vars2[HH.sr.revs]]<-
  6-Chinese3[,HH.pr.vars2[HH.sr.revs]]

#OP

#reverse coded self-ratings
Chinese3[,OP.sr.vars[OP.sr.revs]]<-
  6-Chinese3[,OP.sr.vars[OP.sr.revs]]

#reverse coded couple-ratings
Chinese3[,OP.pr.vars2[OP.sr.revs]]<-
  6-Chinese3[,OP.pr.vars2[OP.sr.revs]]

#calculate scale means
Chinese3$sr.AG<-rowMeans(Chinese3[,AG.sr.vars])
Chinese3$sr.CO<-rowMeans(Chinese3[,CO.sr.vars])
Chinese3$sr.EM<-rowMeans(Chinese3[,EM.sr.vars])
Chinese3$sr.EX<-rowMeans(Chinese3[,EX.sr.vars])
Chinese3$sr.HH<-rowMeans(Chinese3[,HH.sr.vars])
Chinese3$sr.OP<-rowMeans(Chinese3[,OP.sr.vars])

Chinese3$pr.AG<-rowMeans(Chinese3[,AG.pr.vars2])
Chinese3$pr.CO<-rowMeans(Chinese3[,CO.pr.vars2])
Chinese3$pr.EM<-rowMeans(Chinese3[,EM.pr.vars2])
Chinese3$pr.EX<-rowMeans(Chinese3[,EX.pr.vars2])
Chinese3$pr.HH<-rowMeans(Chinese3[,HH.pr.vars2])
Chinese3$pr.OP<-rowMeans(Chinese3[,OP.pr.vars2])
```

## Scale names


```r
sr.scales<-c("sr.AG",
             "sr.CO",
             "sr.EM",
             "sr.EX",
             "sr.HH",
             "sr.OP")

pr.scales<-c("pr.AG",
             "pr.CO",
             "pr.EM",
             "pr.EX",
             "pr.HH",
             "pr.OP")
```


# Calculate correlations


## Danish


```r
Danish.cors<-corr.test(Danish[,c(sr.scales,pr.scales)],
                adjust="none")

Danish.cors.list<-list()


for (i in 1:length(sr.scales)){
  corr<-Danish.cors$r[sr.scales[i],pr.scales[i]]
  se<-Danish.cors$se[sr.scales[i],pr.scales[i]]
  t.val<-Danish.cors$t[sr.scales[i],pr.scales[i]]
  p<-Danish.cors$p[sr.scales[i],pr.scales[i]]
  
  Danish.cors.list[[i]]<-cbind.data.frame(
    trait=sr.scales[i],r=corr,se=se,t=t.val,p=p
  )
  
}

Danish.cors.df<-do.call(rbind.data.frame,Danish.cors.list)
cbind.data.frame(trait=substr(Danish.cors.df$trait,4,5),
                 round(Danish.cors.df[,2:4],2),
                 p=round(Danish.cors.df[,5],3))
```

```
##   trait     r   se     t     p
## 1    AG -0.02 0.11 -0.16 0.877
## 2    CO -0.20 0.10 -1.96 0.053
## 3    EM -0.18 0.10 -1.73 0.087
## 4    EX  0.16 0.10  1.56 0.123
## 5    HH  0.50 0.09  5.43 0.000
## 6    OP  0.35 0.10  3.53 0.001
```

```r
export(Danish.cors.df,"../../output/Danish/trait.agg.cors.xlsx")
```

## British


```r
British.cors<-corr.test(British[,c(sr.scales,pr.scales)],
                adjust="none")

British.cors.list<-list()


for (i in 1:length(sr.scales)){
  corr<-British.cors$r[sr.scales[i],pr.scales[i]]
  se<-British.cors$se[sr.scales[i],pr.scales[i]]
  t.val<-British.cors$t[sr.scales[i],pr.scales[i]]
  p<-British.cors$p[sr.scales[i],pr.scales[i]]
  
  British.cors.list[[i]]<-cbind.data.frame(
    trait=sr.scales[i],r=corr,se=se,t=t.val,p=p
  )
  
}

British.cors.df<-do.call(rbind.data.frame,British.cors.list)
cbind.data.frame(trait=substr(British.cors.df$trait,4,5),
                 round(British.cors.df[,2:4],2),
                 p=round(British.cors.df[,5],3))
```

```
##   trait     r   se     t     p
## 1    AG -0.10 0.06 -1.68 0.094
## 2    CO -0.03 0.06 -0.50 0.617
## 3    EM -0.20 0.06 -3.42 0.001
## 4    EX  0.02 0.06  0.31 0.755
## 5    HH  0.44 0.05  8.28 0.000
## 6    OP  0.31 0.06  5.54 0.000
```

```r
export(British.cors.df,"../../output/British/trait.agg.cors.xlsx")
```


## Chinese1


```r
Chinese1.cors<-corr.test(Chinese1[,c(sr.scales,pr.scales)],
                adjust="none")

Chinese1.cors.list<-list()


for (i in 1:length(sr.scales)){
  corr<-Chinese1.cors$r[sr.scales[i],pr.scales[i]]
  se<-Chinese1.cors$se[sr.scales[i],pr.scales[i]]
  t.val<-Chinese1.cors$t[sr.scales[i],pr.scales[i]]
  p<-Chinese1.cors$p[sr.scales[i],pr.scales[i]]
  
  Chinese1.cors.list[[i]]<-cbind.data.frame(
    trait=sr.scales[i],r=corr,se=se,t=t.val,p=p
  )
  
}

Chinese1.cors.df<-do.call(rbind.data.frame,Chinese1.cors.list)
cbind.data.frame(trait=substr(Chinese1.cors.df$trait,4,5),
                 round(Chinese1.cors.df[,2:4],2),
                 p=round(Chinese1.cors.df[,5],3))
```

```
##   trait     r   se     t     p
## 1    AG  0.21 0.08  2.49 0.014
## 2    CO -0.04 0.08 -0.47 0.637
## 3    EM  0.02 0.08  0.19 0.847
## 4    EX  0.08 0.08  0.98 0.327
## 5    HH  0.49 0.07  6.67 0.000
## 6    OP  0.26 0.08  3.11 0.002
```

```r
export(Chinese1.cors.df,"../../output/Chinese 1/trait.agg.cors.xlsx")
```

## Chinese2


```r
Chinese2.cors<-corr.test(Chinese2[,c(sr.scales,pr.scales)],
                adjust="none")

Chinese2.cors.list<-list()


for (i in 1:length(sr.scales)){
  corr<-Chinese2.cors$r[sr.scales[i],pr.scales[i]]
  se<-Chinese2.cors$se[sr.scales[i],pr.scales[i]]
  t.val<-Chinese2.cors$t[sr.scales[i],pr.scales[i]]
  p<-Chinese2.cors$p[sr.scales[i],pr.scales[i]]
  
  Chinese2.cors.list[[i]]<-cbind.data.frame(
    trait=sr.scales[i],r=corr,se=se,t=t.val,p=p
  )
  
}

Chinese2.cors.df<-do.call(rbind.data.frame,Chinese2.cors.list)
cbind.data.frame(trait=substr(Chinese2.cors.df$trait,4,5),
                 round(Chinese2.cors.df[,2:4],2),
                 p=round(Chinese2.cors.df[,5],3))
```

```
##   trait     r   se     t     p
## 1    AG  0.12 0.05  2.26 0.024
## 2    CO  0.16 0.05  3.10 0.002
## 3    EM -0.13 0.05 -2.51 0.012
## 4    EX  0.07 0.05  1.32 0.187
## 5    HH  0.52 0.05 11.33 0.000
## 6    OP  0.27 0.05  5.33 0.000
```

```r
export(Chinese2.cors.df,"../../output/Chinese 2/trait.agg.cors.xlsx")
```

## Chinese3


```r
Chinese3.cors<-corr.test(Chinese3[,c(sr.scales,pr.scales)],
                adjust="none")

Chinese3.cors.list<-list()


for (i in 1:length(sr.scales)){
  corr<-Chinese3.cors$r[sr.scales[i],pr.scales[i]]
  se<-Chinese3.cors$se[sr.scales[i],pr.scales[i]]
  t.val<-Chinese3.cors$t[sr.scales[i],pr.scales[i]]
  p<-Chinese3.cors$p[sr.scales[i],pr.scales[i]]
  
  Chinese3.cors.list[[i]]<-cbind.data.frame(
    trait=sr.scales[i],r=corr,se=se,t=t.val,p=p
  )
  
}

Chinese3.cors.df<-do.call(rbind.data.frame,Chinese3.cors.list)
cbind.data.frame(trait=substr(Chinese3.cors.df$trait,4,5),
                 round(Chinese3.cors.df[,2:4],2),
                 p=round(Chinese3.cors.df[,5],3))
```

```
##   trait     r   se     t     p
## 1    AG  0.13 0.06  2.24 0.026
## 2    CO -0.05 0.06 -0.91 0.361
## 3    EM -0.15 0.06 -2.76 0.006
## 4    EX  0.08 0.06  1.43 0.155
## 5    HH  0.48 0.05  9.67 0.000
## 6    OP  0.30 0.05  5.51 0.000
```

```r
export(Chinese3.cors.df,"../../output/Chinese 3/trait.agg.cors.xlsx")
```

## Meta-analytical

### AG


```r
AG.eff<-data.frame(
  r=
  c(
    Danish.cors.df[Danish.cors.df$trait=="sr.AG","r"],
    British.cors.df[British.cors.df$trait=="sr.AG","r"],
    Chinese1.cors.df[Chinese1.cors.df$trait=="sr.AG","r"],
    Chinese2.cors.df[Chinese2.cors.df$trait=="sr.AG","r"],
    Chinese3.cors.df[Chinese3.cors.df$trait=="sr.AG","r"]),
  n=
    c(nrow(Danish),
      nrow(British),
      nrow(Chinese1),
      nrow(Chinese2),
      nrow(Chinese3)))
  
m.AG<-metacor(cor = AG.eff$r,n=AG.eff$n,
              sm="ZCOR",comb.fixed=F,
              method.tau="ML",
              studlab=c("Danish",
                        "British Isles",
                        "Chinese 1",
                        "Chinese 2",
                        "Chinese 3"))

m.AG
```

```
##                   COR            95%-CI %W(random)
## Danish        -0.0163 [-0.2204; 0.1891]       13.2
## British Isles -0.0981 [-0.2105; 0.0169]       22.7
## Chinese 1      0.2062 [ 0.0424; 0.3593]       16.8
## Chinese 2      0.1192 [ 0.0155; 0.2205]       24.1
## Chinese 3      0.1254 [ 0.0151; 0.2327]       23.2
## 
## Number of studies combined: k = 5
## 
##                         COR            95%-CI    z p-value
## Random effects model 0.0687 [-0.0282; 0.1643] 1.39  0.1643
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0073 [0.0008; 0.1200]; tau = 0.0856 [0.0283; 0.3464];
##  I^2 = 70.1% [23.8%; 88.3%]; H = 1.83 [1.15; 2.92]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  13.39    4  0.0095
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-profile method for confidence interval of tau^2 and tau
## - Fisher's z transformation of correlations
```

```r
m.AG.row<-
  cbind.data.frame(
  trait="AG",
  eff=m.AG$TE.random,
  se=m.AG$seTE.random,
  z=m.AG$zval.random,
  p=m.AG$pval.random
)
```


### CO


```r
CO.eff<-data.frame(
  r=
  c(
    Danish.cors.df[Danish.cors.df$trait=="sr.CO","r"],
    British.cors.df[British.cors.df$trait=="sr.CO","r"],
    Chinese1.cors.df[Chinese1.cors.df$trait=="sr.CO","r"],
    Chinese2.cors.df[Chinese2.cors.df$trait=="sr.CO","r"],
    Chinese3.cors.df[Chinese3.cors.df$trait=="sr.CO","r"]),
  n=
    c(nrow(Danish),
      nrow(British),
      nrow(Chinese1),
      nrow(Chinese2),
      nrow(Chinese3)))
  
m.CO<-metacor(cor = CO.eff$r,n=CO.eff$n,
              sm="ZCOR",comb.fixed=F,
              method.tau="ML",
              studlab=c("Danish",
                        "British Isles",
                        "Chinese 1",
                        "Chinese 2",
                        "Chinese 3"))

m.CO
```

```
##                   COR            95%-CI %W(random)
## Danish        -0.2026 [-0.3912; 0.0023]       13.4
## British Isles -0.0294 [-0.1437; 0.0857]       22.6
## Chinese 1     -0.0401 [-0.2041; 0.1261]       17.0
## Chinese 2      0.1626 [ 0.0597; 0.2621]       23.9
## Chinese 3     -0.0516 [-0.1612; 0.0592]       23.1
## 
## Number of studies combined: k = 5
## 
##                          COR            95%-CI     z p-value
## Random effects model -0.0138 [-0.1128; 0.0856] -0.27  0.7864
## 
## Quantifying heterogeneity:
##  tau^2 = 0.0080 [0.0011; 0.1360]; tau = 0.0892 [0.0338; 0.3688];
##  I^2 = 72.4% [30.6%; 89.0%]; H = 1.90 [1.20; 3.02]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  14.49    4  0.0059
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-profile method for confidence interval of tau^2 and tau
## - Fisher's z transformation of correlations
```

```r
m.CO.row<-
  cbind.data.frame(
  trait="CO",
  eff=m.CO$TE.random,
  se=m.CO$seTE.random,
  z=m.CO$zval.random,
  p=m.CO$pval.random
)
```


### EM


```r
EM.eff<-data.frame(
  r=
  c(
    Danish.cors.df[Danish.cors.df$trait=="sr.EM","r"],
    British.cors.df[British.cors.df$trait=="sr.EM","r"],
    Chinese1.cors.df[Chinese1.cors.df$trait=="sr.EM","r"],
    Chinese2.cors.df[Chinese2.cors.df$trait=="sr.EM","r"],
    Chinese3.cors.df[Chinese3.cors.df$trait=="sr.EM","r"]),
  n=
    c(nrow(Danish),
      nrow(British),
      nrow(Chinese1),
      nrow(Chinese2),
      nrow(Chinese3)))
  
m.EM<-metacor(cor = EM.eff$r,n=EM.eff$n,
              sm="ZCOR",comb.fixed=F,
              method.tau="ML",
              studlab=c("Danish",
                        "British Isles",
                        "Chinese 1",
                        "Chinese 2",
                        "Chinese 3"))

m.EM
```

```
##                   COR             95%-CI %W(random)
## Danish        -0.1797 [-0.3709;  0.0261]        7.5
## British Isles -0.1970 [-0.3049; -0.0841]       24.5
## Chinese 1      0.0164 [-0.1493;  0.1813]       11.7
## Chinese 2     -0.1323 [-0.2331; -0.0288]       29.9
## Chinese 3     -0.1541 [-0.2601; -0.0443]       26.4
## 
## Number of studies combined: k = 5
## 
##                          COR             95%-CI     z  p-value
## Random effects model -0.1405 [-0.1959; -0.0842] -4.86 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 < 0.0001 [0.0000; 0.0543]; tau = 0.0009 [0.0000; 0.2329];
##  I^2 = 13.9% [0.0%; 82.1%]; H = 1.08 [1.00; 2.36]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  4.65    4  0.3255
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-profile method for confidence interval of tau^2 and tau
## - Fisher's z transformation of correlations
```

```r
m.EM.row<-
  cbind.data.frame(
  trait="EM",
  eff=m.EM$TE.random,
  se=m.EM$seTE.random,
  z=m.EM$zval.random,
  p=m.EM$pval.random
)
```


### EX


```r
EX.eff<-data.frame(
  r=
  c(
    Danish.cors.df[Danish.cors.df$trait=="sr.EX","r"],
    British.cors.df[British.cors.df$trait=="sr.EX","r"],
    Chinese1.cors.df[Chinese1.cors.df$trait=="sr.EX","r"],
    Chinese2.cors.df[Chinese2.cors.df$trait=="sr.EX","r"],
    Chinese3.cors.df[Chinese3.cors.df$trait=="sr.EX","r"]),
  n=
    c(nrow(Danish),
      nrow(British),
      nrow(Chinese1),
      nrow(Chinese2),
      nrow(Chinese3)))
  
m.EX<-metacor(cor = EX.eff$r,n=EX.eff$n,
              sm="ZCOR",comb.fixed=F,
              method.tau="ML",
              studlab=c("Danish",
                        "British Isles",
                        "Chinese 1",
                        "Chinese 2",
                        "Chinese 3"))

m.EX
```

```
##                  COR            95%-CI %W(random)
## Danish        0.1621 [-0.0441; 0.3552]        7.5
## British Isles 0.0183 [-0.0967; 0.1328]       24.5
## Chinese 1     0.0831 [-0.0833; 0.2451]       11.7
## Chinese 2     0.0701 [-0.0341; 0.1728]       29.9
## Chinese 3     0.0803 [-0.0305; 0.1891]       26.4
## 
## Number of studies combined: k = 5
## 
##                         COR           95%-CI    z p-value
## Random effects model 0.0687 [0.0118; 0.1252] 2.36  0.0181
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0147]; tau = 0 [0.0000; 0.1211];
##  I^2 = 0.0% [0.0%; 48.3%]; H = 1.00 [1.00; 1.39]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.61    4  0.8072
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-profile method for confidence interval of tau^2 and tau
## - Fisher's z transformation of correlations
```

```r
m.EX.row<-
  cbind.data.frame(
  trait="EX",
  eff=m.EX$TE.random,
  se=m.EX$seTE.random,
  z=m.EX$zval.random,
  p=m.EX$pval.random
)
```


### HH


```r
HH.eff<-data.frame(
  r=
  c(
    Danish.cors.df[Danish.cors.df$trait=="sr.HH","r"],
    British.cors.df[British.cors.df$trait=="sr.HH","r"],
    Chinese1.cors.df[Chinese1.cors.df$trait=="sr.HH","r"],
    Chinese2.cors.df[Chinese2.cors.df$trait=="sr.HH","r"],
    Chinese3.cors.df[Chinese3.cors.df$trait=="sr.HH","r"]),
  n=
    c(nrow(Danish),
      nrow(British),
      nrow(Chinese1),
      nrow(Chinese2),
      nrow(Chinese3)))
  
m.HH<-metacor(cor = HH.eff$r,n=HH.eff$n,
              sm="ZCOR",comb.fixed=F,
              method.tau="ML",
              studlab=c("Danish",
                        "British Isles",
                        "Chinese 1",
                        "Chinese 2",
                        "Chinese 3"))

m.HH
```

```
##                  COR           95%-CI %W(random)
## Danish        0.4965 [0.3247; 0.6366]        7.5
## British Isles 0.4374 [0.3397; 0.5258]       24.5
## Chinese 1     0.4922 [0.3558; 0.6080]       11.7
## Chinese 2     0.5160 [0.4354; 0.5884]       29.9
## Chinese 3     0.4797 [0.3899; 0.5605]       26.4
## 
## Number of studies combined: k = 5
## 
##                         COR           95%-CI     z  p-value
## Random effects model 0.4835 [0.4386; 0.5260] 18.13 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0083]; tau = 0 [0.0000; 0.0913];
##  I^2 = 0.0% [0.0%; 51.2%]; H = 1.00 [1.00; 1.43]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  1.70    4  0.7899
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-profile method for confidence interval of tau^2 and tau
## - Fisher's z transformation of correlations
```

```r
m.HH.row<-
  cbind.data.frame(
  trait="HH",
  eff=m.HH$TE.random,
  se=m.HH$seTE.random,
  z=m.HH$zval.random,
  p=m.HH$pval.random
)
```


### OP


```r
OP.eff<-data.frame(
  r=
  c(
    Danish.cors.df[Danish.cors.df$trait=="sr.OP","r"],
    British.cors.df[British.cors.df$trait=="sr.OP","r"],
    Chinese1.cors.df[Chinese1.cors.df$trait=="sr.OP","r"],
    Chinese2.cors.df[Chinese2.cors.df$trait=="sr.OP","r"],
    Chinese3.cors.df[Chinese3.cors.df$trait=="sr.OP","r"]),
  n=
    c(nrow(Danish),
      nrow(British),
      nrow(Chinese1),
      nrow(Chinese2),
      nrow(Chinese3)))
  
m.OP<-metacor(cor = OP.eff$r,n=OP.eff$n,
              sm="ZCOR",comb.fixed=F,
              method.tau="ML",
              studlab=c("Danish",
                        "British Isles",
                        "Chinese 1",
                        "Chinese 2",
                        "Chinese 3"))

m.OP
```

```
##                  COR           95%-CI %W(random)
## Danish        0.3485 [0.1547; 0.5164]        7.5
## British Isles 0.3096 [0.2020; 0.4098]       24.5
## Chinese 1     0.2551 [0.0938; 0.4034]       11.7
## Chinese 2     0.2724 [0.1733; 0.3660]       29.9
## Chinese 3     0.2975 [0.1934; 0.3951]       26.4
## 
## Number of studies combined: k = 5
## 
##                         COR           95%-CI     z  p-value
## Random effects model 0.2920 [0.2390; 0.3433] 10.34 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0 [0.0000; 0.0049]; tau = 0 [0.0000; 0.0699];
##  I^2 = 0.0% [0.0%; 2.4%]; H = 1.00 [1.00; 1.01]
## 
## Test of heterogeneity:
##     Q d.f. p-value
##  0.85    4  0.9314
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Maximum-likelihood estimator for tau^2
## - Q-profile method for confidence interval of tau^2 and tau
## - Fisher's z transformation of correlations
```

```r
m.OP.row<-
  cbind.data.frame(
  trait="OP",
  eff=m.OP$TE.random,
  se=m.OP$seTE.random,
  z=m.OP$zval.random,
  p=m.OP$pval.random
)
```

## Results to same table


```r
m.ALL<-
  rbind(m.AG.row,
        m.CO.row,
        m.EM.row,
        m.EX.row,
        m.HH.row,
        m.OP.row)

m.ALL
```

```
##   trait         eff         se          z            p
## 1    AG  0.06882193 0.04949092  1.3905971 1.643476e-01
## 2    CO -0.01376337 0.05079309 -0.2709693 7.864146e-01
## 3    EM -0.14142897 0.02910179 -4.8598026 1.175028e-06
## 4    EX  0.06880283 0.02909880  2.3644562 1.805656e-02
## 5    HH  0.52751878 0.02909880 18.1285418 1.897236e-73
## 6    OP  0.30079621 0.02909880 10.3370666 4.789751e-25
```

```r
export(m.ALL,
       "../../output/Meta-analysis/Trait_aggregate_assumed_similarity.xlsx")
```

# Session information


```r
s<-sessionInfo()
print(s,locale=F)
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] meta_4.15-1 psych_2.0.9 rio_0.5.16 
## 
## loaded via a namespace (and not attached):
##  [1] zip_2.1.1          Rcpp_1.0.5         nloptr_1.2.2.2     pillar_1.5.1      
##  [5] compiler_4.0.3     cellranger_1.1.0   forcats_0.5.0      tools_4.0.3       
##  [9] boot_1.3-25        statmod_1.4.34     lme4_1.1-23        digest_0.6.25     
## [13] evaluate_0.14      lifecycle_1.0.0    tibble_3.0.3       nlme_3.1-149      
## [17] lattice_0.20-41    pkgconfig_2.0.3    rlang_0.4.10       Matrix_1.2-18     
## [21] openxlsx_4.2.2     curl_4.3           yaml_2.2.1         parallel_4.0.3    
## [25] CompQuadForm_1.4.3 haven_2.3.1        xfun_0.19          xml2_1.3.2        
## [29] metafor_2.4-0      stringr_1.4.0      knitr_1.30         vctrs_0.3.7       
## [33] hms_0.5.3          grid_4.0.3         data.table_1.14.0  fansi_0.4.1       
## [37] readxl_1.3.1       foreign_0.8-80     rmarkdown_2.5      minqa_1.2.4       
## [41] magrittr_1.5       MASS_7.3-53        splines_4.0.3      ellipsis_0.3.1    
## [45] htmltools_0.5.0    mnormt_2.0.2       utf8_1.2.1         stringi_1.5.3     
## [49] tmvnsim_1.0-2      crayon_1.4.1
```
