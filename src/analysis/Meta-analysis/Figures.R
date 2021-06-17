## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------
library(meta)
#library(devtools)
#install.packages("devtools")
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(dplyr)
library(rio)


## ----------------------------------------------------------------------------

HEXACO.Dist<-
  import("../../../data/processed/Meta-analysis/HEXACO.Dist.xlsx")

#rename British to British Isles
HEXACO.Dist$sample[HEXACO.Dist$sample=="British"]<-"British Isles"



## ----------------------------------------------------------------------------

HEXACO.Dist$sample.HEXACO<-paste0(HEXACO.Dist$sample," ",HEXACO.Dist$domain)
HEXACO.Dist$domain.name<-case_when(
  HEXACO.Dist$domain=="HH"~"Honesty-Humility",
  HEXACO.Dist$domain=="EM"~"Emotionality",
  HEXACO.Dist$domain=="EX"~"Extraversion",
  HEXACO.Dist$domain=="AG"~"Agreeableness",
  HEXACO.Dist$domain=="CO"~"Conscientiousness",
  HEXACO.Dist$domain=="OE"~"Openness to Experience")

HEXACO.Dist



## ----------------------------------------------------------------------------

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



## ----------------------------------------------------------------------------

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


## ----------------------------------------------------------------------------

HEXACO.Dist.by.satis<-
  import("../../../data/processed/Meta-analysis/HEXACO.Dist.by.satis.xlsx")

HEXACO.Dist.by.commit<-
  import("../../../data/processed/Meta-analysis/HEXACO.Dist.by.commit.xlsx")


#rename British to British Isles
HEXACO.Dist.by.satis$sample[HEXACO.Dist.by.satis$sample=="British"]<-
  "British Isles"

HEXACO.Dist.by.commit$sample[HEXACO.Dist.by.commit$sample=="British"]<-
  "British Isles"



## ----------------------------------------------------------------------------

HEXACO.Dist.by.satis$sample.HEXACO<-paste0(HEXACO.Dist.by.satis$sample," ",HEXACO.Dist.by.satis$domain)
HEXACO.Dist.by.satis$domain.name<-case_when(
  HEXACO.Dist.by.satis$domain=="HH"~"Honesty-Humility",
  HEXACO.Dist.by.satis$domain=="EM"~"Emotionality",
  HEXACO.Dist.by.satis$domain=="EX"~"Extraversion",
  HEXACO.Dist.by.satis$domain=="AG"~"Agreeableness",
  HEXACO.Dist.by.satis$domain=="CO"~"Conscientiousness",
  HEXACO.Dist.by.satis$domain=="OE"~"Openness to Experience")

HEXACO.Dist.by.satis

HEXACO.Dist.by.commit$sample.HEXACO<-paste0(HEXACO.Dist.by.commit$sample," ",HEXACO.Dist.by.commit$domain)
HEXACO.Dist.by.commit$domain.name<-case_when(
  HEXACO.Dist.by.commit$domain=="HH"~"Honesty-Humility",
  HEXACO.Dist.by.commit$domain=="EM"~"Emotionality",
  HEXACO.Dist.by.commit$domain=="EX"~"Extraversion",
  HEXACO.Dist.by.commit$domain=="AG"~"Agreeableness",
  HEXACO.Dist.by.commit$domain=="CO"~"Conscientiousness",
  HEXACO.Dist.by.commit$domain=="OE"~"Openness to Experience")

HEXACO.Dist.by.commit



## ----------------------------------------------------------------------------
names(HEXACO.Dist.by.commit) == names(HEXACO.Dist.by.satis)

#rename the interaction effect

names(HEXACO.Dist.by.commit)[2]<-
  "moderated_effect"

names(HEXACO.Dist.by.satis)[2]<-
  "moderated_effect"

names(HEXACO.Dist.by.commit) == names(HEXACO.Dist.by.satis)

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





## ----------------------------------------------------------------------------

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


## ----------------------------------------------------------------------------

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


## ----------------------------------------------------------------------------
sI <- sessionInfo()
print(sI, locale = FALSE)

