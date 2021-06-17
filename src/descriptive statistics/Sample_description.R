## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------
library(rio)
library(psych)
library(dplyr)


## ----------------------------------------------------------------------------
Dan<-import("Dan_fdat.xlsx")
Brit<-import("Brit_fdat.xlsx")
Chi2<-import("Chi2_fdat.xlsx")
CH3<-import("CH3_fdat.xlsx")
combcouple<-import("combcouple_fdat.xlsx")


## ----------------------------------------------------------------------------
Dan.obs<-nrow(Dan)
Brit.obs<-nrow(Brit)
Chi2.obs<-nrow(Chi2)
CH3.obs<-nrow(CH3)
combcouple.obs<-nrow(combcouple)


## ----------------------------------------------------------------------------

Dan.s<-Dan %>%
  group_by(ID) %>%
  sample_n(size=1)

Brit.s<-Brit %>%
  group_by(ID) %>%
  sample_n(size=1)

Chi2.s<-Chi2 %>%
  group_by(ID) %>%
  sample_n(size=1)

CH3.s<-CH3 %>%
  group_by(ID) %>%
  sample_n(size=1)

combcouple.s<-combcouple %>%
  group_by(ID) %>%
  sample_n(size=1)


## ----------------------------------------------------------------------------
Dan.n<-nrow(Dan.s)
Brit.n<-nrow(Brit.s)
Chi2.n<-nrow(Chi2.s)
CH3.n<-nrow(CH3.s)
combcouple.n<-nrow(combcouple.s)


## ----------------------------------------------------------------------------
Dan.n
table(Dan.s$sex,useNA="always")/nrow(Dan.s)
describe(Dan.s$age,fast=T)
table(Dan.s$age)
table(Dan.s$age<18)
table(Dan.s$age<10)
#there is something wrong with the age variable, only code those >  17
describe(Dan.s$age[Dan.s$age>17],fast=T)

table(Dan.s$rela_status)


## ----------------------------------------------------------------------------
Brit.n
table(Brit.s$sex,useNA="always")/nrow(Brit.s)
describe(Brit.s$age,fast=T)

round(100*table(Brit.s$rela_status,useNA="always")/nrow(Brit.s),1)

describe(Brit.s$length,fast=T)


## ----------------------------------------------------------------------------
Chi2.n
table(Chi2.s$sex,useNA="always")/nrow(Chi2.s)
describe(Chi2.s$age,fast=T)

round(100*table(Chi2.s$relation_type,useNA="always")/nrow(Chi2.s),1)

describe(Chi2.s$length,fast=T)


## ----------------------------------------------------------------------------
CH3.n
table(CH3.s$sex,useNA="always")/nrow(CH3.s)
describe(CH3.s$age,fast=T)

round(100*table(CH3.s$relation_type,
                useNA="always")/nrow(CH3.s),1)

describe(CH3.s$length,fast=T)


## ----------------------------------------------------------------------------
combcouple.n
table(combcouple.s$sex,useNA="always")/nrow(combcouple.s)
describe(combcouple.s$age,fast=T)
names(combcouple.s)
#round(100*table(combcouple.s$relation_type,
#                useNA="always")/nrow(combcouple.s),1)

describe(combcouple.s$length,fast=T)


## ----------------------------------------------------------------------------
dir()

