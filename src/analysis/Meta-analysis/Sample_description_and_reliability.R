## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE--------------------------------------------
library(rio)
library(psych)
library(dplyr)


## ----------------------------------------------------------------------------

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


## ----------------------------------------------------------------------------
nrow(Dan)
round(100*table(Dan$sex,useNA="always")/nrow(Dan),1)
describe(Dan$age,fast=T)
Dan$real.age<-Dan$age+17
describe(Dan$real.age,fast=T)


describe(Dan$length,fast=T)


## ----------------------------------------------------------------------------
nrow(Brit)
table(Brit$sex,useNA="always")/nrow(Brit)
describe(Brit$age,fast=T)

round(100*table(Brit$rela_status,useNA="always")/nrow(Brit),1)

describe(Brit$length,fast=T)


## ----------------------------------------------------------------------------
nrow(Chi1)
table(Chi1$sex,useNA="always")/nrow(Chi1)
describe(Chi1$age,fast=T)

round(100*table(Chi1$relation_type,useNA="always")/nrow(Chi1),1)

describe(Chi1$length,fast=T)


## ----------------------------------------------------------------------------
nrow(Chi2)
table(Chi2$sex,useNA="always")/nrow(Chi2)
describe(Chi2$age,fast=T)

round(100*table(Chi2$relation_type,
                useNA="always")/nrow(Chi2),1)

describe(Chi2$length,fast=T)


## ----------------------------------------------------------------------------
nrow(Chi3comb)
table(Chi3comb$sex,useNA="always")/nrow(Chi3comb)
describe(Chi3comb$age,fast=T)

#round(100*table(Chi3comb$relation_type,
#                useNA="always")/nrow(Chi3comb),1)

describe(Chi3comb$length,fast=T)


## ----------------------------------------------------------------------------

Dan$age<-Dan$real.age

total.dat<-
  rbind(Dan[,c("sex","age","length")],
        Brit[,c("sex","age","length")],
        Chi1[,c("sex","age","length")],
        Chi2[,c("sex","age","length")],
        Chi3comb[,c("sex","age","length")])


nrow(total.dat)
round(100*table(total.dat$sex,useNA="always")/
        nrow(total.dat),1)

describe(total.dat$age,fast=T)
describe(total.dat$length,fast=T)



## ----------------------------------------------------------------------------

OP.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=01, to =60,by=6))<10,
                paste0("0",c(seq(from=01, to =60,by=6))),
                c(seq(from=01, to =60,by=6))))

OP.sr.vars
OP.sr.revs<-c(1,4,6,9,10)

OP.pr.vars1<-paste0("cp",OP.sr.vars)
OP.pr.vars2<-paste0("p",OP.sr.vars)

CO.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=02, to =60,by=6))<10,
                paste0("0",c(seq(from=02, to =60,by=6))),
                c(seq(from=02, to =60,by=6))))

CO.sr.vars
CO.sr.revs<-c(3,4,5,6,8,10)

CO.pr.vars1<-paste0("cp",CO.sr.vars)
CO.pr.vars2<-paste0("p",CO.sr.vars)

AG.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=03, to =60,by=6))<10,
                paste0("0",c(seq(from=03, to =60,by=6))),
                c(seq(from=03, to =60,by=6))))

AG.sr.vars
AG.sr.revs<-c(2,3,4,10)

AG.pr.vars1<-paste0("cp",AG.sr.vars)
AG.pr.vars2<-paste0("p",AG.sr.vars)

EX.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=04, to =60,by=6))<10,
                paste0("0",c(seq(from=04, to =60,by=6))),
                c(seq(from=04, to =60,by=6))))

EX.sr.vars
EX.sr.revs<-c(2,5,8,9)

EX.pr.vars1<-paste0("cp",EX.sr.vars)
EX.pr.vars2<-paste0("p",EX.sr.vars)

EM.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=05, to =60,by=6))<10,
                paste0("0",c(seq(from=05, to =60,by=6))),
                c(seq(from=05, to =60,by=6))))

EM.sr.vars
EM.sr.revs<-c(6,7,9,10)

EM.pr.vars1<-paste0("cp",EM.sr.vars)
EM.pr.vars2<-paste0("p",EM.sr.vars)

HH.sr.vars<-
  paste0("hex",
         ifelse(c(seq(from=06, to =60,by=6))<10,
                paste0("0",c(seq(from=06, to =60,by=6))),
                c(seq(from=06, to =60,by=6))))

HH.sr.vars
HH.sr.revs<-c(2,4,5,7,8,10)

HH.pr.vars1<-paste0("cp",HH.sr.vars)
HH.pr.vars2<-paste0("p",HH.sr.vars)




## ----------------------------------------------------------------------------
round(alpha(Dan[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,HH.sr.vars],
      keys = HH.sr.vars[HH.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,EM.sr.vars],
      keys = EM.sr.vars[EM.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,EX.sr.vars],
      keys = EX.sr.vars[EX.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,AG.sr.vars],
      keys = AG.sr.vars[AG.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,CO.sr.vars],
      keys = CO.sr.vars[CO.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,OP.sr.vars],
      keys = OP.sr.vars[OP.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,HH.pr.vars1],
      keys = HH.pr.vars1[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,HH.pr.vars2],
      keys = HH.pr.vars2[HH.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,EM.pr.vars1],
      keys = EM.pr.vars1[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,EM.pr.vars2],
      keys = EM.pr.vars2[EM.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,EX.pr.vars1],
      keys = EX.pr.vars1[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,EX.pr.vars2],
      keys = EX.pr.vars2[EX.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,AG.pr.vars1],
      keys = AG.pr.vars1[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,AG.pr.vars2],
      keys = AG.pr.vars2[AG.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,CO.pr.vars1],
      keys = CO.pr.vars1[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,CO.pr.vars2],
      keys = CO.pr.vars2[CO.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
round(alpha(Dan[,OP.pr.vars1],
      keys = OP.pr.vars1[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Brit[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Chi1[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Chi2[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)

round(alpha(Chi3comb[,OP.pr.vars2],
      keys = OP.pr.vars2[OP.sr.revs])$total$raw_alpha,2)



## ----------------------------------------------------------------------------
satis.vars<-paste0("satis0",1:7)
satis.revs<-c("satis04","satis07")

round(alpha(Brit[,satis.vars],
      keys = satis.revs)$total$raw_alpha,2)


## ----------------------------------------------------------------------------

satis.vars<-paste0("satis0",1:7)
satis.revs<-c("satis04","satis07")

round(alpha(Chi1[,satis.vars],
      keys = satis.revs)$total$raw_alpha,2)


## ----------------------------------------------------------------------------

satis.vars<-paste0("satis0",1:5)
round(alpha(Chi2[,satis.vars])$total$raw_alpha,2)


## ----------------------------------------------------------------------------

Chi3a<-
  import("../../../data/processed/Chinese 3/Chi3a_fdat_relscales.xlsx")

satis.vars<-paste0("satis0",1:5)
round(alpha(Chi3a[,satis.vars])$total$raw_alpha,2)


## ----------------------------------------------------------------------------
Chi3b<-
  import("../../../data/processed/Chinese 3/Chi3b_fdat_relscales.xlsx")

satis.vars<-paste0("satis",1:7)
satis.keys<-c("satis3","satis4")
round(alpha(Chi3b[,satis.vars],
            keys=satis.keys)$total$raw_alpha,2)


## ----------------------------------------------------------------------------

commit.vars<-paste0("commit0",1:7)
commit.revs<-c("commit03","commit04")

round(alpha(Brit[,commit.vars],
      keys = commit.revs)$total$raw_alpha,2)




## ----------------------------------------------------------------------------

commit.vars<-paste0("comit0",1:7)
commit.revs<-c("comit03","comit04")

round(alpha(Chi1[,commit.vars],
      keys = commit.revs)$total$raw_alpha,2)


## ----------------------------------------------------------------------------
commit.vars<-paste0("commit0",1:7)
commit.revs<-c("commit03","commit04")


round(alpha(Chi2[,commit.vars],
      keys = commit.revs)$total$raw_alpha,2)


## ----------------------------------------------------------------------------
commit.vars<-paste0("commit0",1:7)

round(alpha(Chi3a[,commit.vars])$total$raw_alpha,2)


## ----------------------------------------------------------------------------
commit.vars<-paste0("commit",1:7)
#recode -99 to NA
Chi3b[,commit.vars][Chi3b[,commit.vars]==-99]<-NA

commit.vars<-paste0("commit",1:7)
commit.keys<-c("commit4","commit7")
round(alpha(Chi3b[,commit.vars],
            keys=commit.keys)$total$raw_alpha,2)


## ----------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

