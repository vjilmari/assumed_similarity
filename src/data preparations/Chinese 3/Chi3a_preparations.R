# Load packages 
library(rio)
library(dplyr)
library(zoo)
library(psych)
source("src/custom functions/custom_functions.R")


# Chi3a data
# Load data
Chi3a_dat<-read.csv("data/raw/Chinese 3/Chi3a.csv",
                      stringsAsFactors = F,
                      header=T)

#code relationship length

Chi3a_dat$length<-Chi3a_dat$year.x*12+Chi3a_dat$month.x


# calculate commit and satis

satis.x.vars<-names(Chi3a_dat)[grepl("satis",names(Chi3a_dat)) &
                     grepl(".x",names(Chi3a_dat))]

satis.y.vars<-names(Chi3a_dat)[grepl("satis",names(Chi3a_dat)) &
                                   grepl(".y",names(Chi3a_dat))]

describe(Chi3a_dat[,satis.x.vars],fast=T)
cor(Chi3a_dat[,satis.x.vars])
describe(Chi3a_dat[,satis.y.vars],fast=T)
cor(Chi3a_dat[,satis.y.vars],use="pairwise.complete.obs")

Chi3a_dat$satis.x<-rowMeans(Chi3a_dat[,satis.x.vars],na.rm=T)
Chi3a_dat$satis.y<-rowMeans(Chi3a_dat[,satis.y.vars],na.rm=T)

cor(Chi3a_dat$satis.x,
    Chi3a_dat$satis.y,use="pairwise.complete.obs")



commit.x.vars<-names(Chi3a_dat)[grepl("commit",names(Chi3a_dat)) &
                                   grepl(".x",names(Chi3a_dat))]

commit.y.vars<-names(Chi3a_dat)[grepl("commit",names(Chi3a_dat)) &
                                   grepl(".y",names(Chi3a_dat))]

describe(Chi3a_dat[,commit.x.vars],fast=T)
cor(Chi3a_dat[,commit.x.vars])
describe(Chi3a_dat[,commit.y.vars],fast=T)
cor(Chi3a_dat[,commit.y.vars],use="pairwise.complete.obs")

Chi3a_dat$commit03.x<-10-Chi3a_dat$commit03.x
Chi3a_dat$commit04.x<-10-Chi3a_dat$commit04.x

Chi3a_dat$commit03.y<-10-Chi3a_dat$commit03.y
Chi3a_dat$commit04.y<-10-Chi3a_dat$commit04.y

round(cor(Chi3a_dat[,commit.x.vars]),2)
round(cor(Chi3a_dat[,commit.y.vars],use="pairwise.complete.obs"),2)

#recode the response scale from 1 to 5
Chi3a_dat[,commit.x.vars]<-
  sapply(Chi3a_dat[,commit.x.vars],rescale.commit)

describe(Chi3a_dat[,commit.x.vars],fast=T)

Chi3a_dat[,commit.y.vars]<-
  sapply(Chi3a_dat[,commit.y.vars],rescale.commit)

describe(Chi3a_dat[,commit.y.vars],fast=T)

Chi3a_dat$commit.x<-rowMeans(Chi3a_dat[,commit.x.vars],na.rm=T)
Chi3a_dat$commit.y<-rowMeans(Chi3a_dat[,commit.y.vars],na.rm=T)

cor(Chi3a_dat$commit.x,
    Chi3a_dat$commit.y,use="pairwise.complete.obs")



#look what the complete sample size would be with satisfaction and commitment
names(Chi3a_dat)
sr_items.x<-c(paste0("hex0",1:9,".x"),
            paste0("hex",10:60,".x"))
table(sr_items.x %in% names(Chi3a_dat))

pr_items.x<-c(paste0("phex0",1:9,".x"),
            paste0("phex",10:60,".x"))
table(pr_items.x %in% names(Chi3a_dat))

sr_items.y<-c(paste0("hex0",1:9,".y"),
              paste0("hex",10:60,".y"))
table(sr_items.y %in% names(Chi3a_dat))

pr_items.y<-c(paste0("phex0",1:9,".y"),
              paste0("phex",10:60,".y"))
table(pr_items.y %in% names(Chi3a_dat))

names(Chi3a_dat)
Chi3a_dat$couple_ID<-paste0("CP",1:nrow(Chi3a_dat))

#compile data into stacked format

Chi3a_fdat<-data.frame(
  couple_ID=c(Chi3a_dat$couple_ID,Chi3a_dat$couple_ID))

Chi3a_fdat$sex=c(Chi3a_dat$sex.x,Chi3a_dat$sex.y)

Chi3a_fdat$age=c(Chi3a_dat$age.x,Chi3a_dat$age.y)
Chi3a_fdat$satis=c(Chi3a_dat$satis.x,Chi3a_dat$satis.y)
Chi3a_fdat$commit=c(Chi3a_dat$commit.x,Chi3a_dat$commit.y)
Chi3a_fdat$length=c(Chi3a_dat$length,Chi3a_dat$length)


Chi3a_fdat<-cbind(Chi3a_fdat,
                    as.data.frame(mapply(c, 
                                         Chi3a_dat[,sr_items.x],
                                         Chi3a_dat[,sr_items.y])))

Chi3a_fdat<-cbind(Chi3a_fdat,
                    as.data.frame(mapply(c, 
                                         Chi3a_dat[,pr_items.x],
                                         Chi3a_dat[,pr_items.y])))

names(Chi3a_fdat)

sr_items<-c(paste0("hex0",1:9),
              paste0("hex",10:60))
table(sr_items %in% names(Chi3a_dat))

pr_items<-c(paste0("phex0",1:9),
              paste0("phex",10:60))
table(pr_items %in% names(Chi3a_dat))


names(Chi3a_fdat)[names(Chi3a_fdat) %in% sr_items.x]<-sr_items
names(Chi3a_fdat)[names(Chi3a_fdat) %in% pr_items.x]<-pr_items

names(Chi3a_fdat)

export(Chi3a_fdat,"data/processed/Chinese 3/Chi3a_fdat.xlsx")

#make a version that also includes the satisfaction and commitment items

Chi3a_fdat$satis01=c(Chi3a_dat$satis01.x,Chi3a_dat$satis01.y)
Chi3a_fdat$satis02=c(Chi3a_dat$satis02.x,Chi3a_dat$satis02.y)
Chi3a_fdat$satis03=c(Chi3a_dat$satis03.x,Chi3a_dat$satis03.y)
Chi3a_fdat$satis04=c(Chi3a_dat$satis04.x,Chi3a_dat$satis04.y)
Chi3a_fdat$satis05=c(Chi3a_dat$satis05.x,Chi3a_dat$satis05.y)

Chi3a_fdat$commit01=c(Chi3a_dat$commit01.x,Chi3a_dat$commit01.y)
Chi3a_fdat$commit02=c(Chi3a_dat$commit02.x,Chi3a_dat$commit02.y)
Chi3a_fdat$commit03=c(Chi3a_dat$commit03.x,Chi3a_dat$commit03.y)
Chi3a_fdat$commit04=c(Chi3a_dat$commit04.x,Chi3a_dat$commit04.y)
Chi3a_fdat$commit05=c(Chi3a_dat$commit05.x,Chi3a_dat$commit05.y)
Chi3a_fdat$commit06=c(Chi3a_dat$commit06.x,Chi3a_dat$commit06.y)
Chi3a_fdat$commit07=c(Chi3a_dat$commit07.x,Chi3a_dat$commit07.y)

export(Chi3a_fdat,"data/processed/Chinese 3/Chi3a_fdat_relscales.xlsx")

# Look for Chi3comb_preparations where these couple files are combined