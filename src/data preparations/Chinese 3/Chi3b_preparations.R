# Load packages 
library(rio)
library(dplyr)
library(zoo)
library(psych)

# Chi3b data
# Load data
Chi3b_dat<-read.csv("data/raw/Chinese 3/Chi3b.csv",
                   stringsAsFactors = F,
                   header=T)

Chi3b_dat$couple_ID<-paste0("CPB",Chi3b_dat$dyad)
Chi3b_dat$sex<-Chi3b_dat$person

#look what the complete sample size would be with satisfaction and commitment
names(Chi3b_dat)
sr_items<-c(paste0("hex0",1:9),
            paste0("hex",10:60))
table(sr_items %in% names(Chi3b_dat))

pr_items<-c(paste0("phex0",1:9),
            paste0("phex",10:60))
table(pr_items %in% names(Chi3b_dat))

Chi3b_fdat<-
  Chi3b_dat %>%
  dplyr::select(couple_ID,
                sex,
                age,
                satis,
                commit,
                length,
                all_of(sr_items),
                all_of(pr_items))

describe(Chi3b_fdat,fast=T)
#recode the -99 values as missing
Chi3b_fdat[Chi3b_fdat==-99]<-NA
#check
describe(Chi3b_fdat,fast=T)
#save the data file
export(Chi3b_fdat,"data/processed/Chinese 3/Chi3b_fdat.xlsx")

#make a different file for the relationship variables scales

satis_items<-paste0("satis",1:7)
commit_items<-paste0("commit",1:7)

Chi3b_fdat_relscales<-
  Chi3b_dat %>%
  dplyr::select(couple_ID,
                sex,
                age,
                satis,
                commit,
                length,
                all_of(satis_items),all_of(commit_items),
                all_of(sr_items),
                all_of(pr_items))

export(Chi3b_fdat_relscales,"data/processed/Chinese 3/Chi3b_fdat_relscales.xlsx")

#The rest of the editing happens in Chi3comb_preparations