# Load packages 
library(rio)
library(dplyr)
library(zoo)

# Chi3b data
# Load data
Chi3b_fdat<-import("data/processed/Chinese 3/Chi3b_fdat.xlsx")
Chi3a_fdat<-import("data/processed/Chinese 3/Chi3a_fdat.xlsx")


#check if the names are identical and in identical order
table(names(Chi3b_fdat) == names(Chi3a_fdat),useNA="always")

#standardize satisfaction within sample (were measured with different methods)
Chi3b_fdat$satis.z<-
  (Chi3b_fdat$satis-mean(Chi3b_fdat$satis,na.rm=T))/
  sd(Chi3b_fdat$satis,na.rm=T)

#Chi3b_fdat$commit.z<-
#  (Chi3b_fdat$commit-mean(Chi3b_fdat$commit,na.rm=T))/
#  sd(Chi3b_fdat$commit,na.rm=T)

Chi3a_fdat$satis.z<-
  (Chi3a_fdat$satis-mean(Chi3a_fdat$satis,na.rm=T))/
  sd(Chi3a_fdat$satis,na.rm=T)

#Chi3a_fdat$commit.z<-
#  (Chi3a_fdat$commit-mean(Chi3a_fdat$commit,na.rm=T))/
#  sd(Chi3a_fdat$commit,na.rm=T)




#rbind the data-frames
Chi3comb_fdat<-rbind(Chi3b_fdat,
                       Chi3a_fdat)

# Add ID variable 

Chi3comb_fdat$ID<-paste0("ID",1:nrow(Chi3comb_fdat))

#look at the general presence of missing values

table(is.na(Chi3comb_fdat))  

#there are some missing values in this data file

for (i in 1:ncol(Chi3comb_fdat)){
  na.count<-sum(is.na(Chi3comb_fdat[,i]))
  
  if (na.count>0) {print(names(Chi3comb_fdat)[i])}
}

# some missingness

table(Chi3comb_fdat$satis,useNA="always")
table(Chi3comb_fdat$commit,useNA="always")


sr_items<-c(paste0("hex0",1:9),
            paste0("hex",10:60))
table(sr_items %in% names(Chi3comb_fdat))

pr_items<-c(paste0("phex0",1:9),
            paste0("phex",10:60))
table(pr_items %in% names(Chi3comb_fdat))

table(rowMeans(Chi3comb_fdat[,pr_items]),useNA="always")
table(rowMeans(Chi3comb_fdat[,sr_items]),useNA="always")


#exclude these from the Chi3comb_fdat

Chi3comb_fdat<-Chi3comb_fdat[!is.na(Chi3comb_fdat$satis) &
                                   !is.na(Chi3comb_fdat$commit)  &
                       !is.na(rowMeans(Chi3comb_fdat[,pr_items]))
                       &
                         !is.na(rowMeans(Chi3comb_fdat[,sr_items])),]

table(is.na(Chi3comb_fdat))

for (i in 1:ncol(Chi3comb_fdat)){
  na.count<-sum(is.na(Chi3comb_fdat[,i]))
  
  if (na.count>0) {print(names(Chi3comb_fdat)[i])}
}

#there are no missing values left (except for age )

#screen the data for repetitive responses to personality items (>10)

p1<-rep(1,11)
p2<-rep(2,11)
p3<-rep(3,11)
p4<-rep(4,11)
p5<-rep(5,11)


searchX <- function(x, X) all(x==X)
#rollapply(v, FUN=searchX, X=x, width=length(x))

rep_sr<-matrix(ncol=5,nrow=nrow(Chi3comb_fdat))

for (j in 1:nrow(Chi3comb_fdat)){
  
  v <- c(as.matrix(Chi3comb_fdat[j,sr_items]))
  
  rep_sr[j,1]<-
    sum(rollapply(v, FUN=searchX, X=p1, width=length(p1)))
  
  rep_sr[j,2]<-
    sum(rollapply(v, FUN=searchX, X=p2, width=length(p2)))
  
  rep_sr[j,3]<-
    sum(rollapply(v, FUN=searchX, X=p3, width=length(p3)))
  
  rep_sr[j,4]<-
    sum(rollapply(v, FUN=searchX, X=p4, width=length(p4)))
  
  rep_sr[j,5]<-
    sum(rollapply(v, FUN=searchX, X=p5, width=length(p5)))
  
  
}

table(rowSums(rep_sr))

#there are two repetitive responses in self ratings

rep_pr<-matrix(ncol=5,nrow=nrow(Chi3comb_fdat))

for (j in 1:nrow(Chi3comb_fdat)){
  
  v <- c(as.matrix(Chi3comb_fdat[j,pr_items]))
  
  rep_pr[j,1]<-
    sum(rollapply(v, FUN=searchX, X=p1, width=length(p1)))
  
  rep_pr[j,2]<-
    sum(rollapply(v, FUN=searchX, X=p2, width=length(p2)))
  
  rep_pr[j,3]<-
    sum(rollapply(v, FUN=searchX, X=p3, width=length(p3)))
  
  rep_pr[j,4]<-
    sum(rollapply(v, FUN=searchX, X=p4, width=length(p4)))
  
  rep_pr[j,5]<-
    sum(rollapply(v, FUN=searchX, X=p5, width=length(p5)))
  
  
}

table(rowSums(rep_pr))

#there are three repetitive response in partner ratings
Chi3comb_fdat[rowSums(rep_sr)!=0,sr_items]
Chi3comb_fdat[rowSums(rep_pr)!=0,pr_items]

#exclude these
Chi3comb_fdat<-Chi3comb_fdat[c(rowSums(rep_sr)==0 & rowSums(rep_pr)==0),]

#exclude the participants < 18
table(!(Chi3comb_fdat$age<18))

Chi3comb_fdat<-Chi3comb_fdat[!(Chi3comb_fdat$age<18),]

#descriptives
names(Chi3comb_fdat)
mean(Chi3comb_fdat$age,na.rm=T)
sd(Chi3comb_fdat$age,na.rm=T)
min(Chi3comb_fdat$age,na.rm=T)
max(Chi3comb_fdat$age,na.rm=T)

mean(Chi3comb_fdat$length,na.rm=T)
sd(Chi3comb_fdat$length,na.rm=T)
min(Chi3comb_fdat$length,na.rm=T)
max(Chi3comb_fdat$length,na.rm=T)

describe(Chi3comb_fdat,fast=T)

table(Chi3comb_fdat$sex)

#this needed to be rerun for some odd reason

table(is.na(Chi3comb_fdat))

for (i in 1:ncol(Chi3comb_fdat)){
  na.count<-sum(is.na(Chi3comb_fdat[,i]))
  
  if (na.count>0) {print(names(Chi3comb_fdat)[i])}
}

Chi3comb_fdat<-Chi3comb_fdat[!is.na(Chi3comb_fdat$satis) &
                                   !is.na(Chi3comb_fdat$commit)  &
                                   !is.na(rowMeans(Chi3comb_fdat[,pr_items]))
                                 &
                                   !is.na(rowMeans(Chi3comb_fdat[,sr_items])),]


#save subject-level data file

export(Chi3comb_fdat,"data/processed/Chinese 3/Chi3comb_fdat.xlsx")

#All the remaining data should be suitable for profile analysis

#Do the initial grand mean centering for all ratings

total_grand_mean<-mean(c(as.matrix(Chi3comb_fdat[,c(sr_items,pr_items)])))

Chi3comb_fdat[,c(sr_items,pr_items)]<-Chi3comb_fdat[,c(sr_items,pr_items)]-total_grand_mean

#obtain normative profile based on self-ratings (and partner ratings)

norm_sr<-sapply(Chi3comb_fdat[,sr_items],mean)
norm_pr<-sapply(Chi3comb_fdat[,pr_items],mean)

cor(norm_sr,norm_pr)

#center the self-ratings around the item means

#save raw scores to another frame first
Chi3comb_sr_items_raw<-Chi3comb_fdat[,sr_items]
names(Chi3comb_sr_items_raw)<-paste0("raw",sr_items)
sr_items_raw<-names(Chi3comb_sr_items_raw)

head(Chi3comb_fdat[,sr_items])

for (i in 1:nrow(Chi3comb_fdat)){
  Chi3comb_fdat[i,sr_items]<-Chi3comb_fdat[i,sr_items]-norm_sr
}

#add the original raw ratings to the frame
Chi3comb_fdat<-cbind(Chi3comb_fdat,Chi3comb_sr_items_raw)

#construct a separate set of partner ratings that are centered
#across the normative partner profiles
Chi3comb_fdat_pr_dist<-data.frame(matrix(ncol=60,nrow=nrow(Chi3comb_fdat)))

for (i in 1:nrow(Chi3comb_fdat)){
  Chi3comb_fdat_pr_dist[i,]<-Chi3comb_fdat[i,pr_items]-norm_pr
}

names(Chi3comb_fdat_pr_dist)<-paste0("d",pr_items)
head(Chi3comb_fdat_pr_dist)

Chi3comb_fdat<-cbind(Chi3comb_fdat,Chi3comb_fdat_pr_dist)


#compile profile analysis datafile

#add trait domains

domains<-rep(c("OE","CO","AG","EX","EM","HH"),times=10)

#do this for one participant at a time
Chi3comb_long_fdat_list<-list()
names(Chi3comb_fdat)

for (i in 1:nrow(Chi3comb_fdat)){
  dat<-Chi3comb_fdat[i,]
  rownames(dat)<-NULL
  #pcdat<-Chi3comb_fdat_pr_dist[i,]
  #rownames(pcdat)<-NULL
  #pcdat<-unname(pcdat)
  
  Chi3comb_long_fdat_list[[i]]<-
    data.frame(#ID=paste0("ID",i),
               item=sr_items,
               domain=domains,
               SRc=t(dat[,sr_items]),
               SR=t(dat[,sr_items_raw]),
               PR=t(dat[,pr_items]),
               PRc=t(dat[,paste0("d",pr_items)]),
               Norm_sr=norm_sr,
               Norm_pr=norm_pr,
               dat[,c("ID","couple_ID","sex","age","length",
                      "commit","satis","satis.z")])
}
names(Chi3comb_long_fdat_list[[1]])
names(Chi3comb_long_fdat_list[[1]])==names(Chi3comb_long_fdat_list[[2]])

Chi3comb_long_fdat<-do.call(rbind.data.frame,Chi3comb_long_fdat_list)

#add within profile standardized variables
#add also within profile centered and standardized

norm_sr_sd<-sd(Chi3comb_long_fdat[1:60,"Norm_sr"])

sd.SRc.dat<-Chi3comb_long_fdat %>%
  group_by(ID) %>%
  summarize(
    SD_SR=sd(SR),
    SD_SRc=sd(SRc),
    M_SRc=mean(SRc))

#calculate the average SD across distinctive self-rating profiles
mean_SRc_sd<-mean(sd.SRc.dat$SD_SRc)
#calculate the average SD across overall self-rating profiles
mean_SR_sd<-mean(sd.SRc.dat$SD_SR)

#check the variance ratio between distinctive and normative profiles
(mean_SRc_sd^2)/(norm_sr_sd^2)

#same for partner perceptions
sd.PR.dat<-Chi3comb_long_fdat %>%
  group_by(ID) %>%
  summarize(SD_PR=sd(PR))

mean_PR_sd<-mean(sd.PR.dat$SD_PR)



#Construct scaled variables

#partner ratings scaled with mean SD of partner ratings
Chi3comb_long_fdat$PR.z<-Chi3comb_long_fdat$PR/mean_PR_sd
#distinctive self-ratings scaled with mean SD of distinctive self-ratings
Chi3comb_long_fdat$SRc.z<-Chi3comb_long_fdat$SRc/mean_SRc_sd
#overall self-ratings scaled with mean SD of overall self-ratings
Chi3comb_long_fdat$SR.z<-Chi3comb_long_fdat$SR/mean_SR_sd
#normative profiles scaled with sd of normative profile
Chi3comb_long_fdat$Norm_sr.z<-Chi3comb_long_fdat$Norm_sr/norm_sr_sd

#then also scale within each profile
Chi3comb_long_fdat<-
  left_join(x=Chi3comb_long_fdat,
            y=sd.SRc.dat,
            by="ID")

Chi3comb_long_fdat<-
  left_join(x=Chi3comb_long_fdat,
            y=sd.PR.dat,
            by="ID")



#within profile standardized profiles

#within profile centered standardized profiles
#this is an additional centering within profiles for self-ratings
Chi3comb_long_fdat$SRc.zc<-(Chi3comb_long_fdat$SRc-Chi3comb_long_fdat$M_SRc)/mean_SRc_sd



#add standardized versions of commitment

#Chi3comb_long_fdat$satis.z<-
#  (Chi3comb_long_fdat$satis-mean(Chi3comb_fdat$satis))/sd(Chi3comb_fdat$satis)


Chi3comb_long_fdat$commit.z<-
  (Chi3comb_long_fdat$commit-mean(Chi3comb_fdat$commit))/sd(Chi3comb_fdat$commit)


#save the long format data file used in profile analysis

export(Chi3comb_long_fdat,"data/processed/Chinese 3/Chi3comb_long_fdat.xlsx")
