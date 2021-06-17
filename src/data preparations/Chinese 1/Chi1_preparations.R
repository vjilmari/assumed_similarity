# Load packages 
library(rio)
library(dplyr)
library(zoo)

# Chinese1 data
# Load data
Chi1_dat<-read.csv("data/raw/Chinese 1/Chinese1.csv",
                   stringsAsFactors = F,
                   header=T)
table(Chi1_dat$length,useNA = "always")

#exclude participants with zero relationship length
Chi1_dat<-Chi1_dat[Chi1_dat$length>0,]

#look what the complete sample size would be with satisfaction and commitment

sr_items<-c(paste0("hex0",1:9),
            paste0("hex",10:60))
table(sr_items %in% names(Chi1_dat))

pr_items<-c(paste0("phex0",1:9),
            paste0("phex",10:60))
table(sr_items %in% names(Chi1_dat))

sr_domains<-c("HH","EM","EX","AG","CO","OP")
pr_domains<-paste0("p",sr_domains)

satis_items<-paste0("satis0",1:7)

commit_items<-paste0("comit0",1:7)

#select needed variable
Chi1_fdat<-Chi1_dat %>%
  dplyr::select(sex,age,
                relation_type,
                length,
                commit,satis,
                all_of(satis_items),all_of(commit_items),
                all_of(sr_items),all_of(pr_items),
                all_of(sr_domains),all_of(pr_domains))


table(is.na(Chi1_fdat))  

#there are very few missing values in this data file
#look where they are

for (i in 1:ncol(Chi1_fdat)){
  na.count<-sum(is.na(Chi1_fdat[,i]))
  
  if (na.count>0) {print(names(Chi1_fdat)[i])}
}

#there are missing values among age, and the moderators

#see if this would be the case also if some missingness would be allowed in these items
satis_items<-paste0("satis0",1:7)

Chi1_satis_dat<-Chi1_dat %>%
  dplyr::select(all_of(satis_items))

table(rowSums(is.na(Chi1_satis_dat)))  

commit_items<-paste0("comit0",1:7)

Chi1_commit_dat<-Chi1_dat %>%
  dplyr::select(all_of(commit_items))

table(rowSums(is.na(Chi1_commit_dat)))  


#yes, the values seem to be completely missing from satis and commit,

#exclude these from the Chi1_fdat

Chi1_fdat<-Chi1_fdat[!is.na(Chi1_fdat$satis) &
                       !is.na(Chi1_fdat$commit),]

table(is.na(Chi1_fdat))
#there are no missing values left 

#screen the data for repetitive responses to personality items (>10)

p1<-rep(1,11)
p2<-rep(2,11)
p3<-rep(3,11)
p4<-rep(4,11)
p5<-rep(5,11)



searchX <- function(x, X) all(x==X)
#rollapply(v, FUN=searchX, X=x, width=length(x))

rep_sr<-matrix(ncol=5,nrow=nrow(Chi1_fdat))

for (j in 1:nrow(Chi1_fdat)){
  
  v <- c(as.matrix(Chi1_fdat[j,sr_items]))
  
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

#there are no repetitive responses in self ratings


rep_pr<-matrix(ncol=5,nrow=nrow(Chi1_fdat))

for (j in 1:nrow(Chi1_fdat)){
  
  v <- c(as.matrix(Chi1_fdat[j,pr_items]))
  
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

#there is one repetitive response in partner ratings
Chi1_fdat[rowSums(rep_pr)!=0,pr_items]
#exclude these
Chi1_fdat<-Chi1_fdat[rowSums(rep_pr)==0,]

#descriptives

table(Chi1_fdat$relation_type,useNA="always")
prop.table(table(Chi1_fdat$sex,useNA="always"))
mean(Chi1_fdat$age,na.rm=T)
sd(Chi1_fdat$age,na.rm=T)
min(Chi1_fdat$age,na.rm=T)
max(Chi1_fdat$age,na.rm=T)

mean(Chi1_fdat$length,na.rm=T)
sd(Chi1_fdat$length,na.rm=T)
min(Chi1_fdat$length,na.rm=T)
max(Chi1_fdat$length,na.rm=T)

table(Chi1_fdat$length)

#save the final subject level file

export(Chi1_fdat,"data/processed/Chinese 1/Chi1_fdat.xlsx")


#All the remaining data should be suitable for profile analysis

#Do the initial grand mean centering for all ratings

total_grand_mean<-mean(c(as.matrix(Chi1_fdat[,c(sr_items,pr_items)])))

Chi1_fdat[,c(sr_items,pr_items)]<-Chi1_fdat[,c(sr_items,pr_items)]-total_grand_mean

#obtain normative profile based on self-ratings (and partner ratings)

norm_sr<-sapply(Chi1_fdat[,sr_items],mean)
norm_pr<-sapply(Chi1_fdat[,pr_items],mean)

cor(norm_sr,norm_pr)

#center the self-ratings around the item means

#save raw scores to another frame first
Chi1_sr_items_raw<-Chi1_fdat[,sr_items]
names(Chi1_sr_items_raw)<-paste0("raw",sr_items)
sr_items_raw<-names(Chi1_sr_items_raw)

head(Chi1_fdat[,sr_items])

for (i in 1:nrow(Chi1_fdat)){
  Chi1_fdat[i,sr_items]<-Chi1_fdat[i,sr_items]-norm_sr
}

#add the original raw ratings to the frame
Chi1_fdat<-cbind(Chi1_fdat,Chi1_sr_items_raw)

#construct a separate set of partner ratings that are centered
#across the normative partner profiles
Chi1_fdat_pr_dist<-data.frame(matrix(ncol=60,nrow=nrow(Chi1_fdat)))

for (i in 1:nrow(Chi1_fdat)){
  Chi1_fdat_pr_dist[i,]<-Chi1_fdat[i,pr_items]-norm_pr
}

names(Chi1_fdat_pr_dist)<-paste0("d",pr_items)
head(Chi1_fdat_pr_dist)

Chi1_fdat<-cbind(Chi1_fdat,Chi1_fdat_pr_dist)

#compile profile analysis datafile

#add trait domains

domains<-rep(c("OE","CO","AG","EX","EM","HH"),times=10)

#do this for one participant at a time
Chi1_long_fdat_list<-list()


for (i in 1:nrow(Chi1_fdat)){
  dat<-Chi1_fdat[i,]
  rownames(dat)<-NULL
  #pcdat<-Chi1_fdat_pr_dist[i,]
  #rownames(pcdat)<-NULL
  #pcdat<-unname(pcdat)
  
  Chi1_long_fdat_list[[i]]<-
    data.frame(ID=paste0("ID",i),
               item=sr_items,
               domain=domains,
               SRc=t(dat[,sr_items]),
               SR=t(dat[,sr_items_raw]),
               PR=t(dat[,pr_items]),
               PRc=t(dat[,paste0("d",pr_items)]),
               Norm_sr=norm_sr,
               Norm_pr=norm_pr,
               dat[,c("sex","age","length",
                      "relation_type",
                      "commit","satis",
                      sr_domains,pr_domains)])
}
names(Chi1_long_fdat_list[[1]])
names(Chi1_long_fdat_list[[1]]) %in% names(Chi1_long_fdat_list[[2]])

Chi1_long_fdat<-do.call(rbind.data.frame,Chi1_long_fdat_list)

#add within profile standardized variables
#add also within profile centered and standardized

norm_sr_sd<-sd(Chi1_long_fdat[1:60,"Norm_sr"])

sd.SRc.dat<-Chi1_long_fdat %>%
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
sd.PR.dat<-Chi1_long_fdat %>%
  group_by(ID) %>%
  summarize(SD_PR=sd(PR))

mean_PR_sd<-mean(sd.PR.dat$SD_PR)



#Construct scaled variables

#partner ratings scaled with mean SD of partner ratings
Chi1_long_fdat$PR.z<-Chi1_long_fdat$PR/mean_PR_sd
#distinctive self-ratings scaled with mean SD of distinctive self-ratings
Chi1_long_fdat$SRc.z<-Chi1_long_fdat$SRc/mean_SRc_sd
#overall self-ratings scaled with mean SD of overall self-ratings
Chi1_long_fdat$SR.z<-Chi1_long_fdat$SR/mean_SR_sd
#normative profiles scaled with sd of normative profile
Chi1_long_fdat$Norm_sr.z<-Chi1_long_fdat$Norm_sr/norm_sr_sd

#then also scale within each profile
Chi1_long_fdat<-
  left_join(x=Chi1_long_fdat,
            y=sd.SRc.dat,
            by="ID")

Chi1_long_fdat<-
  left_join(x=Chi1_long_fdat,
            y=sd.PR.dat,
            by="ID")




#within profile centered standardized profiles
#this is an additional centering within profiles for self-ratings
Chi1_long_fdat$SRc.zc<-(Chi1_long_fdat$SRc-Chi1_long_fdat$M_SRc)/mean_SRc_sd



#add standardized versions of satisfaction and commitment
#take mean and sd from the wide data

Chi1_long_fdat$satis.z<-
  (Chi1_long_fdat$satis-mean(Chi1_fdat$satis))/sd(Chi1_fdat$satis)

Chi1_long_fdat$commit.z<-
  (Chi1_long_fdat$commit-mean(Chi1_fdat$commit))/sd(Chi1_fdat$commit)


#save the long format data file used in profile analysis

export(Chi1_long_fdat,"data/processed/Chinese 1/Chi1_long_fdat.xlsx")
