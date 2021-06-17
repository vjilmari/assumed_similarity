# Load packages 

library(rio)
library(dplyr)
library(zoo)
library(psych)

# Dan data
# Load raw data
Dan_dat<-read.csv("data/raw/Danish/Danishdata.csv",
                   stringsAsFactors = F,
                   header=T)
str(Dan_dat)
names(Dan_dat)

table(Dan_dat$rela_status,useNA = "always")
table(Dan_dat$gender,useNA = "always")
table(Dan_dat$length,useNA = "always")

#exclude relationship length of 0
Dan_dat<-Dan_dat[Dan_dat$length!=0,]

#code self- and partner-rating items
sr_items<-c(paste0("hex0",1:9),
            paste0("hex",10:60))
table(sr_items %in% names(Dan_dat))

pr_items<-c(paste0("cphex0",1:9),
            paste0("cphex",10:60))
table(sr_items %in% names(Dan_dat))

#domain names
sr_domains<-c("HH","EM","EX","AG","CO","OP")
pr_domains<-paste0("cp",sr_domains)
names(Dan_dat)

#exclude unnecessary variables from the profile data
Dan_fdat<-Dan_dat %>%
  dplyr::select(sex=gender,age,
                rela_status,length,
                all_of(sr_items),all_of(pr_items),
                all_of(sr_domains),all_of(pr_domains))


table(is.na(Dan_fdat))  

#there are very no missing values in this data file


for (i in 1:ncol(Dan_fdat)){
  na.count<-sum(is.na(Dan_fdat[,i]))
  
  if (na.count>0) {print(names(Dan_fdat)[i])}
}


#screen the data for repetitive responses to personality items (>10)

p1<-rep(1,11)
p2<-rep(2,11)
p3<-rep(3,11)
p4<-rep(4,11)
p5<-rep(5,11)


#search function
searchX <- function(x, X) all(x==X)
#rollapply(v, FUN=searchX, X=x, width=length(x))

rep_sr<-matrix(ncol=5,nrow=nrow(Dan_fdat))

for (j in 1:nrow(Dan_fdat)){
  
  v <- c(as.matrix(Dan_fdat[j,sr_items]))
  
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


rep_pr<-matrix(ncol=5,nrow=nrow(Dan_fdat))

for (j in 1:nrow(Dan_fdat)){
  
  v <- c(as.matrix(Dan_fdat[j,pr_items]))
  
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

#there are also no repetitive response in partner ratings
Dan_fdat[rowSums(rep_sr)!=0,sr_items]
Dan_fdat[rowSums(rep_pr)!=0,pr_items]

#exclude these
Dan_fdat<-Dan_fdat[rowSums(rep_sr)==0 & rowSums(rep_pr)==0,]

#export processed wide data file
export(Dan_fdat,"data/processed/Danish/Dan_fdat.xlsx")

#All the remaining data should be suitable for profile analysis

#Do the initial grand mean centering for all ratings

total_grand_mean<-mean(c(as.matrix(Dan_fdat[,c(sr_items,pr_items)])))

Dan_fdat[,c(sr_items,pr_items)]<-Dan_fdat[,c(sr_items,pr_items)]-total_grand_mean

#obtain normative profile based on self-ratings (and partner ratings)

norm_sr<-sapply(Dan_fdat[,sr_items],mean)
norm_pr<-sapply(Dan_fdat[,pr_items],mean)

cor(norm_sr,norm_pr)

#center the self-ratings around the item means

#save raw scores to another frame first
Dan_sr_items_raw<-Dan_fdat[,sr_items]
names(Dan_sr_items_raw)<-paste0("raw",sr_items)
sr_items_raw<-names(Dan_sr_items_raw)

head(Dan_fdat[,sr_items])

#loop through each subject (row)
for (i in 1:nrow(Dan_fdat)){
  Dan_fdat[i,sr_items]<-Dan_fdat[i,sr_items]-norm_sr
}

#add the original raw ratings to the frame
Dan_fdat<-cbind(Dan_fdat,Dan_sr_items_raw)

#construct a separate set of partner ratings that are centered
#across the normative partner profiles
Dan_fdat_pr_dist<-data.frame(matrix(ncol=60,nrow=nrow(Dan_fdat)))

for (i in 1:nrow(Dan_fdat)){
  Dan_fdat_pr_dist[i,]<-Dan_fdat[i,pr_items]-norm_pr
}

names(Dan_fdat_pr_dist)<-paste0("d",pr_items)
head(Dan_fdat_pr_dist)

Dan_fdat<-cbind(Dan_fdat,Dan_fdat_pr_dist)

#compile profile analysis datafile

#add trait domains

domains<-rep(c("OE","CO","AG","EX","EM","HH"),times=10)

#do this for one participant at a time
Dan_long_fdat_list<-list()


for (i in 1:nrow(Dan_fdat)){
  dat<-Dan_fdat[i,]
  rownames(dat)<-NULL
  #pcdat<-Dan_fdat_pr_dist[i,]
  #rownames(pcdat)<-NULL
  #pcdat<-unname(pcdat)
  
  Dan_long_fdat_list[[i]]<-
    data.frame(ID=paste0("ID",i),
               item=sr_items,
               domain=domains,
               SRc=t(dat[,sr_items]),
               SR=t(dat[,sr_items_raw]),
               PR=t(dat[,pr_items]),
               PRc=t(dat[,paste0("d",pr_items)]),
               Norm_sr=norm_sr,
               Norm_pr=norm_pr,
               dat[,c("sex","age",
                      "rela_status",#"same_sex",
                      #"commit","satis","responsive",
                      sr_domains,pr_domains)])
               #sex=dat$sex,
               #age=dat$age,
               #relation_type=dat$relation_type,
               #sex_oritation=dat$sex_oritation,
               #sex_oritation=dat$commit,
               #sex_oritation=dat$satis)
}

names(Dan_long_fdat_list[[1]])
#check if the names were correctly coded
names(Dan_long_fdat_list[[1]])==names(Dan_long_fdat_list[[2]])

#save to a data frame
Dan_long_fdat<-do.call(rbind.data.frame,Dan_long_fdat_list)

#add within profile standardized variables
#add also within profile centered and standardized

norm_sr_sd<-sd(Dan_long_fdat[1:60,"Norm_sr"])

sd.SRc.dat<-Dan_long_fdat %>%
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
sd.PR.dat<-Dan_long_fdat %>%
  group_by(ID) %>%
  summarize(SD_PR=sd(PR))

mean_PR_sd<-mean(sd.PR.dat$SD_PR)



#Construct scaled variables

#partner ratings scaled with mean SD of partner ratings
Dan_long_fdat$PR.z<-Dan_long_fdat$PR/mean_PR_sd
#distinctive self-ratings scaled with mean SD of distinctive self-ratings
Dan_long_fdat$SRc.z<-Dan_long_fdat$SRc/mean_SRc_sd
#overall self-ratings scaled with mean SD of overall self-ratings
Dan_long_fdat$SR.z<-Dan_long_fdat$SR/mean_SR_sd
#normative profiles scaled with sd of normative profile
Dan_long_fdat$Norm_sr.z<-Dan_long_fdat$Norm_sr/norm_sr_sd

#then also scale within each profile
Dan_long_fdat<-
  left_join(x=Dan_long_fdat,
            y=sd.SRc.dat,
            by="ID")

Dan_long_fdat<-
  left_join(x=Dan_long_fdat,
            y=sd.PR.dat,
            by="ID")



#within profile centered standardized profiles
#this is an additional centering within profiles for self-ratings
Dan_long_fdat$SRc.zc<-(Dan_long_fdat$SRc-Dan_long_fdat$M_SRc)/mean_SRc_sd




#save the long format data file used in profile analysis

export(Dan_long_fdat,"data/processed/Danish/Dan_long_fdat.xlsx")
