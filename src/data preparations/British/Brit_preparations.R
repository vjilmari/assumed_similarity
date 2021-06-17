# Load packages
library(rio)
library(dplyr)
library(zoo)

# British data
# Load data
Brit_dat<-read.csv("data/raw/British/British1.csv",
                   stringsAsFactors = F,
                   header=T)

#look what the complete sample size would be with satisfaction and commitment

sr_items<-c(paste0("hex0",1:9),
            paste0("hex",10:60))
table(sr_items %in% names(Brit_dat))

pr_items<-c(paste0("phex0",1:9),
            paste0("phex",10:60))
table(sr_items %in% names(Brit_dat))

sr_domains<-c("HH","EM","EX","AG","CO","OP")
pr_domains<-paste0("p",sr_domains)

satis_items<-paste0("satis0",1:7)

commit_items<-paste0("commit0",1:7)

#exclude variables not needed
Brit_fdat<-Brit_dat %>%
  dplyr::select(sex,age,length,
                rela_status,
                sex_oritation,
                commit,satis,
                all_of(satis_items),all_of(commit_items),
                all_of(sr_items),all_of(pr_items),
                all_of(sr_domains),all_of(pr_domains))


table(is.na(Brit_fdat))  

#there are few missing values in this data file
#look where they are


for (i in 1:ncol(Brit_fdat)){
  na.count<-sum(is.na(Brit_fdat[,i]))
  
  if (na.count>0) {print(names(Brit_fdat)[i])}
}

#they are the moderator variables

#see if this would be the case also if some missingness would be allowed in these items
satis_items<-paste0("satis0",1:7)

Brit_satis_dat<-Brit_dat %>%
  dplyr::select(all_of(satis_items))

table(rowSums(is.na(Brit_satis_dat)))  

commit_items<-paste0("commit0",1:7)

Brit_commit_dat<-Brit_dat %>%
  dplyr::select(all_of(commit_items))

table(rowSums(is.na(Brit_commit_dat)))  


#yes, the values seem to be completely missing from satis and commit,

#exclude these from the Brit_fdat

Brit_fdat<-Brit_fdat[!is.na(Brit_fdat$satis) &
                       !is.na(Brit_fdat$commit),]

table(is.na(Brit_fdat))
#there are no missing values left

#remove the participants who were less than 18 years of age
Brit_fdat<-Brit_fdat[Brit_fdat$age>17,]


#screen the data for repetitive responses to personality items (>10)

p1<-rep(1,11)
p2<-rep(2,11)
p3<-rep(3,11)
p4<-rep(4,11)
p5<-rep(5,11)



searchX <- function(x, X) all(x==X)
#rollapply(v, FUN=searchX, X=x, width=length(x))

rep_sr<-matrix(ncol=5,nrow=nrow(Brit_fdat))

for (j in 1:nrow(Brit_fdat)){
  
  v <- c(as.matrix(Brit_fdat[j,sr_items]))
  
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


rep_pr<-matrix(ncol=5,nrow=nrow(Brit_fdat))

for (j in 1:nrow(Brit_fdat)){
  
  v <- c(as.matrix(Brit_fdat[j,pr_items]))
  
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

#there were no repetitive response partner ratings

export(Brit_fdat,"data/processed/British/Brit_fdat.xlsx")

#All the data should therefore be suitable for profile analysis

#Do the initial grand mean centering for all ratings

total_grand_mean<-mean(c(as.matrix(Brit_fdat[,c(sr_items,pr_items)])))

Brit_fdat[,c(sr_items,pr_items)]<-Brit_fdat[,c(sr_items,pr_items)]-total_grand_mean

#obtain normativeness profile based on self-ratings (and partner ratings)

norm_sr<-sapply(Brit_fdat[,sr_items],mean)
norm_pr<-sapply(Brit_fdat[,pr_items],mean)

cor(norm_sr,norm_pr)

#center the self-ratings around the item means

#save raw scores to another frame first
Brit_sr_items_raw<-Brit_fdat[,sr_items]
names(Brit_sr_items_raw)<-paste0("raw",sr_items)
sr_items_raw<-names(Brit_sr_items_raw)

head(Brit_fdat[,sr_items])

for (i in 1:nrow(Brit_fdat)){
  Brit_fdat[i,sr_items]<-Brit_fdat[i,sr_items]-norm_sr
}

#add the original raw ratings to the frame
Brit_fdat<-cbind(Brit_fdat,Brit_sr_items_raw)

#construct a separate set of partner ratings that are centered
#across the normative partner profiles
Brit_fdat_pr_dist<-data.frame(matrix(ncol=60,nrow=nrow(Brit_fdat)))

for (i in 1:nrow(Brit_fdat)){
  Brit_fdat_pr_dist[i,]<-Brit_fdat[i,pr_items]-norm_pr
}

names(Brit_fdat_pr_dist)<-paste0("d",pr_items)
head(Brit_fdat_pr_dist)

Brit_fdat<-cbind(Brit_fdat,Brit_fdat_pr_dist)

#compile profile analysis datafile

#add trait domains

domains<-rep(c("OE","CO","AG","EX","EM","HH"),times=10)

#do this for one participant at a time
Brit_long_fdat_list<-list()


for (i in 1:nrow(Brit_fdat)){
  dat<-Brit_fdat[i,]
  rownames(dat)<-NULL
  #pcdat<-Brit_fdat_pr_dist[i,]
  #rownames(pcdat)<-NULL
  #pcdat<-unname(pcdat)
  
  Brit_long_fdat_list[[i]]<-
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
                      "rela_status","sex_oritation",
                      "commit","satis",
                      sr_domains,pr_domains)])
               #sex=dat$sex,
               #age=dat$age,
               #rela_status=dat$rela_status,
               #sex_oritation=dat$sex_oritation,
               #sex_oritation=dat$commit,
               #sex_oritation=dat$satis)
}
names(Brit_long_fdat_list[[1]])
names(Brit_long_fdat_list[[1]])==names(Brit_long_fdat_list[[2]])

Brit_long_fdat<-do.call(rbind.data.frame,Brit_long_fdat_list)

#add within profile standardized variables
#add also within profile centered and standardized

norm_sr_sd<-sd(Brit_long_fdat[1:60,"Norm_sr"])

sd.SRc.dat<-Brit_long_fdat %>%
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
sd.PR.dat<-Brit_long_fdat %>%
  group_by(ID) %>%
  summarize(SD_PR=sd(PR))

mean_PR_sd<-mean(sd.PR.dat$SD_PR)



#Construct scaled variables

#partner ratings scaled with mean SD of partner ratings
Brit_long_fdat$PR.z<-Brit_long_fdat$PR/mean_PR_sd
#distinctive self-ratings scaled with mean SD of distinctive self-ratings
Brit_long_fdat$SRc.z<-Brit_long_fdat$SRc/mean_SRc_sd
#overall self-ratings scaled with mean SD of overall self-ratings
Brit_long_fdat$SR.z<-Brit_long_fdat$SR/mean_SR_sd
#normative profiles scaled with sd of normative profile
Brit_long_fdat$Norm_sr.z<-Brit_long_fdat$Norm_sr/norm_sr_sd

#then also scale within each profile
Brit_long_fdat<-
  left_join(x=Brit_long_fdat,
            y=sd.SRc.dat,
            by="ID")

Brit_long_fdat<-
  left_join(x=Brit_long_fdat,
            y=sd.PR.dat,
            by="ID")



#within profile centered standardized profiles
#this is an additional centering within profiles for self-ratings
Brit_long_fdat$SRc.zc<-(Brit_long_fdat$SRc-Brit_long_fdat$M_SRc)/mean_SRc_sd



#add standardized versions of satisfaction and commitment
#take mean and sd from the wide data

Brit_long_fdat$satis.z<-
  (Brit_long_fdat$satis-mean(Brit_fdat$satis))/sd(Brit_fdat$satis)

Brit_long_fdat$commit.z<-
  (Brit_long_fdat$commit-mean(Brit_fdat$commit))/sd(Brit_fdat$commit)


#save the long format data file used in profile analysis

export(Brit_long_fdat,"data/processed/British/Brit_long_fdat.xlsx")
