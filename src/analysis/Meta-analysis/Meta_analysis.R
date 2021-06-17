## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE--------------------------------------------
library(metafor)
library(dplyr)
library(rio)
library(finalfit)
library(multcomp)
source("../../custom functions/custom_functions.R")



## ----------------------------------------------------------------------------

Brit.Main<-
  import("../../../output/British/Brit.Main.xlsx")
Chi1.Main<-
  import("../../../output/Chinese 1/Chi1.Main.xlsx")
Chi2.Main<-
  import("../../../output/Chinese 2/Chi2.Main.xlsx")
Chi3comb.Main<-
  import("../../../output/Chinese 3/Chi3comb.Main.xlsx")
Dan.Main<-
  import("../../../output/Danish/Dan.Main.xlsx")

#combine
Main<-
  data.frame(rbind(
    Brit.Main,
    Chi1.Main,
    Chi2.Main,
    Chi3comb.Main,
    Dan.Main),
    sample=rep(c("British",
                 "Chinese 1",
                 "Chinese 2",
                 "Chinese 3",
                 "Danish"),each=3),
    country=rep(c("Britain","China","China","China",
                  "Denmark"),each=3))

#export
export(Main,
       "../../../data/processed/Meta-analysis/Main.Dist.xlsx")


## ----------------------------------------------------------------------------
(rma.Dist<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc",
        data=Main,
        method="ML"))


## ----------------------------------------------------------------------------
(rma.Dist.by.country<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc",
        mods=~country-1,
        data=Main,
        method="ML"))

anova(rma.Dist.by.country,rma.Dist)

summary(glht(rma.Dist.by.country,
             linfct=cbind(contrMat(rep(1,3),type="Tukey"))),
        test=adjusted("none"))

summary(glht(rma.Dist.by.country,
             linfct=cbind(contrMat(rep(1,3),type="Tukey"))),
        test=adjusted("holm"))


## ----------------------------------------------------------------------------
Brit.satis<-
  import("../../../output/British/Brit.satis.xlsx")
Chi1.satis<-
  import("../../../output/Chinese 1/Chi1.satis.xlsx")
Chi2.satis<-
  import("../../../output/Chinese 2/Chi2.satis.xlsx")
Chi3comb.satis<-
  import("../../../output/Chinese 3/Chi3comb.satis.xlsx")

#combine
satis<-
  data.frame(rbind(
    Brit.satis,
    Chi1.satis,
    Chi2.satis,
    Chi3comb.satis),
    sample=rep(c("British","Chinese 1",
                 "Chinese 2","Chinese 3"),each=6),
    country=rep(c("Britain","China","China","China"),each=6))

#export
export(satis,
       "../../../data/processed/Meta-analysis/satis.Dist.xlsx")


## ----------------------------------------------------------------------------
(rma.Dist.satis<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc:satis.z",
        data=satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.satis.by.country<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc:satis.z",
        mods=~country,
        data=satis,
        method="ML"))

(rma.Dist.satis.by.country<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc:satis.z",
        mods=~country-1,
        data=satis,
        method="ML"))


## ----------------------------------------------------------------------------
Brit.commit<-
  import("../../../output/British/Brit.commit.xlsx")
Chi1.commit<-
  import("../../../output/Chinese 1/Chi1.commit.xlsx")
Chi2.commit<-
  import("../../../output/Chinese 2/Chi2.commit.xlsx")
Chi3comb.commit<-
  import("../../../output/Chinese 3/Chi3comb.commit.xlsx")

#combine
commit<-
  data.frame(rbind(
    Brit.commit,
    Chi1.commit,
    Chi2.commit,
    Chi3comb.commit),
    sample=rep(c("British","Chinese 1",
                 "Chinese 2","Chinese 3"),each=6),
    country=rep(c("Britain","China","China","China"),each=6))

#export
export(commit,
       "../../../data/processed/Meta-analysis/commit.Dist.xlsx")


## ----------------------------------------------------------------------------
(rma.Dist.commit<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc:commit.z",
        data=commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.commit.by.country<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc:commit.z",
        mods=~country,
        data=commit,
        method="ML"))

(rma.Dist.commit.by.country<-rma.uni(yi = Estimate,
        sei=Std..Error,
        subset=...1=="SRc.zc:commit.z",
        mods=~country-1,
        data=commit,
        method="ML"))


## ----------------------------------------------------------------------------
Brit.HEXACO.Dist<-
  import("../../../output/British/Brit.HEXACO.Dist.xlsx")
Chi1.HEXACO.Dist<-
  import("../../../output/Chinese 1/Chi1.HEXACO.Dist.xlsx")
Chi2.HEXACO.Dist<-
  import("../../../output/Chinese 2/Chi2.HEXACO.Dist.xlsx")
Chi3comb.HEXACO.Dist<-
  import("../../../output/Chinese 3/Chi3comb.HEXACO.Dist.xlsx")
Dan.HEXACO.Dist<-
  import("../../../output/Danish/Dan.HEXACO.Dist.xlsx")

#combine
HEXACO.Dist<-
  data.frame(rbind(
    Brit.HEXACO.Dist,
    Chi1.HEXACO.Dist,
    Chi2.HEXACO.Dist,
    Chi3comb.HEXACO.Dist,
    Dan.HEXACO.Dist),
    sample=rep(c("British","Chinese 1",
                 "Chinese 2","Chinese 3","Danish"),each=6),
    country=rep(c("Britain","China","China",
                  "China","Denmark"),each=6))


#order by domain
HEXACO.Dist<-HEXACO.Dist[order(HEXACO.Dist$domain),]

#export
export(HEXACO.Dist,
       "../../../data/processed/Meta-analysis/HEXACO.Dist.xlsx")



## ----------------------------------------------------------------------------

(rma.Dist.AG<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        subset = domain=="AG",
        data=HEXACO.Dist,
        method="ML"))


## ----------------------------------------------------------------------------
(rma.Dist.AG.by.country<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        mods=~country-1,
        subset = domain=="AG",
        data=HEXACO.Dist,
        method="ML"))

anova(rma.Dist.AG.by.country,
      rma.Dist.AG)


summary(glht(rma.Dist.AG.by.country,
             linfct=cbind(contrMat(rep(1,3),type="Tukey"))),
        test=adjusted("none"))

summary(glht(rma.Dist.AG.by.country,
             linfct=cbind(contrMat(rep(1,3),type="Tukey"))),
        test=adjusted("holm"))


## ----------------------------------------------------------------------------

(rma.Dist.CO<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        subset = domain=="CO",
        data=HEXACO.Dist,
        method="ML"))


## ----------------------------------------------------------------------------
(rma.Dist.CO.by.country<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        mods=~country-1,
        subset = domain=="CO",
        data=HEXACO.Dist,
        method="ML"))

anova(rma.Dist.CO.by.country,
      rma.Dist.CO)


summary(glht(rma.Dist.CO.by.country,
             linfct=cbind(contrMat(rep(1,3),type="Tukey"))),
        test=adjusted("none"))

summary(glht(rma.Dist.CO.by.country,
             linfct=cbind(contrMat(rep(1,3),type="Tukey"))),
        test=adjusted("holm"))


## ----------------------------------------------------------------------------

(rma.Dist.EM<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        subset = domain=="EM",
        data=HEXACO.Dist,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.EX<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        subset = domain=="EX",
        data=HEXACO.Dist,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.HH<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        subset = domain=="HH",
        data=HEXACO.Dist,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.OE<-rma.uni(yi = SRc.zc.trend,
        sei=SE,
        subset = domain=="OE",
        data=HEXACO.Dist,
        method="ML"))


## ----------------------------------------------------------------------------
rma.HEXACO.Dist.all<-
  rma.uni(yi = SRc.zc.trend,
        sei=SE,
        #slab = labs,
        mods=~domain-1,
        data=HEXACO.Dist,
        method="ML")

rma.HEXACO.Dist.all

summary(glht(rma.HEXACO.Dist.all,
             linfct=cbind(contrMat(rep(1,6),type="Tukey"))),
        test=adjusted("none"))

summary(glht(rma.HEXACO.Dist.all,
             linfct=cbind(contrMat(rep(1,6),type="Tukey"))),
        test=adjusted("holm"))



## ----------------------------------------------------------------------------
Brit.HEXACO.Dist.by.satis<-
  import("../../../output/British/Brit.HEXACO.Dist.by.satis.xlsx")
Chi1.HEXACO.Dist.by.satis<-
  import("../../../output/Chinese 1/Chi1.HEXACO.Dist.by.satis.xlsx")
Chi2.HEXACO.Dist.by.satis<-
  import("../../../output/Chinese 2/Chi2.HEXACO.Dist.by.satis.xlsx")
Chi3comb.HEXACO.Dist.by.satis<-
  import("../../../output/Chinese 3/Chi3comb.HEXACO.Dist.by.satis.xlsx")

#combine
HEXACO.Dist.by.satis<-
  data.frame(rbind(
    Brit.HEXACO.Dist.by.satis,
    Chi1.HEXACO.Dist.by.satis,
    Chi2.HEXACO.Dist.by.satis,
    Chi3comb.HEXACO.Dist.by.satis),
    sample=rep(c("British","Chinese 1",
                 "Chinese 2","Chinese 3"),each=6),
    country=rep(c("Britain","China","China","China"),each=6))

#order by domain
HEXACO.Dist.by.satis<-
  HEXACO.Dist.by.satis[order(HEXACO.Dist.by.satis$domain),]

#export
export(HEXACO.Dist.by.satis,
       "../../../data/processed/Meta-analysis/HEXACO.Dist.by.satis.xlsx")


## ----------------------------------------------------------------------------

(rma.Dist.AG.satis<-rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="AG",
        data=HEXACO.Dist.by.satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.AG.satis.by.country<-
  rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="AG",
        mods=~country-1,
        data=HEXACO.Dist.by.satis,
        method="ML"))

anova(rma.Dist.AG.satis.by.country,
      rma.Dist.AG.satis)


## ----------------------------------------------------------------------------

(rma.Dist.CO.satis<-rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="CO",
        data=HEXACO.Dist.by.satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.EM.satis<-rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="EM",
        data=HEXACO.Dist.by.satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.EX.satis<-rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="EX",
        data=HEXACO.Dist.by.satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.EX.satis.by.country<-
  rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="EX",
        mods=~country-1,
        data=HEXACO.Dist.by.satis,
        method="ML"))

anova(rma.Dist.EX.satis.by.country,
      rma.Dist.EX.satis)


## ----------------------------------------------------------------------------

(rma.Dist.HH.satis<-rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="HH",
        data=HEXACO.Dist.by.satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.OE.satis<-rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="OE",
        data=HEXACO.Dist.by.satis,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.OE.satis.by.country<-
  rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="OE",
        mods=~country-1,
        data=HEXACO.Dist.by.satis,
        method="ML"))

anova(rma.Dist.OE.satis.by.country,
      rma.Dist.OE.satis)


## ----------------------------------------------------------------------------
(rma.HEXACO.Dist.satis.all<-
  rma.uni(yi = satis.z_SRc.zc.trend,
        sei=SE,
        #slab = labs,
        mods=~domain-1,
        data=HEXACO.Dist.by.satis,
        method="ML"))


summary(glht(rma.HEXACO.Dist.satis.all,
             linfct=cbind(contrMat(rep(1,6),type="Tukey"))),
        test=adjusted("none"))

summary(glht(rma.HEXACO.Dist.satis.all,
             linfct=cbind(contrMat(rep(1,6),type="Tukey"))),
        test=adjusted("holm"))


## ----------------------------------------------------------------------------
Brit.HEXACO.Dist.by.commit<-
  import("../../../output/British/Brit.HEXACO.Dist.by.commit.xlsx")
Chi1.HEXACO.Dist.by.commit<-
  import("../../../output/Chinese 1/Chi1.HEXACO.Dist.by.commit.xlsx")
Chi2.HEXACO.Dist.by.commit<-
  import("../../../output/Chinese 2/Chi2.HEXACO.Dist.by.commit.xlsx")
Chi3comb.HEXACO.Dist.by.commit<-
  import("../../../output/Chinese 3/Chi3comb.HEXACO.Dist.by.commit.xlsx")

#combine
HEXACO.Dist.by.commit<-
  data.frame(rbind(
    Brit.HEXACO.Dist.by.commit,
    Chi1.HEXACO.Dist.by.commit,
    Chi2.HEXACO.Dist.by.commit,
    Chi3comb.HEXACO.Dist.by.commit),
    sample=rep(c("British","Chinese 1",
                 "Chinese 2","Chinese 3"),each=6),
    country=rep(c("Britain","China","China","China"),each=6))

#order by domain
HEXACO.Dist.by.commit<-
  HEXACO.Dist.by.commit[order(HEXACO.Dist.by.commit$domain),]


#export
export(HEXACO.Dist.by.commit,
       "../../../data/processed/Meta-analysis/HEXACO.Dist.by.commit.xlsx")


## ----------------------------------------------------------------------------

(rma.Dist.AG.commit<-rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="AG",
        data=HEXACO.Dist.by.commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.AG.commit.by.country<-
  rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="AG",
        mods=~country-1,
        data=HEXACO.Dist.by.commit,
        method="ML"))

anova(rma.Dist.AG.commit.by.country,
      rma.Dist.AG.commit)


## ----------------------------------------------------------------------------

(rma.Dist.CO.commit<-rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="CO",
        data=HEXACO.Dist.by.commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.EM.commit<-rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="EM",
        data=HEXACO.Dist.by.commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.EX.commit<-rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="EX",
        data=HEXACO.Dist.by.commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.HH.commit<-rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="HH",
        data=HEXACO.Dist.by.commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.HH.commit.by.country<-
  rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="HH",
        mods=~country-1,
        data=HEXACO.Dist.by.commit,
        method="ML"))

anova(rma.Dist.HH.commit.by.country,
      rma.Dist.HH.commit)


## ----------------------------------------------------------------------------

(rma.Dist.OE.commit<-rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="OE",
        data=HEXACO.Dist.by.commit,
        method="ML"))


## ----------------------------------------------------------------------------

(rma.Dist.OE.commit.by.country<-
  rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="OE",
        mods=~country-1,
        data=HEXACO.Dist.by.commit,
        method="ML"))

anova(rma.Dist.OE.commit.by.country,
      rma.Dist.OE.commit)


## ----------------------------------------------------------------------------

(rma.Dist.OE.commit.by.country<-
  rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        subset = domain=="OE",
        mods=~country-1,
        data=HEXACO.Dist.by.commit,
        method="ML"))

anova(rma.Dist.OE.commit.by.country,
      rma.Dist.OE.commit)


## ----------------------------------------------------------------------------
(rma.HEXACO.Dist.commit.all<-
  rma.uni(yi = commit.z_SRc.zc.trend,
        sei=SE,
        #slab = labs,
        mods=~domain-1,
        data=HEXACO.Dist.by.commit,
        method="ML"))


summary(glht(rma.HEXACO.Dist.commit.all,
             linfct=cbind(contrMat(rep(1,6),type="Tukey"))),
        test=adjusted("none"))

summary(glht(rma.HEXACO.Dist.commit.all,
             linfct=cbind(contrMat(rep(1,6),type="Tukey"))),
        test=adjusted("holm"))


## ----------------------------------------------------------------------------
Brit.Dist.item.effs<-
  import("../../../output/British/Brit.Dist.item.effs.xlsx")
Chi1.Dist.item.effs<-
  import("../../../output/Chinese 1/Chi1.Dist.item.effs.xlsx")
Chi2.Dist.item.effs<-
  import("../../../output/Chinese 2/Chi2.Dist.item.effs.xlsx")
Chi3comb.Dist.item.effs<-
  import("../../../output/Chinese 3/Chi3comb.Dist.item.effs.xlsx")
Dan.Dist.item.effs<-
  import("../../../output/Danish/Dan.Dist.item.effs.xlsx")

#combine
Dist.item.effs<-
  data.frame(rbind(
    Brit.Dist.item.effs,
    Chi1.Dist.item.effs,
    Chi2.Dist.item.effs,
    Chi3comb.Dist.item.effs,
    Dan.Dist.item.effs),
    sample=rep(c("British","Chinese 1","Chinese 2",
                 "Chinese 3","Danish"),each=60),
    country=rep(c("Britain","China","China",
                  "China","Denmark"),each=60))
#names(Dist.item.effs)

#order by domain
#Dist.item.effs<-Dist.item.effs[order(Dist.item.effs$domain),]


## ----------------------------------------------------------------------------

pooled.item.effs<-data.frame(matrix(ncol=9,nrow=60))


for (i in 1:nrow(pooled.item.effs)) {
  rma.mod<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             subset = item==Dist.item.effs[i,"item"],
             data=Dist.item.effs,
             method="ML")
  
  pooled.item.effs[i,1]<-Dist.item.effs[i,"item"]
  pooled.item.effs[i,2]<-Dist.item.effs[i,"domain"]
  pooled.item.effs[i,3:9]<-
    c(est=rma.mod$beta,
    se=rma.mod$se,
    p=rma.mod$pval,
    LL=rma.mod$ci.lb,
    UL=rma.mod$ci.ub,
    Q=rma.mod$QE,
    Qp=rma.mod$QEp)
}

colnames(pooled.item.effs)<-
  c("item","domain","est","se","p","LL","UL","Q","Qp")
str(pooled.item.effs)

export(pooled.item.effs,
       "../../../data/processed/Meta-analysis/pooled.item.effs.xlsx")


## ----------------------------------------------------------------------------
labs<-
  paste0(pooled.item.effs$domain,"_",
         pooled.item.effs$item)

rma.pooled.item.effs.HH<-
  rma.uni(yi = est,
        sei=se,
        slab = labs,
        #mods=~domain,
        subset = domain=="HH",
        data=pooled.item.effs,
        method="ML")

rma.pooled.item.effs.HH


forest(rma.pooled.item.effs.HH,
       ilab = paste0("Qp = ",
                     round_tidy(
                       pooled.item.effs[
                         pooled.item.effs$domain=="HH",
                         "Qp"],3)),
       ilab.xpos = -0.5,ilab.pos = 4,
       addfit = T,
       header="Item-specific distinctive
       assumed similarity: HH",
       alim = c(-1,1.10),
       at = c(seq(from=-.20,to=0.7,by=0.1)))


## ----------------------------------------------------------------------------
HH.item.contr<-test.item.mod(rma.pooled.item.effs.HH)[[1]]
cbind(item=HH.item.contr[,1],
      round(HH.item.contr[,c(2,5,8)],3))


## ----------------------------------------------------------------------------
labs<-
  paste0(pooled.item.effs$domain,"_",
         pooled.item.effs$item)

rma.pooled.item.effs.EM<-
  rma.uni(yi = est,
        sei=se,
        slab = labs,
        #mods=~domain,
        subset = domain=="EM",
        data=pooled.item.effs,
        method="ML")

rma.pooled.item.effs.EM


forest(rma.pooled.item.effs.EM,
       ilab = paste0("Qp = ",
                     round_tidy(
                       pooled.item.effs[
                         pooled.item.effs$domain=="EM",
                         "Qp"],3)),
       ilab.xpos = -0.5,ilab.pos = 4,
       addfit = T,
       header="Item-specific distinctive
       assumed similarity: EM",
       alim = c(-1,1.10),
       at = c(seq(from=-.20,to=0.7,by=0.1)))


## ----------------------------------------------------------------------------
EM.item.contr<-test.item.mod(rma.pooled.item.effs.EM)[[1]]
cbind(item=EM.item.contr[,1],
      round(EM.item.contr[,c(2,5,8)],3))


## ----------------------------------------------------------------------------
labs<-
  paste0(pooled.item.effs$domain,"_",
         pooled.item.effs$item)

rma.pooled.item.effs.EX<-
  rma.uni(yi = est,
        sei=se,
        slab = labs,
        #mods=~domain,
        subset = domain=="EX",
        data=pooled.item.effs,
        method="ML")

rma.pooled.item.effs.EX




forest(rma.pooled.item.effs.EX,
       ilab = paste0("Qp = ",
                     round_tidy(
                       pooled.item.effs[
                         pooled.item.effs$domain=="EX",
                         "Qp"],3)),
       ilab.xpos = -0.5,ilab.pos = 4,
       addfit = T,
       header="Item-specific distinctive
       assumed similarity: EX",
       alim = c(-1,1.10),
       at = c(seq(from=-.20,to=0.7,by=0.1)))


## ----------------------------------------------------------------------------
EX.item.contr<-test.item.mod(rma.pooled.item.effs.EX)[[1]]
cbind(item=EX.item.contr[,1],
      round(EX.item.contr[,c(2,5,8)],3))


## ----------------------------------------------------------------------------
labs<-
  paste0(pooled.item.effs$domain,"_",
         pooled.item.effs$item)

rma.pooled.item.effs.AG<-
  rma.uni(yi = est,
        sei=se,
        slab = labs,
        #mods=~domain,
        subset = domain=="AG",
        data=pooled.item.effs,
        method="ML")

rma.pooled.item.effs.AG




forest(rma.pooled.item.effs.AG,
       ilab = paste0("Qp = ",
                     round_tidy(
                       pooled.item.effs[
                         pooled.item.effs$domain=="AG",
                         "Qp"],3)),
       ilab.xpos = -0.5,ilab.pos = 4,
       addfit = T,
       header="Item-specific distinctive
       assumed similarity: AG",
       alim = c(-1,1.10),
       at = c(seq(from=-.20,to=0.7,by=0.1)))


## ----------------------------------------------------------------------------
AG.item.contr<-test.item.mod(rma.pooled.item.effs.AG)[[1]]
cbind(item=AG.item.contr[,1],
      round(AG.item.contr[,c(2,5,8)],3))


## ----------------------------------------------------------------------------
labs<-
  paste0(pooled.item.effs$domain,"_",
         pooled.item.effs$item)

rma.pooled.item.effs.CO<-
  rma.uni(yi = est,
        sei=se,
        slab = labs,
        #mods=~domain,
        subset = domain=="CO",
        data=pooled.item.effs,
        method="ML")

rma.pooled.item.effs.CO





forest(rma.pooled.item.effs.CO,
       ilab = paste0("Qp = ",
                     round_tidy(
                       pooled.item.effs[
                         pooled.item.effs$domain=="CO",
                         "Qp"],3)),
       ilab.xpos = -0.5,ilab.pos = 4,
       addfit = T,
       header="Item-specific distinctive
       assumed similarity: CO",
       alim = c(-1,1.10),
       at = c(seq(from=-.20,to=0.7,by=0.1)))


## ----------------------------------------------------------------------------
CO.item.contr<-test.item.mod(rma.pooled.item.effs.CO)[[1]]
cbind(item=CO.item.contr[,1],
      round(CO.item.contr[,c(2,5,8)],3))


## ----------------------------------------------------------------------------
labs<-
  paste0(pooled.item.effs$domain,"_",
         pooled.item.effs$item)

rma.pooled.item.effs.OE<-
  rma.uni(yi = est,
        sei=se,
        slab = labs,
        #mods=~domain,
        subset = domain=="OE",
        data=pooled.item.effs,
        method="ML")

rma.pooled.item.effs.OE



forest(rma.pooled.item.effs.OE,
       ilab = paste0("Qp = ",
                     round_tidy(
                       pooled.item.effs[
                         pooled.item.effs$domain=="OE",
                         "Qp"],3)),
       ilab.xpos = -0.5,ilab.pos = 4,
       addfit = T,
       header="Item-specific distinctive
       assumed similarity: OE",
       alim = c(-1,1.10),
       at = c(seq(from=-.20,to=0.7,by=0.1)))


## ----------------------------------------------------------------------------
OE.item.contr<-test.item.mod(rma.pooled.item.effs.OE)[[1]]
cbind(item=OE.item.contr[,1],
      round(OE.item.contr[,c(2,5,8)],3))


## ----------------------------------------------------------------------------
(rma.mod1<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             method="ML"))

(rma.mod2<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             mods=~sample,
             method="ML"))

rma.mod2$R2

(rma.mod3<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             mods=~sample+domain,
             method="ML"))
rma.mod3$R2

rma.mod3$R2-rma.mod2$R2


## ----------------------------------------------------------------------------

(Brit.rma.mod1<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="British",
             method="ML"))


(Brit.rma.mod2<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="British",
             mods=~domain,
             method="ML"))

Brit.rma.mod2$R2




## ----------------------------------------------------------------------------

(Chi1.rma.mod1<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Chinese 1",
             method="ML"))


(Chi1.rma.mod2<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Chinese 1",
             mods=~domain,
             method="ML"))

Chi1.rma.mod2$R2




## ----------------------------------------------------------------------------

(Chi2.rma.mod1<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Chinese 2",
             method="ML"))


(Chi2.rma.mod2<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Chinese 2",
             mods=~domain,
             method="ML"))

Chi2.rma.mod2$R2




## ----------------------------------------------------------------------------

(Chi3comb.rma.mod1<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Chinese 3",
             method="ML"))


(Chi3comb.rma.mod2<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Chinese 3",
             mods=~domain,
             method="ML"))

Chi3comb.rma.mod2$R2




## ----------------------------------------------------------------------------

(Den.rma.mod1<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Danish",
             method="ML"))


(Den.rma.mod2<-rma.uni(yi = SRc.zc.trend,
             sei=SE,
             data=Dist.item.effs,
             subset=sample=="Danish",
             mods=~domain,
             method="ML"))

Den.rma.mod2$R2




## ---- code = readLines("../../custom functions/custom_functions.R")----------


## ----------------------------------------------------------------------------
sI <- sessionInfo()
print(sI, locale = FALSE)

