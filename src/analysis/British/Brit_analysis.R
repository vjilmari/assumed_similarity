## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE--------------------------------------------
library(lme4)
library(rio)
library(lmerTest)
library(emmeans)
library(dplyr)
library(ggplot2)
library(metafor)
library(finalfit)
library(psych)
#custom functions
source("../../custom functions/custom_functions.R")


## ----------------------------------------------------------------------------
Brit_long_fdat<-
  import("../../../data/processed/British/Brit_long_fdat.xlsx")



## ----------------------------------------------------------------------------

mod.dat<-Brit_long_fdat %>%
  group_by(ID) %>%
  summarize(satis.z=mean(satis.z),
            commit.z=mean(commit.z),
            cor.SRc.zc=transf.rtoz(cor(PR.z,SRc.zc)),
            cor.Norm_sr.z=transf.rtoz(cor(PR.z,Norm_sr.z)),
            cor.SR=transf.rtoz(cor(PR,SR)))

# mean distinctive similarity
(mean.dist.sim<-transf.ztor(mean(mod.dat$cor.SRc.zc)))
# mean normative similarity
(mean.norm.sim<-transf.ztor(mean(mod.dat$cor.Norm_sr.z)))
# mean overall profile similarity
(mean.over.sim<-transf.ztor(mean(mod.dat$cor.SR)))

#correlations with moderators
print(corr.test(mod.dat[,c("cor.SRc.zc",
                     "cor.Norm_sr.z",
                     "cor.SR",
                     "satis.z",
                     "commit.z")],adjust="none"),short=F)

#partial correlations with moderators

#distinctive (normative partialed)

partial.r(data=mod.dat,
          x=c("cor.SRc.zc",
                     "satis.z",
                     "commit.z"),
          y=c("cor.Norm_sr.z"))

#normative (distinctive partialed)

partial.r(data=mod.dat,
          x=c("cor.Norm_sr.z",
                     "satis.z",
                     "commit.z"),
          y=c("cor.SRc.zc"))



## ----------------------------------------------------------------------------
mod0<-lmer(PR.z~SRc.zc+Norm_sr.z+(1|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))
getMOD(mod0)


## ----------------------------------------------------------------------------
mod1<-lmer(PR.z~SRc.zc+Norm_sr.z+(0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

getMOD(mod1)
#check the CIs for random effects as well
confint(mod1, oldNames=FALSE)

#obtain random slope SDs for scaling purposes
(Dist.SD.mod1<-
    getREVAR(model=mod1,par="SRc.zc",
             grp="ID",type="sdcor"))

(Norm.SD.mod1<-
    getREVAR(model=mod1,par="Norm_sr.z",
             grp="ID",type="sdcor"))


#save estimates
export(summary(mod1)$coefficients,
       "../../../output/British/Brit.main.xlsx",row.names=T)

#save slope-SD estimates
export(cbind(Dist.SD.mod1,Norm.SD.mod1),
       "../../../output/British/Brit.slope.SD.xlsx",row.names=T)



## ----------------------------------------------------------------------------

mod2<-lmer(PR.z~SRc.zc+Norm_sr.z+
             satis.z+satis.z:SRc.zc+satis.z:Norm_sr.z+
             (0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

getMOD(mod2)

#save estimates
export(summary(mod2)$coefficients,
       "../../../output/British/Brit.satis.xlsx",row.names=T)



## ----------------------------------------------------------------------------
(slopes.Dist.satis.mod2<-emtrends(mod2,
         var="SRc.zc",
         specs="satis.z",
         at=list(satis.z=c(-1,0,1)),
         lmerTest.limit = 17760,
         disable.pbkrtest=T,infer=c(T,T)))

export(slopes.Dist.satis.mod2,
       "../../../output/British/Brit.satis.slopes.xlsx",row.names=T)


## ----------------------------------------------------------------------------
#obtain effect by scaling the contrast between mean and mean+1SD
eff.Dist.satis.mod2<-emtrends(mod2,
         var="SRc.zc",
         specs="satis.z",
         at=list(satis.z=c(1,0,-1)),
         lmerTest.limit = 17760,
         disable.pbkrtest=T,infer=c(T,T))
pairs(eff.Dist.satis.mod2,
      scale=1/Dist.SD.mod1,adjust="none",infer=c(T,T))



## ----------------------------------------------------------------------------

mod3<-lmer(PR.z~SRc.zc+Norm_sr.z+
             commit.z+commit.z:SRc.zc+commit.z:Norm_sr.z+
             (0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

getMOD(mod3)

#save estimates
export(summary(mod3)$coefficients,
       "../../../output/British/Brit.commit.xlsx",row.names=T)


## ----------------------------------------------------------------------------
(slopes.Dist.commit.mod3<-emtrends(mod3,
         var="SRc.zc",
         specs="commit.z",
         at=list(commit.z=c(-1,0,1)),
         lmerTest.limit = 17760,
         disable.pbkrtest=T,infer=c(T,T)))

export(slopes.Dist.commit.mod3,
       "../../../output/British/Brit.commit.slopes.xlsx",row.names=T)


## ----------------------------------------------------------------------------

#obtain effect by scaling the contrast between mean and mean+1SD
eff.Dist.commit.mod3<-emtrends(mod3,
         var="SRc.zc",
         specs="commit.z",
         at=list(commit.z=c(1,0,-1)),
         lmerTest.limit = 17760,infer=c(T,T),
         disable.pbkrtest=T)
pairs(eff.Dist.commit.mod3,scale=1/Dist.SD.mod1,
      adjust="none",infer=c(T,T))



## ----------------------------------------------------------------------------
mod9<-lmer(PR.z~SRc.zc+Norm_sr.z+
             (0+SRc.zc+Norm_sr.z|ID)+
             domain+domain:SRc.zc+domain:Norm_sr.z,
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

getMOD(mod9)
anova(mod9)
anova(mod1,mod9)


## ----------------------------------------------------------------------------

Dist.HEXACO.mod9<-emtrends(mod9,var="SRc.zc",specs="domain",
                          lmerTest.limit = 21540,
                          disable.pbkrtest=T,infer=c(T,T))
Dist.HEXACO.mod9

#export the domain-specific estimates
export(data.frame(test(Dist.HEXACO.mod9)),
       "../../../output/British/Brit.HEXACO.Dist.xlsx")
contrast(Dist.HEXACO.mod9,"eff",adjust="holm")
contrast(Dist.HEXACO.mod9,"del.eff",adjust="holm")
pairs(Dist.HEXACO.mod9,adjust="holm")
pairs(Dist.HEXACO.mod9,adjust="none")


## ----------------------------------------------------------------------------

Norm.HEXACO.mod9<-emtrends(mod9,var="Norm_sr.z",
                           specs="domain",
                          lmerTest.limit = 21540,
                          disable.pbkrtest=T,infer=c(T,T))
Norm.HEXACO.mod9
test(Norm.HEXACO.mod9)
export(data.frame(test(Norm.HEXACO.mod9)),
       "../../../output/British/Brit.HEXACO.Norm.xlsx")
contrast(Norm.HEXACO.mod9,"eff",adjust="holm")
contrast(Norm.HEXACO.mod9,"del.eff",adjust="holm")
pairs(Norm.HEXACO.mod9,adjust="holm")
pairs(Norm.HEXACO.mod9,adjust="none")


## ----------------------------------------------------------------------------
mod10<-lmer(PR.z~SRc.zc+Norm_sr.z+
             domain+domain:SRc.zc+domain:Norm_sr.z+
             satis.z+satis.z:SRc.zc+satis.z:Norm_sr.z+
             domain:satis.z+
             domain:satis.z:SRc.zc+
             domain:satis.z:Norm_sr.z+
              (0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

getMOD(mod10)
anova(mod10)

#model with manually defined interactions


Brit_long_fdat$satis.z_SRc.zc<-
  Brit_long_fdat$satis.z*Brit_long_fdat$SRc.zc

Brit_long_fdat$satis.z_Norm_sr.z<-
  Brit_long_fdat$satis.z*Brit_long_fdat$Norm_sr.z

mod10.alt<-lmer(PR.z~SRc.zc+Norm_sr.z+
             domain+domain:SRc.zc+domain:Norm_sr.z+
             satis.z+satis.z_SRc.zc+satis.z_Norm_sr.z+
             domain:satis.z+
             domain:satis.z_SRc.zc+
             domain:satis.z_Norm_sr.z+
               (0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

#confirm that the models are identical
anova(mod10,mod10.alt)




## ----------------------------------------------------------------------------

#satisfaction effect on distinctive similarity separately for domains

#unscaled

(Dist.satis.domain.mod10.alt<-
  emtrends(mod10.alt,
           var="satis.z_SRc.zc",
           #by="domain",
           specs=c("domain"),
           lmerTest.limit = 17760,
           disable.pbkrtest=T,infer=c(T,T)))

export(Dist.satis.domain.mod10.alt,
       "../../../output/British/Brit.HEXACO.Dist.by.satis.xlsx")

pairs(Dist.satis.domain.mod10.alt,adjust="holm")
contrast(Dist.satis.domain.mod10.alt,"eff",adjust="holm")
contrast(Dist.satis.domain.mod10.alt,"del.eff",adjust="holm")

#scaled with common SD

Dist.var.common.scaled<-paste0("satis.z_SRc.zc*c(",
                        Dist.SD.mod1,")")

(Dist.satis.domain.mod10.alt.scaled<-
  emtrends(mod10.alt,
           var=Dist.var.common.scaled,
           #by="domain",
           specs=c("domain"),
           lmerTest.limit = 17760,
           disable.pbkrtest=T,infer=c(T,T)))


export(Dist.satis.domain.mod10.alt.scaled,
       "../../../output/British/Brit.HEXACO.Dist.by.satis.scaled.xlsx")


#simple slopes

(Dist.satis.slopes.domain.mod10<-
  emtrends(mod10,
           var="SRc.zc",
           by="domain",
           specs=c("satis.z"),
           at=list(satis.z=c(-1,0,1)),      
           lmerTest.limit = 17760,
           disable.pbkrtest=T,infer=c(T,T)))

export(data.frame(Dist.satis.slopes.domain.mod10),
       "../../../output/British/Brit.HEXACO.Dist.by.satis.slopes.xlsx")



## ----------------------------------------------------------------------------
mod11<-lmer(PR.z~SRc.zc+Norm_sr.z+
             domain+domain:SRc.zc+domain:Norm_sr.z+
             commit.z+commit.z:SRc.zc+commit.z:Norm_sr.z+
             domain:commit.z+
             domain:commit.z:SRc.zc+
             domain:commit.z:Norm_sr.z+
              (0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

getMOD(mod11)
anova(mod11)

#model with manually defined interactions


Brit_long_fdat$commit.z_SRc.zc<-
  Brit_long_fdat$commit.z*Brit_long_fdat$SRc.zc

Brit_long_fdat$commit.z_Norm_sr.z<-
  Brit_long_fdat$commit.z*Brit_long_fdat$Norm_sr.z

mod11.alt<-lmer(PR.z~SRc.zc+Norm_sr.z+
             domain+domain:SRc.zc+domain:Norm_sr.z+
             commit.z+commit.z_SRc.zc+commit.z_Norm_sr.z+
             domain:commit.z+
             domain:commit.z_SRc.zc+
             domain:commit.z_Norm_sr.z+
               (0+SRc.zc+Norm_sr.z|ID),
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))

#confirm that the models are identical
anova(mod11,mod11.alt)




## ----------------------------------------------------------------------------

#commitment effect on distinctive similarity separately for domains

#unscaled

(Dist.commit.domain.mod11.alt<-
  emtrends(mod11.alt,
           var="commit.z_SRc.zc",
           #by="domain",
           specs=c("domain"),
           lmerTest.limit = 17760,
           disable.pbkrtest=T,infer=c(T,T)))

export(Dist.commit.domain.mod11.alt,
       "../../../output/British/Brit.HEXACO.Dist.by.commit.xlsx")

pairs(Dist.commit.domain.mod11.alt,adjust="holm")
contrast(Dist.commit.domain.mod11.alt,"eff",adjust="holm")
contrast(Dist.commit.domain.mod11.alt,"del.eff",adjust="holm")

#scaled with common SD

Dist.var.common.scaled<-paste0("commit.z_SRc.zc*c(",
                        Dist.SD.mod1,")")

(Dist.commit.domain.mod11.alt.scaled<-
  emtrends(mod11.alt,
           var=Dist.var.common.scaled,
           #by="domain",
           specs=c("domain"),
           lmerTest.limit = 17760,
           disable.pbkrtest=T,infer=c(T,T)))


export(Dist.commit.domain.mod11.alt.scaled,
       "../../../output/British/Brit.HEXACO.Dist.by.commit.scaled.xlsx")


#simple slopes

(Dist.commit.slopes.domain.mod11<-
  emtrends(mod11,
           var="SRc.zc",
           by="domain",
           specs=c("commit.z"),
           at=list(commit.z=c(-1,0,1)),      
           lmerTest.limit = 17760,
           disable.pbkrtest=T,infer=c(T,T)))

export(data.frame(Dist.commit.slopes.domain.mod11),
       "../../../output/British/Brit.HEXACO.Dist.by.commit.slopes.xlsx")



## ----------------------------------------------------------------------------
mod12<-lmer(PR.z~SRc.zc+Norm_sr.z+
             (0+SRc.zc+Norm_sr.z|ID)+
             item:SRc.zc,
           data=Brit_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))
Brit.Dist.item.effs<-
  emtrends(mod12,var="SRc.zc",specs="item",
         lmerTest.limit = 21540,
         disable.pbkrtest=T)

#contrasts for items within domain
#OE

contrast(Brit.Dist.item.effs,
         "del.eff",include=seq(1,60,6),adjust="holm")

#CO
contrast(Brit.Dist.item.effs,
         "del.eff",include=seq(2,60,6),adjust="holm")
#AG
contrast(Brit.Dist.item.effs,
         "del.eff",include=seq(3,60,6),adjust="holm")
#EX
contrast(Brit.Dist.item.effs,
         "del.eff",include=seq(4,60,6),adjust="holm")
#EM
contrast(Brit.Dist.item.effs,
         "del.eff",include=seq(5,60,6),adjust="holm")
#HH
contrast(Brit.Dist.item.effs,
         "del.eff",include=seq(6,60,6),adjust="holm")

#combine to same frame
Brit.Dist.item.effs<-cbind(data.frame(Brit.Dist.item.effs),
      t.ratio=data.frame(test(Brit.Dist.item.effs))$t.ratio,
      p.value=data.frame(test(Brit.Dist.item.effs))$p.value,
      adj.p.value=
        data.frame(test(Brit.Dist.item.effs,adjust="holm"))$p.value)

Brit.Dist.item.effs$domain<-
  rep(c("OE","CO","AG","EX","EM","HH"),10)

Brit.Dist.item.effs$item.number<-rep(1:10,each=6)

Brit.Dist.item.effs

#save
export(Brit.Dist.item.effs,
       "../../../output/British/Brit.Dist.item.effs.xlsx")


## ----------------------------------------------------------------------------

#save the domain means to different frames
Brit.Dist.dom.means<-Brit.Dist.item.effs %>%
  group_by(domain) %>%
  summarize(SRc.zc.trend=mean(SRc.zc.trend))

Brit.Dist.dom.sds<-Brit.Dist.item.effs %>%
  group_by(domain) %>%
  summarize(SRc.zc.trend.sd=sd(SRc.zc.trend))

Brit.Dist.dom.means$LL<-
  Brit.Dist.dom.means$SRc.zc.trend-
  Brit.Dist.dom.sds$SRc.zc.trend.sd

Brit.Dist.dom.means$UL<-
  Brit.Dist.dom.means$SRc.zc.trend+
  Brit.Dist.dom.sds$SRc.zc.trend.sd

Brit.Dist.dom.means


#Distinctive similarity
Fig1<-ggplot(data=Brit.Dist.item.effs,
             aes(x=as.factor(item.number),y=SRc.zc.trend,group=domain))+
  geom_point(size=4,aes(shape=domain))+
  geom_line()+
  ylab("Distinctive similarity")+
  xlab("Item")+
  geom_hline(aes(yintercept=SRc.zc.trend),
             Brit.Dist.dom.means,linetype=1,size=1.5)+
  geom_hline(aes(yintercept=LL),
             Brit.Dist.dom.means,linetype=3,size=1)+
  geom_hline(aes(yintercept=UL),
             Brit.Dist.dom.means,linetype=3,size=1)+
  theme(axis.text.y=element_text(size=12,face="bold",family="sans"),
        axis.title.y=element_text(size=14,face="bold",family="sans"),
        axis.text.x=element_text(size=8,family="sans"),
        axis.title.x=element_text(size=14,face="bold",family="sans"),
        strip.background =element_rect(fill="white"),
        panel.grid.major = element_line(linetype=3,color="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=12,family="sans"),
        panel.background = element_rect(fill="white"),
        legend.key = element_rect(colour = NA, fill = NA))+
  facet_wrap(~domain,ncol=6)+ 
  theme(strip.text.x = element_text(size = 12))

Fig1



## ---- code = readLines("../../custom functions/custom_functions.R")----------


## ----------------------------------------------------------------------------
sI <- sessionInfo()
print(sI, locale = FALSE)

