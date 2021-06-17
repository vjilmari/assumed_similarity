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
#custom functions
source("../../custom functions/custom_functions.R")



## ----------------------------------------------------------------------------

Dan_long_fdat<-
  import("../../../data/processed/Danish/Dan_long_fdat.xlsx")


## ----------------------------------------------------------------------------

mod.dat<-Dan_long_fdat %>%
  group_by(ID) %>%
  summarize(#satis.z=mean(satis.z),
            #commit.z=mean(commit.z),
            #responsive.z=mean(responsive.z),
            cor.SRc.zc=transf.rtoz(cor(PR.z,SRc.zc)),
            cor.Norm_sr.z=transf.rtoz(cor(PR.z,Norm_sr.z)),
            cor.SR=transf.rtoz(cor(PR,SR)))

# mean distinctive similarity
(mean.dist.sim<-transf.ztor(mean(mod.dat$cor.SRc.zc)))
# mean normative similarity
(mean.norm.sim<-transf.ztor(mean(mod.dat$cor.Norm_sr.z)))
# mean overall profile similarity
(mean.over.sim<-transf.ztor(mean(mod.dat$cor.SR)))




## ----------------------------------------------------------------------------
mod0<-lmer(PR.z~SRc.zc+Norm_sr.z+(1|ID),
           data=Dan_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))
getMOD(mod0)


## ----------------------------------------------------------------------------
mod1<-lmer(PR.z~SRc.zc+Norm_sr.z+(0+SRc.zc+Norm_sr.z|ID),
           data=Dan_long_fdat,
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
       "../../../output/Danish/Dan.main.xlsx",row.names=T)

#save slope-SD estimates
export(cbind(Dist.SD.mod1,Norm.SD.mod1),
       "../../../output/Danish/Dan.slope.SD.xlsx",row.names=T)



## ----------------------------------------------------------------------------
mod9<-lmer(PR.z~SRc.zc+Norm_sr.z+
             (0+SRc.zc+Norm_sr.z|ID)+
             domain+domain:SRc.zc+domain:Norm_sr.z,
           data=Dan_long_fdat,
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
       "../../../output/Danish/Dan.HEXACO.Dist.xlsx")
contrast(Dist.HEXACO.mod9,"eff",adjust="holm")
contrast(Dist.HEXACO.mod9,"del.eff",adjust="holm")
pairs(Dist.HEXACO.mod9,adjust="holm")
pairs(Dist.HEXACO.mod9,adjust="none")


## ----------------------------------------------------------------------------

Norm.HEXACO.mod9<-
  emtrends(mod9,
           var="Norm_sr.z",
           specs="domain",
           lmerTest.limit = 21540,
           disable.pbkrtest=T,infer=c(T,T))

Norm.HEXACO.mod9
test(Norm.HEXACO.mod9)
export(data.frame(test(Norm.HEXACO.mod9)),
       "../../../output/Danish/Dan.HEXACO.Norm.xlsx")
contrast(Norm.HEXACO.mod9,"eff",adjust="holm")
contrast(Norm.HEXACO.mod9,"del.eff",adjust="holm")
pairs(Norm.HEXACO.mod9,adjust="holm")
pairs(Norm.HEXACO.mod9,adjust="none")


## ----------------------------------------------------------------------------
mod12<-lmer(PR.z~SRc.zc+Norm_sr.z+
             (0+SRc.zc+Norm_sr.z|ID)+
             item:SRc.zc,
           data=Dan_long_fdat,
           REML=F,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e8)))
Dan.Dist.item.effs<-
  emtrends(mod12,var="SRc.zc",specs="item",
         lmerTest.limit = 21540,
         disable.pbkrtest=T)

#contrasts for items within domain
#OE

contrast(Dan.Dist.item.effs,
         "del.eff",include=seq(1,60,6),adjust="holm")

#CO
contrast(Dan.Dist.item.effs,
         "del.eff",include=seq(2,60,6),adjust="holm")
#AG
contrast(Dan.Dist.item.effs,
         "del.eff",include=seq(3,60,6),adjust="holm")
#EX
contrast(Dan.Dist.item.effs,
         "del.eff",include=seq(4,60,6),adjust="holm")
#EM
contrast(Dan.Dist.item.effs,
         "del.eff",include=seq(5,60,6),adjust="holm")
#HH
contrast(Dan.Dist.item.effs,
         "del.eff",include=seq(6,60,6),adjust="holm")

#combine to same frame
Dan.Dist.item.effs<-cbind(data.frame(Dan.Dist.item.effs),
      t.ratio=data.frame(test(Dan.Dist.item.effs))$t.ratio,
      p.value=data.frame(test(Dan.Dist.item.effs))$p.value,
      adj.p.value=data.frame(test(Dan.Dist.item.effs,adjust="holm"))$p.value)

Dan.Dist.item.effs$domain<-
  rep(c("OE","CO","AG","EX","EM","HH"),10)

Dan.Dist.item.effs$item.number<-rep(1:10,each=6)

Dan.Dist.item.effs

#save
export(Dan.Dist.item.effs,
       "../../../output/Danish/Dan.Dist.item.effs.xlsx")


## ----------------------------------------------------------------------------

#save the domain means to different frames
Dan.Dist.dom.means<-Dan.Dist.item.effs %>%
  group_by(domain) %>%
  summarize(SRc.zc.trend=mean(SRc.zc.trend))

Dan.Dist.dom.sds<-Dan.Dist.item.effs %>%
  group_by(domain) %>%
  summarize(SRc.zc.trend.sd=sd(SRc.zc.trend))

Dan.Dist.dom.means$LL<-
  Dan.Dist.dom.means$SRc.zc.trend-
  Dan.Dist.dom.sds$SRc.zc.trend.sd

Dan.Dist.dom.means$UL<-
  Dan.Dist.dom.means$SRc.zc.trend+
  Dan.Dist.dom.sds$SRc.zc.trend.sd

Dan.Dist.dom.means


#Distinctive similarity
Fig1<-ggplot(data=Dan.Dist.item.effs,
             aes(x=as.factor(item.number),y=SRc.zc.trend,group=domain))+
  geom_point(size=4,aes(shape=domain))+
  geom_line()+
  ylab("Distinctive similarity")+
  xlab("Item")+
  geom_hline(aes(yintercept=SRc.zc.trend),
             Dan.Dist.dom.means,linetype=1,size=1.5)+
  geom_hline(aes(yintercept=LL),
             Dan.Dist.dom.means,linetype=3,size=1)+
  geom_hline(aes(yintercept=UL),
             Dan.Dist.dom.means,linetype=3,size=1)+
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

