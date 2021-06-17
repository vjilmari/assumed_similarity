---
title: "Statistical power"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Packages


```r
library(rio)
library(lme4)
library(lmerTest)
library(psych)
source("../custom functions/custom_functions.R")
```

# Simulation for the entire profile

## Sample size


```r
#leave Danish sample out because not included in the moderation analysis
n<-1104
```

## Test the function in a single run


```r
prof_mod_power(n=n,
               n.item=60,
               CL.cor=.10,
               mean.Dist=.30,
               sd.Dist=.15,
               wi.rand = F,
               sd.Dist.wi=.00001)
```

```
##               Est           p    scaled scaled.res  slope.SD lvl.2.cor
## sr:Rel 0.01735061 0.003970276 0.1139251  0.1068785 0.1522984 0.1043324
##           emp.cor  emp.cor.z        R2         R     re.cor
## sr:Rel 0.09501905 0.09364693 0.0108927 0.1043681 0.08654937
```

## Run simulation


```r
#number of repeated processes
REPS=1000
#seed for reproducibility
set.seed(93543)

res.list.10<-list()
res.list.20<-list()
res.list.30<-list()

start.time <- Sys.time()

for (i in 1:REPS){
  res.list.10[[i]]<-
    prof_mod_power(n=n,
               n.item=60,
               CL.cor=.10,
               mean.Dist=.30,
               sd.Dist=.15,
               wi.rand = F,
               sd.Dist.wi=.00001)
  
  res.list.20[[i]]<-
    prof_mod_power(n=n,
               n.item=60,
               CL.cor=.20,
               mean.Dist=.30,
               sd.Dist=.15,
               wi.rand = F,
               sd.Dist.wi=.00001)
  
  res.list.30[[i]]<-
    prof_mod_power(n=n,
               n.item=60,
               CL.cor=.30,
               mean.Dist=.30,
               sd.Dist=.15,
               wi.rand = F,
               sd.Dist.wi=.00001)
}
```

```
## Warning in sqrt(R2): NaNs produced
```

```
## Warning in sqrt(1 - weight^2): NaNs produced
```

```
## Warning in rnorm(n.items, sd = sqrt(1 - weight^2)): NAs produced
```

```r
end.time <- Sys.time()
time.taken <- end.time - start.time
#how long it took
time.taken
```

```
## Time difference of 2.328236 hours
```

```r
res.dat.10<-do.call(rbind.data.frame,res.list.10)
res.dat.20<-do.call(rbind.data.frame,res.list.20)
res.dat.30<-do.call(rbind.data.frame,res.list.30)
export(res.dat.10,"power_simuation_results.10.xlsx")
export(res.dat.20,"power_simuation_results.20.xlsx")
export(res.dat.30,"power_simuation_results.30.xlsx")
```

## Analyze the results

### Power


```r
#with population effect r =.10
table(res.dat.10$Est>0 & res.dat.10$p<.05)/nrow(res.dat.10)
```

```
## 
## FALSE  TRUE 
## 0.276 0.724
```

```r
#with population effect r =.20
table(res.dat.20$Est>0 & res.dat.20$p<.05)/nrow(res.dat.20)
```

```
## 
## FALSE  TRUE 
## 0.001 0.999
```

```r
#with population effect r =.30
table(res.dat.30$Est>0 & res.dat.30$p<.05)/nrow(res.dat.30)
```

```
## 
## TRUE 
##    1
```

### Effect sizes


```r
#with population effect size r = .10
describe(res.dat.10,fast=T)
```

```
##            vars    n mean   sd   min  max range se
## Est           1 1000 0.01 0.01  0.00 0.03  0.03  0
## p             2 1000 0.07 0.15  0.00 0.98  0.98  0
## scaled        3 1000 0.10 0.04 -0.02 0.21  0.22  0
## scaled.res    4 1000 0.09 0.04 -0.02 0.20  0.21  0
## slope.SD      5 1000 0.15 0.01  0.13 0.17  0.04  0
## lvl.2.cor     6 1000 0.10 0.03 -0.01 0.19  0.20  0
## emp.cor       7 1000 0.08 0.03 -0.01 0.16  0.17  0
## emp.cor.z     8 1000 0.08 0.03 -0.01 0.16  0.17  0
## R2            9 1000 0.01 0.01  0.00 0.04  0.04  0
## R            10  999 0.10 0.04  0.00 0.21  0.21  0
## re.cor       11 1000 0.08 0.03 -0.01 0.16  0.17  0
```

```r
#with population effect size r = .20
describe(res.dat.20,fast=T)
```

```
##            vars    n mean   sd  min  max range se
## Est           1 1000 0.03 0.01 0.01 0.05  0.04  0
## p             2 1000 0.00 0.01 0.00 0.17  0.17  0
## scaled        3 1000 0.20 0.04 0.06 0.33  0.28  0
## scaled.res    4 1000 0.19 0.03 0.05 0.32  0.26  0
## slope.SD      5 1000 0.15 0.01 0.13 0.17  0.04  0
## lvl.2.cor     6 1000 0.20 0.03 0.10 0.28  0.18  0
## emp.cor       7  999 0.16 0.03 0.03 0.25  0.22  0
## emp.cor.z     8  999 0.16 0.03 0.03 0.26  0.23  0
## R2            9 1000 0.04 0.01 0.00 0.11  0.11  0
## R            10 1000 0.20 0.04 0.05 0.33  0.28  0
## re.cor       11 1000 0.15 0.03 0.04 0.26  0.22  0
```

```r
#with population effect size r = .30
describe(res.dat.30,fast=T)
```

```
##            vars    n mean   sd  min  max range se
## Est           1 1000 0.05 0.01 0.03 0.07  0.04  0
## p             2 1000 0.00 0.00 0.00 0.00  0.00  0
## scaled        3 1000 0.30 0.04 0.16 0.42  0.26  0
## scaled.res    4 1000 0.28 0.03 0.16 0.40  0.25  0
## slope.SD      5 1000 0.15 0.01 0.13 0.17  0.03  0
## lvl.2.cor     6 1000 0.30 0.03 0.20 0.39  0.19  0
## emp.cor       7 1000 0.24 0.03 0.14 0.32  0.19  0
## emp.cor.z     8 1000 0.24 0.03 0.14 0.33  0.19  0
## R2            9 1000 0.09 0.02 0.03 0.17  0.15  0
## R            10 1000 0.30 0.04 0.16 0.42  0.25  0
## re.cor       11 1000 0.23 0.03 0.13 0.33  0.20  0
```


# Custom functions


```r
getFE<-function(model){
  require(finalfit) #install this package first (it helps having the correct sign for rounded numbers that are near zero)
  coefs<-data.frame(summary(model)$coefficients) #obtain fixed effects
  names.temp<-rownames(coefs) #save the names of the fixed effects
  coefs$lower<-coefs[,1]-qt(p=.975,df=coefs[,"df"])*coefs[,2] #lower confidence 
  coefs$upper<-coefs[,1]+qt(p=.975,df=coefs[,"df"])*coefs[,2] #upper confidence
  coefs<-cbind.data.frame(Est=round_tidy(coefs[,1],2),
                          SE=round_tidy(coefs[,2],2),
                          df=round_tidy(coefs[,3],2),
                          t=round_tidy(coefs[,4],2),
                          p=round_tidy(coefs[,5],3),
                          LL=round_tidy(coefs$lower,2),
                          UL=round_tidy(coefs$upper,2)) #construct new output frame
  rownames(coefs)<-names.temp #add effect names
  
  return(coefs) #print the output
}


getVC<-function(model){
  require(finalfit)
  VC<-as.data.frame(VarCorr(model))
  VC<-cbind(VC[,c(1:3)],
            est_SD=round_tidy(VC[,5],2),
            est_VAR=round_tidy(VC[,4],2))
  return(VC)
}

getDEV<-function(model){
  DEV<-unname(summary(model)$devcomp$cmp["dev"])
  n.pars<-sum(getME(model,"p"),
              getME(model,"m"))
  output<-cbind(Deviance=DEV,n.pars=n.pars)
  return(output)
}

getMOD<-function(model){
  Fixed<-getFE(model)
  Random<-getVC(model)
  Deviance<-getDEV(model)
  output<-list(Fixed=Fixed,
               Random=Random,
               Deviance=Deviance)
  return(output)
}

getREVAR<-function(model,par,grp,type){
  d<-as.data.frame(VarCorr(model))
  d[d$var1==par & is.na(d$var2) & d$grp==grp,type][1]
  
}


getR2Dist_satisSlope<-function(model,cl.par){
  v.full<-as.numeric(r2mlm(model)$R2["v","within"])
  red.mod<-update(model,.~.-satis.z:SRc.zc)
  v.red<-as.numeric(r2mlm(red.mod)$R2["v","within"])
  R2.slope<-(v.red-v.full)/v.red
  output<-c(v.red=v.red,
            v.full=v.full,
            R2.slope=R2.slope,
            R.slope=sqrt(R2.slope))
  return(output)
}


getR2Norm_satisSlope<-function(model,cl.par){
  v.full<-as.numeric(r2mlm(model)$R2["v","within"])
  red.mod<-update(model,.~.-satis.z:Norm_sr.z)
  v.red<-as.numeric(r2mlm(red.mod)$R2["v","within"])
  R2.slope<-(v.red-v.full)/v.red
  output<-c(v.red=v.red,
            v.full=v.full,
            R2.slope=R2.slope,
            R.slope=sqrt(R2.slope))
  return(output)
}


getR2Dist_commitSlope<-function(model,cl.par){
  v.full<-as.numeric(r2mlm(model)$R2["v","within"])
  red.mod<-update(model,.~.-commit.z:SRc.zc)
  v.red<-as.numeric(r2mlm(red.mod)$R2["v","within"])
  R2.slope<-(v.red-v.full)/v.red
  output<-c(v.red=v.red,
            v.full=v.full,
            R2.slope=R2.slope,
            R.slope=sqrt(R2.slope))
  return(output)
}


getR2Norm_commitSlope<-function(model,cl.par){
  v.full<-as.numeric(r2mlm(model)$R2["v","within"])
  red.mod<-update(model,.~.-commit.z:Norm_sr.z)
  v.red<-as.numeric(r2mlm(red.mod)$R2["v","within"])
  R2.slope<-(v.red-v.full)/v.red
  output<-c(v.red=v.red,
            v.full=v.full,
            R2.slope=R2.slope,
            R.slope=sqrt(R2.slope))
  return(output)
}


#function to rescale from 1 to 9 to 1 to 5

rescale.commit<-function(var){
  var[var==2]<-1.5
  var[var==3]<-2
  var[var==4]<-2.5
  var[var==5]<-3
  var[var==6]<-3.5
  var[var==7]<-4
  var[var==8]<-4.5
  var[var==9]<-5
  return(var)
  
}

#get rid of commas as decimals

commas.to.dots<-function(var){
  as.numeric(sub(",",".",var))
}

#obtain item deviations from domain means from the pooled estimates

test.item.mod<-function(model){
  items<-attr(model$yi,which = "slab")
  estimates<-model$yi[1:length(items)]
  vi<-model$vi
  
  mdat<-data.frame(items=items,
                   estimates=estimates,
                   vi=vi)
  
  mod.effs<-data.frame(matrix(ncol=7,nrow=length(items)))
  del.effs<-data.frame(matrix(ncol=7,nrow=length(items)))
  
  for (i in 1:length(items)){
    
    mod.effs[i,1]<-items[i]
    del.effs[i,1]<-items[i]
    
    mdat$mod<-ifelse(mdat$items==items[i],1,0)
    
    mod.mod<-rma.uni(yi=estimates,
                     vi=vi,
                     mods=~mod,
                     data=mdat,
                     method="ML")
    
    mod.effs[i,2:7]<-
      c(summary(mod.mod)$beta[2],
        summary(mod.mod)$se[2],
        summary(mod.mod)$zval[2],
        summary(mod.mod)$pval[2],
        summary(mod.mod)$ci.lb[2],
        summary(mod.mod)$ci.ub[2])
    
    del.effs[i,2:7]<-
      c(summary(mod.mod)$beta[1],
        summary(mod.mod)$se[1],
        summary(mod.mod)$zval[1],
        summary(mod.mod)$pval[1],
        summary(mod.mod)$ci.lb[1],
        summary(mod.mod)$ci.ub[1])
  }
  
  colnames(mod.effs)<-
    c("item","mod.est","se","z","p","LL","UL")
  
  colnames(del.effs)<-
    c("item","del.est","se","z","p","LL","UL")
  
  mod.effs$p.adj<-p.adjust(mod.effs$p,method = "holm")
  
  output<-list(mod.effs,del.effs)
  return(output)
}

#simulation function that generates item responses based on weighting
#and individual dispersion

## for single items

gen.std<-function(weight){
  sr<-rnorm(1)
  pr<-weight*sr+rnorm(1,sd=sqrt(1-weight^2))
  output<-c(sr,pr)
  return(output)
}

## for entire profile

gen.std.prof<-function(weight,n.items){
  sr<-rnorm(n.items)
  pr<-weight*sr+rnorm(n.items,sd=sqrt(1-weight^2))
  output<-cbind(sr,pr)
  return(output)
}



#fixed effects without rounding (for simulations)

getFEraw<-function(model){
  #require(finalfit) #install this package first (it helps having the correct sign for rounded numbers that are near zero)
  coefs<-data.frame(summary(model)$coefficients) #obtain fixed effects
  names.temp<-rownames(coefs) #save the names of the fixed effects
  coefs$lower<-coefs[,1]-qt(p=.975,df=coefs[,"df"])*coefs[,2] #lower confidence 
  coefs$upper<-coefs[,1]+qt(p=.975,df=coefs[,"df"])*coefs[,2] #upper confidence
  coefs<-cbind.data.frame(Est=coefs[,1],
                          SE=coefs[,2],
                          df=coefs[,3],
                          t=coefs[,4],
                          p=coefs[,5],
                          LL=coefs$lower,
                          UL=coefs$upper) #construct new output frame
  rownames(coefs)<-names.temp #add effect names
  
  return(coefs) #print the output
}

#power simulation function

prof_mod_power<-function(n,
                         n.item,
                         CL.cor,
                         mean.Dist,
                         sd.Dist,
                         wi.rand=F,
                         sd.Dist.wi){
  require(dplyr)
  require(metafor)
  require(lme4)
  require(lmerTest)
  #generate moderator that with standard normal distribution
  Rel<-rnorm(n,mean=0,sd=1)
  #generate similarity correlations for each participant
  #that is dependent on the moderator
  Dist<-CL.cor*Rel+rnorm(n,mean=mean.Dist/sd.Dist,sd=sqrt(1-CL.cor^2))
  #scale similarity correlations to given parameters
  Dist<-Dist*sd.Dist
  
  #generate individual level data
  
  #data list to save the individually generated data frames
  dat.list<-list()
  
  if (wi.rand) {
    for (i in 1:n){
      #generate item-specific distribution of similarity
      #based on person's mean similarity, and some dispersion
      item.Dist<-rnorm(n.item,mean=Dist[i],sd=sd.Dist.wi)
      #save to a frame alongside ID-variable and the moderator
      dat.list[[i]]<-
        cbind.data.frame(ID=paste0("ID",i),Rel=Rel[i],
                         t(sapply(item.Dist,gen.std)))
    }
  } else {
    for (i in 1:n){
      #generate general distribution of similarity
      #for each subject
      
      #save to a frame alongside ID-variable and the moderator
      dat.list[[i]]<-
        cbind.data.frame(ID=paste0("ID",i),Rel=Rel[i],
                         gen.std.prof(weight=Dist[i],
                                      n.item = n.item))
    }
  }
  
  
  
  #combine all participants to same long format data file
  dat<-do.call(rbind.data.frame,dat.list)
  #name the columns in the data
  colnames(dat)<-c("ID","Rel","sr","pr")
  
  #fit model without cross-level interaction
  mod1<-lmerTest::lmer(pr~sr+Rel+(0+sr|ID),data=dat,REML=F)
  #obtain the slope SD for scaling
  slope.SD<-getREVAR(mod1,par="sr",grp="ID",type="sdcor")
  #obtain the slope var for effect size
  slope.var.mod1<-getREVAR(mod1,par="sr",grp="ID",type="vcov")
  
  #fit model with the cross-level interaction
  mod2<-lmerTest::lmer(pr~sr+Rel+sr:Rel+(0+sr|ID),data=dat,REML=F)
  #obtain estimate and p-value
  CL.eff<-getFEraw(mod2)[4,c("Est","p")]
  #obtain the slope var for effect size
  slope.var.mod2<-getREVAR(mod2,par="sr",grp="ID",type="vcov")
  #scale the fixed slope with slope SD from the reduced model
  CL.eff$scaled<-CL.eff$Est/slope.SD
  #obtain empirical correlations
  emp.cor.dat<-dat %>%
    group_by(ID) %>%
    summarise(emp.cor=cor(sr,pr),
              emp.cor.z=transf.rtoz(cor(sr,pr)),
              Rel=mean(Rel))
  emp.cors<-cor(emp.cor.dat[,2:4])[3,1:2]
  
  #reduction in slope variance
  R2=(slope.var.mod1-slope.var.mod2)/slope.var.mod1
  R=sqrt(R2)
  
  #correlation between moderator and posterior mode slopes from the reduced model
  re.mod1<-data.frame(ranef(mod1)$ID)
  re.mod1$ID<-rownames(re.mod1)
  re.mod1<-left_join(re.mod1,emp.cor.dat,by="ID")
  re.cor<-cor(re.mod1$sr,re.mod1$Rel)
  
  #scaling that accounts for residual
  res.SD.mod2<-as.data.frame(VarCorr(mod2))[2,5]
  CL.eff$scaled.res<-CL.eff$Est/(slope.SD/res.SD.mod2)
  
  #combine results
  results<-cbind(CL.eff,
                 slope.SD,
                 lvl.2.cor=cor(Rel,Dist),
                 emp.cor=emp.cors[1],
                 emp.cor.z=emp.cors[2],
                 R2,
                 R,
                 re.cor)
  #export
  return(results)
  
}
```

# Session information


```r
s<-sessionInfo()
print(s,locale=F)
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19041)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] metafor_2.4-0  dplyr_1.0.2    psych_2.0.9    lmerTest_3.1-2 lme4_1.1-23   
## [6] Matrix_1.2-18  rio_0.5.16    
## 
## loaded via a namespace (and not attached):
##  [1] statmod_1.4.34      tidyselect_1.1.0    xfun_0.19          
##  [4] purrr_0.3.4         splines_4.0.3       haven_2.3.1        
##  [7] lattice_0.20-41     colorspace_1.4-1    vctrs_0.3.7        
## [10] generics_0.0.2      htmltools_0.5.0     yaml_2.2.1         
## [13] utf8_1.2.1          rlang_0.4.10        pillar_1.5.1       
## [16] nloptr_1.2.2.2      foreign_0.8-80      glue_1.4.2         
## [19] readxl_1.3.1        lifecycle_1.0.0     stringr_1.4.0      
## [22] munsell_0.5.0       gtable_0.3.0        cellranger_1.1.0   
## [25] zip_2.1.1           evaluate_0.14       knitr_1.30         
## [28] forcats_0.5.0       parallel_4.0.3      curl_4.3           
## [31] fansi_0.4.1         Rcpp_1.0.5          scales_1.1.1       
## [34] tmvnsim_1.0-2       mnormt_2.0.2        ggplot2_3.3.2      
## [37] hms_0.5.3           digest_0.6.25       stringi_1.5.3      
## [40] openxlsx_4.2.2      numDeriv_2016.8-1.1 grid_4.0.3         
## [43] tools_4.0.3         magrittr_1.5        tibble_3.0.3       
## [46] crayon_1.4.1        pkgconfig_2.0.3     MASS_7.3-53        
## [49] ellipsis_0.3.1      data.table_1.14.0   minqa_1.2.4        
## [52] rmarkdown_2.5       R6_2.4.1            boot_1.3-25        
## [55] nlme_3.1-149        compiler_4.0.3
```
