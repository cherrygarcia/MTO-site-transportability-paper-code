##########################################
### Step 1. Estimate site-specific ITTATEs
##########################################
library(survey)
library(mice)
library(mitools)

set.seed(23498)

#make the imputed datasets into a list
someintoutimpls <- list(someintoutimp1, someintoutimp2, someintoutimp3, someintoutimp4, someintoutimp5, someintoutimp6, someintoutimp7, someintoutimp8, someintoutimp9, someintoutimp10,  someintoutimp11, someintoutimp12, someintoutimp13, someintoutimp14, someintoutimp15, someintoutimp16, someintoutimp17, someintoutimp18, someintoutimp19, someintoutimp20, someintoutimp21, someintoutimp22, someintoutimp23, someintoutimp24, someintoutimp25, someintoutimp26, someintoutimp27, someintoutimp28, someintoutimp29, someintoutimp30)

r_drug_pot_evrdat<-someintoutimpls

resdf<-as.data.frame(matrix(rep(1, 5*15), ncol=5))
colnames(resdf)<-c("est", "var","site", "strata", "outcome")
res<-list(resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf, resdf) 

#estimate survey-weighted ITTATEs for our example outcome of any marijuana use by site and sex and for each imputed dataset
for(i in 1:30){
  res[[i]][,"site"]<-rep(1:5, 15)
  res[[i]][,"strata"]<-rep(c(rep("male", 5), rep("female", 5), rep("all", 5)), 15)

  des<-svydesign(id=~1,weights=~wt_totsvy, data=r_drug_pot_evrdat[[i]])
  desm<-svydesign(id=~1,weights=~wt_totsvy, data=r_drug_pot_evrdat[[i]][r_drug_pot_evrdat[[i]]$svy_gender==0,])
  desf<-svydesign(id=~1,weights=~wt_totsvy, data=r_drug_pot_evrdat[[i]][r_drug_pot_evrdat[[i]]$svy_gender==1,])
  
  #for males only
  res[[i]][1,1]<-summary(svyglm(r_drug_pot_evr~ voucher, design=desm, subset=ra_site==1))$coefficients[2]
  res[[i]][1,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desm, subset=ra_site==1))$coefficients[2,2]^2
  res[[i]][2,1]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desm, subset=ra_site==2))$coefficients[2]
  res[[i]][2,2]<-summary(svyglm(r_drug_pot_evr~  voucher  , design=desm, subset=ra_site==2))$coefficients[2,2]^2
  res[[i]][3,1]<-summary(svyglm(r_drug_pot_evr~  voucher  , design=desm, subset=ra_site==3))$coefficients[2]
  res[[i]][3,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desm, subset=ra_site==3))$coefficients[2,2]^2
  res[[i]][4,1]<-summary(svyglm(r_drug_pot_evr~ voucher  , design=desm, subset=ra_site==4))$coefficients[2]
  res[[i]][4,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desm, subset=ra_site==4))$coefficients[2,2]^2
  res[[i]][5,1]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desm, subset=ra_site==5))$coefficients[2]
  res[[i]][5,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desm, subset=ra_site==5))$coefficients[2,2]^2
  
  #for females only
  res[[i]][6,1]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desf, subset=ra_site==1))$coefficients[2]
  res[[i]][6,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desf, subset=ra_site==1))$coefficients[2,2]^2
  res[[i]][7,1]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desf, subset=ra_site==2))$coefficients[2]
  res[[i]][7,2]<-summary(svyglm(r_drug_pot_evr~ voucher , design=desf, subset=ra_site==2))$coefficients[2,2]^2
  res[[i]][8,1]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desf, subset=ra_site==3))$coefficients[2]
  res[[i]][8,2]<-summary(svyglm(r_drug_pot_evr~ voucher , design=desf, subset=ra_site==3))$coefficients[2,2]^2
  res[[i]][9,1]<-summary(svyglm(r_drug_pot_evr~  voucher  , design=desf, subset=ra_site==4))$coefficients[2]
  res[[i]][9,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desf, subset=ra_site==4))$coefficients[2,2]^2
  res[[i]][10,1]<-summary(svyglm(r_drug_pot_evr~  voucher , design=desf, subset=ra_site==5))$coefficients[2]
  res[[i]][10,2]<-summary(svyglm(r_drug_pot_evr~  voucher, design=desf, subset=ra_site==5))$coefficients[2,2]^2
  
  #male and females combined
  res[[i]][11,1]<-summary(svyglm(r_drug_pot_evr~  voucher, design=des, subset=ra_site==1))$coefficients[2]
  res[[i]][11,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=des, subset=ra_site==1))$coefficients[2,2]^2
  res[[i]][12,1]<-summary(svyglm(r_drug_pot_evr~  voucher, design=des, subset=ra_site==2))$coefficients[2]
  res[[i]][12,2]<-summary(svyglm(r_drug_pot_evr~ voucher, design=des, subset=ra_site==2))$coefficients[2,2]^2
  res[[i]][13,1]<-summary(svyglm(r_drug_pot_evr~  voucher, design=des, subset=ra_site==3))$coefficients[2]
  res[[i]][13,2]<-summary(svyglm(r_drug_pot_evr~ voucher , design=des, subset=ra_site==3))$coefficients[2,2]^2
  res[[i]][14,1]<-summary(svyglm(r_drug_pot_evr~  voucher, design=des, subset=ra_site==4))$coefficients[2]
  res[[i]][14,2]<-summary(svyglm(r_drug_pot_evr~  voucher  , design=des, subset=ra_site==4))$coefficients[2,2]^2
  res[[i]][15,1]<-summary(svyglm(r_drug_pot_evr~  voucher, design=des, subset=ra_site==5))$coefficients[2]
  res[[i]][15,2]<-summary(svyglm(r_drug_pot_evr~  voucher , design=des, subset=ra_site==5))$coefficients[2,2]^2
}

#combine results across imputed datasets
resmice<-summary(MIcombine(lapply(res, "[[", 1),lapply(res, "[[", 2)))

################################################################
### Step 2. Test whether the site-specific ITTATEs are different
################################################################
out1<-c()
for(i in 1:30){
  desm<-svydesign(id=~1,weights=~wt_totsvy, data=r_drug_pot_evrdat[[i]][r_drug_pot_evrdat[[i]]$ra_site %in% c(2,4) & r_drug_pot_evrdat[[i]]$svy_gender==0,])
  full<-svyglm(r_drug_pot_evr~ voucher*I(ra_site==2) , design=desm)
  red<-svyglm(r_drug_pot_evr~ voucher + I(ra_site==2), design=desm)
  out1[i]<-anova(red,full)$p
}
mean(out1)

#############################################################################
### Step 3. Test whether evidence against a common outcome model across sites
#############################################################################
# For this, we need to use the functions provided in the following reference:
# Luedtke AR, Carone M, van der Laan, MJ. An Omnibus Nonparametric Test of Equality in Distribution for Unknown Functions. 2015. arXiv preprint arXiv:1510.04195. 

set.seed(23498)

testest<-c()

#estimate p value for each imputed dataset
for(i in 1:30){
  print(i)
  testest[i]<-est.psi.prob.binom.h2o(W=r_drug_pot_evrdat[[i]][,c(5,13:18,79,81:86,90:102,164:166)], A="scontrast", Y="r_drug_pot_evr", data=r_drug_pot_evrdat[[i]])[2]
}
mean(testest)

#######################################
### Step 4. Estimate transported ITTATE
#######################################

#######################################################################
### Step 4a. Estimate predicted probabilities using an ensemble learner
#######################################################################

### Define h2o ensemble function
h2o.gbmint2<-function(..., ntrees=100, interaction.depth=2, seed=1)h2o.gbm.wrapper(...,ntrees=ntrees, interaction.depth=interaction.depth, seed=seed)
h2o.lasso<-function(..., alpha=1)h2o.glm.wrapper(...,alpha=alpha)
h2o.reg<-function(..., alpha=.5)h2o.glm.wrapper(...,alpha=alpha)
h2o.ridge<-function(..., alpha=0)h2o.glm.wrapper(...,alpha=alpha)
learner<-c("h2o.glm.wrapper", "h2o.gbm.wrapper", "h2o.gbmint2", "h2o.lasso",  "h2o.reg", "h2o.ridge")
metalearner<-"h2o.glm.wrapper"

localh2o=h2o.init(nthreads= -1)
h2o.removeAll()

#this function generates predicted probabilities for S, Z, Y
SLpredh2o<-function(a, z, y, site, w, radid_person, data){
  tmpdat<-data[,c(radid_person, a, z, y, site)]
  x<-names(w)
  dat<-data.frame(cbind(tmpdat, w))
  
  dat.h2o<-as.h2o(dat)
  dat.h2o[,site] <-as.factor(dat.h2o[,site])
  dat.h2o[,z] <-as.factor(dat.h2o[,z])
  dat.h2o[,y] <-as.factor(dat.h2o[,y])
  names(dat.h2o)<-c("radid_person", "a", "z","y","site", x)
  fits <- h2o.ensemble(x=x, y ="site",  training_frame=dat.h2o, family="binomial", learner=learner, metalearner=metalearner, cvControl=list(V=5))
  probshat<-as.data.frame(predict(fits, dat.h2o)$pred)[,3]
  
  #predict probabilities for Z by site
  dat.h2o.s0<-dat.h2o[dat.h2o$site=="0",]
  dat.h2o.s1<-dat.h2o[dat.h2o$site=="1",]
  x<-c(names(w), "a")
  SLfitzs0<-h2o.ensemble(y="z", x=x, training_frame=dat.h2o.s0, family="binomial", learner=learner, metalearner=metalearner, cvControl=list(V=5))
  SLfitzs1<-h2o.ensemble(y="z", x=x, training_frame=dat.h2o.s1, family="binomial", learner=learner, metalearner=metalearner, cvControl=list(V=5))
  
  dat.h2o.a0<-dat.h2o.a1<-dat.h2o
  dat.h2o.a0$a<-0
  dat.h2o.a1$a<-1
  
  probzhats0a0<-as.data.frame(predict(SLfitzs0,  dat.h2o.a0)$pred)[,3]
  probzhats0a1<-as.data.frame(predict(SLfitzs0,  dat.h2o.a1)$pred)[,3]
  
  probzhats1a0<-as.data.frame(predict(SLfitzs1, dat.h2o.a0)$pred)[,3]
  probzhats1a1<-as.data.frame(predict(SLfitzs1, dat.h2o.a1)$pred)[,3]
  
  #predict probabilitys for Y 
  x<-c(names(w), "a", "z")
  SLfity   <-h2o.ensemble(y="y", x=x, training_frame=dat.h2o.s1, family="binomial", learner=learner, metalearner=metalearner, cvControl=list(V=5))
 
  x<-c(names(w), "a")
  SLfitynoz<-h2o.ensemble(y="y", x=x, training_frame=dat.h2o.s1, family="binomial", learner=learner, metalearner=metalearner, cvControl=list(V=5))
  
  dat.h2o.z0<-dat.h2o.z1<-dat.h2o
  dat.h2o.z0$z<-0
  dat.h2o.z1$z<-1
  
  proby0hat<-as.data.frame(predict(SLfity, dat.h2o.z0)$pred)[,3] 
  proby1hat<-as.data.frame(predict(SLfity, dat.h2o.z1)$pred)[,3] 
  
  probyhat<-as.data.frame(predict(SLfity, dat.h2o)$pred)[,3] 
  
  proby0hatnoz<-as.data.frame(predict(SLfitynoz, dat.h2o.a0)$pred)[,3] 
  proby1hatnoz<-as.data.frame(predict(SLfitynoz, dat.h2o.a1)$pred)[,3] 
  
  return(cbind(probshat, probzhats0a0, probzhats0a1, probzhats1a0, probzhats1a1, probyhat, proby0hat, proby1hat, proby0hatnoz, proby1hatnoz, dat[,radid_person]))
  
}

### run function on each of the 30 imputed datasets
dat<-r_drug_pot_evrdat
for(i in 1:30){
  print(i)
  SLresdat<-SLpredh2o(a="voucher", z="lowpovmove", y="r_drug_pot_evr", site="scontrast", w=dat[[i]][,c(5, 12:18,79,81:86,90:102,164,167)], radid_person="radid_person", data=dat[[i]])
  write.csv(SLresdat, file=paste0("potvoucher","SLpred", i, ".csv"))
}

#########################################################################################
### Step 4b. Use predicted probabilities in transport TMLE to estimate transported ITTATE
#########################################################################################

### Define TMLE transport function
ittatetmle<-function(a, z, y, site, w, svywt, shat, zhats0a0, zhats0a1, zhats1a0, zhats1a1, yhat, aq2model){
  
  datw<-w
  n.dat<-nrow(datw)
  
  #make sure yhat is within bounds
  tmp1<-ifelse(yhat<.001, .001,yhat)
  tmp2<-ifelse(yhat==1, .999, tmp1)
  yhat<-tmp2
  
  #calculate components of clever covariate
  cpa <- mean(a[site==1])
  cps <- shat
  
  data_new0<-data_new1<-datw
  data_new0$a<-0
  data_new1$a<-1
  
  dga1s0<-dbinom(z, 1,prob=zhats0a1)
  dga1s1<-dbinom(z, 1,prob=zhats1a1)
  dga0s0<-dbinom(z, 1,prob=zhats0a0)
  dga0s1<-dbinom(z, 1,prob=zhats1a0)
  
  #calculate clever covariate
  g0w<-(1-cpa)*(dga0s1/dga0s0)*(cps/(1-cps))*(1/svywt)
  g1w<-cpa*(dga1s1/dga1s0)*(cps/(1-cps))*(1/svywt)
  h0w<-((1-a)*I(site==1))/g0w
  h1w<-(a*I(site==1))/g1w
  
  #initial prediciton
  qtmp<-cbind(yhat, yhat, yhat)
  q<-apply(qtmp, c(1,2), qlogis)
  
  epsilon<-coef(glm(y ~ -1 + offset(q[,1]) + h0w + h1w , family="binomial", subset=site==1 ))
  
  #update initial prediction
  q1<- q + c((epsilon[1]*h0w + epsilon[2]*h1w), epsilon[1]/g0w, epsilon[2]/g1w)
  
  predmodela0<-suppressWarnings(glm(formula=paste("plogis(q1)", aq2model, sep="~"), data=data.frame(cbind(w, a=a,site=site, q1=q1[,2])), subset=site==0 & a==0 , family="binomial"))
  predmodela1<-suppressWarnings(glm(formula=paste("plogis(q1)", aq2model, sep="~"), data=data.frame(cbind(w,a=a,site=site,  q1=q1[,3])), subset=site==0 & a==1 , family="binomial"))
  predmodelaa<-suppressWarnings(glm(formula=paste("plogis(q1) ~", aq2model, "+a", sep=""), data=data.frame(cbind(w, site=site, q1=q1[,1], a=a)), subset=site==0, family="binomial"))
  
  #get initial prediction for second regression model 
  q2pred<-cbind(predict(predmodelaa, type="link", newdata=data.frame(cbind(datw, a=a))), predict(predmodela0, type="link", newdata=datw), predict(predmodela1, type="link", newdata=datw))
  
  cz<-cbind(ifelse(a==0,(I(site==0)*svywt)/(1-cpa), (I(site==0)*svywt)/cpa), (I(site==0)*svywt)/(1-cpa),  (I(site==0)*svywt)/cpa)
 
  num<- (plogis(q2pred[,3][site==0]) - plogis(q2pred[,2][site==0]))*svywt[site==0]
  tmleest<-sum(num)/sum(svywt[site==0])
  
  ps0<-mean(I(site==0))
  
  #calculate efficient influence curve
  eic<-(((h1w/ps0) - (h0w/ps0))*(y - plogis(q[,1]))) +  (((a*cz[,3]/ps0) - ((1-a)*cz[,2]/ps0))* (plogis(q[,1]) - plogis(q2pred[,1]))) + ((I(site==0)/ps0)*((plogis(q2pred[,3]) - plogis(q2pred[,2])) - tmleest))
  
  return(list("est"=tmleest, "var"=var(eic)/n.dat, "eic"=eic))
  
}

### run function on each of the 30 imputed datasets

#read in predicted probabilities
tmpdat <- lapply(Sys.glob("potvoucherSLpred*.csv"), read.csv)

tmpdat2<-tmpdat
for(i in 1:30){
  names(tmpdat[[i]])<-c("trash", "shat",  "zhats0a0", "zhats0a1", "zhats1a0", "zhats1a1", "yhat", "y0hat", "y1hat", "y0hatnoz", "y1hatnoz", "radid_person")
  tmpdat2[[i]] <- join_all(list(r_drug_pot_evrdat[[i]],tmpdat[[i]][,-1] ), by = 'radid_person', type = 'inner', match = 'first')
  tmpdat2[[i]]$zhats1a<-ifelse(tmpdat2[[i]]$voucher==1, tmpdat2[[i]]$zhats1a1, tmpdat2[[i]]$zhats1a0)
  tmpdat2[[i]]$zhats0a<-ifelse(tmpdat2[[i]]$voucher==1, tmpdat2[[i]]$zhats0a1, tmpdat2[[i]]$zhats0a0)
}

guessittate<-list(c(1,2))

#estimate the transported ITTATE
for(i in 1:30){
  guessittate[[i]]<-ittatetmle(a=tmpdat2[[i]]$voucher, z=tmpdat2[[i]]$lowpovmove, y=tmpdat2[[i]]$r_drug_pot_evr, site=tmpdat2[[i]]$scontrast, w=tmpdat2[[i]][,c(6, 12:18,79,81:86,90:102,164,167)], svywt=tmpdat2[[i]]$wt_totsvy, shat=tmpdat2[[i]]$shat, zhats0a0=tmpdat2[[i]]$zhats0a0, zhats0a1=tmpdat2[[i]]$zhats0a1, zhats1a0=tmpdat2[[i]]$zhats1a0, zhats1a1=tmpdat2[[i]]$zhats1a1, yhat=tmpdat2[[i]]$yhat,  aq2model="svy_age_imp+e_repeat_evr +x_c1_behprb+ x_c1_expel+x_c1_gifted+ x_c1_lrnprb+x_c1_schcll+ x_ch_schplay+x_ad_edgradhs+x_ad_nevmarr+ x_ad_parentu18 +x_ad_working+x_hh_afdc+ x_hh_car+ x_hh_disabl+  x_hh_victim + x_hood_5y+  x_hood_chat+ x_hood_nbrkid+  x_hood_nofamily + x_hood_nofriend+ x_hood_safenit +x_hood_verydissat+ x_hous_fndapt+x_hous_mov3tm  + x_hous_movdrgs+x_hous_movschl+ x_hous_sec8bef+ethrace +hhsize")[1:2]
  
}
write.csv(guessittate, "guessittatevoucherpot.csv")

#combine imputations for 1 estimate
pott<-summary(MIcombine(lapply(guessittate, "[[", 1),lapply(guessittate, "[[", 2)))[c(1,3,4)]

# Can now graph the site-specific ITTATEs and the transported ITTATE and calculate the percent of the difference between the ITTATEs explained by composition