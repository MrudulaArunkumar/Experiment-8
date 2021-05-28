#workspace aufraeumen####
rm(list=ls())

#notwendige Pakete laden####
library(plyr)
library(lme4)
library(tidyverse)
library(here)

  #+++++++++++++++++++++++++++++++++++####
  #MULTILEVEL ANALYSIS####
  #+++++++++++++++++++++++++++++++++++++++
  
   
raw.data<-read.csv(here("Data", "Exp8_partialdataset.csv"))
   

#create numerical var for response type

raw.data$previous_rm<-ifelse(raw.data$ResponseType=="RR", 1, 2)
table(raw.data$ResponseType, raw.data$previous_rm)


#limit analyses to testtrials only ####
table(raw.data$Condition)
raw.data<-subset(raw.data, subset = (raw.data$Condition=="test"))

#limit analyses to test trials whose last occurrence was a learning trial
raw.data<-subset(raw.data, subset = (raw.data$Distance_type==1))


#center predictors####
#factors validity and saliency contrast coded with -1/1 as levels
summary(raw.data$val)
summary(raw.data$sal)


#centering within person (recommended, since hypotheses focus on level 1 predictors####

summary(raw.data$previous_rm)
summary(raw.data$Distance)
summary(raw.data$Distance_type)
  
  #previous_rm
  means.previous_rm <- aggregate(data = raw.data, previous_rm ~ participant, mean)
  names(means.previous_rm)[2]<-"previous_rm_mean"
  
  raw.data<-merge (raw.data, means.previous_rm, by="participant")
  
  raw.data$previous_rm_cwp <- raw.data$previous_rm-raw.data$previous_rm_mean
  summary(raw.data$previous_rm_cwp)
  
  #check whether person mean centering was correctly done
  all(raw.data$previous_rm == raw.data$previous_rm_cwp+raw.data$previous_rm_mean)


  #Distance
  means.Distance<- aggregate(data = raw.data, Distance ~ participant, mean)
  names(means.Distance)[2]<-"Distance_mean"
  
  raw.data<-merge (raw.data, means.Distance, by="participant")
  
  raw.data$Distance_cwp <- raw.data$Distance-raw.data$Distance_mean
  summary(raw.data$Distance_cwp)
  
  #check whether person mean centering was correctly done
  all(raw.data$Distance == raw.data$Distance_cwp+raw.data$Distance_mean)
  

  #Distance type
  
  # means.Distance_type<- aggregate(data = raw.data, Distance_type ~ participant, mean)
  # names(means.Distance_type)[2]<-"Distance_type_mean"
  # 
  # raw.data<-merge (raw.data, means.Distance_type, by="participant")
  
  # raw.data$Distance_type_cwp <- raw.data$Distance_type-raw.data$Distance_type_mean
  # summary(raw.data$Distance_type_cwp)
  # 
  #check whether person mean centering was correctly done
  # all(raw.data$Distance_type == raw.data$Distance_type_cwp+raw.data$Distance_type_mean)
  
#new var for errors
  raw.data$err_trials <-1-raw.data$ACC_trials
  


#MLM Analyses ####
#Start with MLM Model (random slopes) that corresponds to ANOVA on aggregated data
  #FActors: Validity (1=valid, -1=invalid) and Saliency (1=salient D, -1=nonsalient D), only test trials enter into analyses
  #validity effect: RT are faster for valid compared to invalid trials
  #saliency effct: RT are faster for salient compared to nonsalient D
  #interaction:
  
 
  #RT
randomSlopes_m1<-lmer(RT_io~1+val*sal + (1+val*sal|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m1)

#Check Richtung der Interaktion (kann man sich "simple slopes" auch f?r MLM anzeigen lassen?) -  validity effect should occur only for salient D, not for nonsalient D -> works
# salientD<-subset(raw.data, subset = (raw.data$sal==1))
# randomSlopes_m11<-lmer(RT_io~1+val + (1+val|participant),
#                       data=salientD,
#                       REML=F,
#                       na.action = "na.omit")
# 
# summary(randomSlopes_m11)
# # 
# nonsalientD<-subset(raw.data, subset = (raw.data$sal==-1))
# randomSlopes_m12<-lmer(RT_io~1+val + (1+val|participant),
#                        data=nonsalientD,
#                        REML=F,
#                        na.action = "na.omit")
# 
# summary(randomSlopes_m12)
# 
#Error rates

randomSlopes_m1_err<-lmer(err_trials~1+val*sal + (1+val*sal|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m1_err)

#Check pattern behind IA
# randomSlopes_m11_err<-lmer(err_trials~1+val + (1+val|participant),
#                       data=salientD,
#                       REML=F,
#                       na.action = "na.omit")
# 
# summary(randomSlopes_m11_err)
# 
# 
# randomSlopes_m12_err<-lmer(err_trials~1+val + (1+val|participant),
#                        data=nonsalientD,
#                        REML=F,
#                        na.action = "na.omit")
# 
# summary(randomSlopes_m12_err)
# 
# 
# #Will Interaction survive once we control for binding & retrieval effects?
# #previous_response match, distance, as main effects
# randomSlopes_m2<-lmer(RT_io~1+val*sal + previous_rm_cwp+ Distance_cwp  +(1+val*sal + previous_rm_cwp + Distance_cwp + Distance_type_cwp|participant), 
#                       data=raw.data, 
#                       REML=F,
#                       na.action = "na.omit")
# 
# summary(randomSlopes_m2)
# 
# 
# randomSlopes_m2_err<-lmer(err_trials~1+val*sal + previous_rm_cwp+ Distance_cwp + Distance_type_cwp +(1+val*sal + previous_rm_cwp + Distance_cwp + Distance_type_cwp|participant), 
#                       data=raw.data, 
#                       REML=F,
#                       na.action = "na.omit")
# 
# summary(randomSlopes_m2_err)

#Model ohne Val and Sal, daf?r mit Interaktionen von Distance, previous_rm, und Distance type
randomSlopes_m3<-lmer(RT_io~1+ previous_rm_cwp*Distance_cwp +(1 + previous_rm_cwp*Distance_cwp|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m3)


randomSlopes_m3_err<-lmer(err_trials~1+ previous_rm_cwp*Distance_cwp +(1 + previous_rm_cwp*Distance_cwp|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m3_err)

#RT:Einfaches SR binding hat "besten" effekt, aber nsign (t=1.73)
#Err: previous_rm x distance ia sign

#Interaktion Val*Sal wieder mit aufnehmen
randomSlopes_m4<-lmer(RT_io~1+val*sal+previous_rm_cwp*Distance_cwp +(1+val*sal+previous_rm_cwp*Distance_cwp|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m4)
#Modell with 4way interaction does not converge

randomSlopes_m4_err<-lmer(err_trials~1+val*sal + previous_rm_cwp*Distance_cwp +(1+val*sal + previous_rm_cwp*Distance_cwp|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m4_err)
#RT: val *sal Interaction still significant
#Err: val *sal Interaction still significant, in addition, previous_rm*distance sign

randomIntercept_m0<-lmer(RT_io~1 + (1|participant), 
                         data=raw.data, 
                         REML=F,
                         na.action = "na.omit")
summary(randomIntercept_m0)

randomIntercept_m0_err<-lmer(err_trials~1 + (1|participant), 
                         data=raw.data, 
                         REML=F,
                         na.action = "na.omit")
summary(randomIntercept_m0_err)

#Model comparison
#Model 3 vs Model0

anova(randomIntercept_m0, randomSlopes_m1)
anova(randomIntercept_m0_err, randomSlopes_m1_err)
#model 1 has sign better fit than model 0 for RT and Err

anova(randomSlopes_m1, randomSlopes_m2)
anova(randomSlopes_m1_err, randomSlopes_m2_err)
#model 2 has no  better fit than model 0 for RT and Err

anova(randomSlopes_m1, randomSlopes_m4)

#++++++++++++++++++++++++++++++++++++++++
#further ideas, meeting march, 31st 2021

#only analyse salient test trials (to reduce number of predictors)
raw.data<-subset(raw.data, subset = (raw.data$Saliency=="Salient"))


#RT
randomSlopes_m5<-lmer(RT_io~1+val + (1+val|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m5)
#val effect sign

#ERR
randomSlopes_m5_err<-lmer(err_trials~1+val + (1+val|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m5_err)
#val effect sign

#model 6: include binding factors (previous_rm and distance, check whether val effect survives)
randomSlopes_m6<-lmer(RT_io~1+val*previous_rm_cwp*Distance_cwp + (1+val*previous_rm_cwp*Distance_cwp|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m6)
#val main effect no longer sign (t=1.273), but: distance and previous_rm ns, either (and no interaction)

#err

randomSlopes_m6_err<-lmer(err_trials~1+val*previous_rm_cwp*Distance_cwp + (1+val*previous_rm_cwp*Distance_cwp|participant), 
                      data=raw.data, 
                      REML=F,
                      na.action = "na.omit")

summary(randomSlopes_m6_err)
#err rates: val main effect still sign (t=-2.599), previous_rm, distance, previous_rm*distance: ns
