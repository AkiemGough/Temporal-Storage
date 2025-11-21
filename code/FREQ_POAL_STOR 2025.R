#--------------------#
#title: "Temporal Storage Effect?"
#author: "Akiem_Gough"
#date: "2025-02-07"
#--------------------#

###Setting working directory
setwd("/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/Data & Analysis/Data")
getwd()

###Importing data set
compcoe <- read.csv("/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/Data & Analysis/Data/ltreb_allspp_qaqc.csv",stringsAsFactors = T)

library(tidyverse)
library(scales)
library(lme4)
library(car)
library(AICcmodavg)
library(glmmTMB)


###Set up (each graphical representation relies on these lines of code)

##Making columns for color distinctions for endophyte status
compcoe$colorvar <- case_when(compcoe$endo_01 == 1 ~"slateblue4",compcoe$endo_01 == 0 ~"goldenrod4")
compcoe$coloUrvar <- case_when(compcoe$endo_01 == 1 ~"slateblue",compcoe$endo_01 == 0 ~"goldenrod")

##Filtering for Poa Alsodes
poaals <- compcoe %>% filter(species == "POAL")
str(poaals)
poaals

##making year x variables for graphs
(x_yrs<- seq(from=2007,to=2021,by=1))

##making size x variables for graphs
(size_dummy<-seq(from=0,to=max(log(poaals$size_t),na.rm=T),by=0.1))

##coercing year into a factor
year_tf <- as.factor(poaals$year_t)

#creating logistic function 
logistic<-function(x){1/(1+exp(-x))}

##FLOWERING TILLERS AS RESPONSE
###################################################
## Tom driving

##model for the probability of flowering
## simple model of the mean for overall flowering probability
poal_flow_mod0<- glm(flw_count_t>0~1,family="binomial", data=poaals)
logistic(coef(poal_flow_mod0)[1])

## add size dependence
poal_flow_mod1<- glm(flw_count_t>0~1+log(size_t),family="binomial", data=poaals)

plot(log(poaals$size_t),jitter(as.integer(poaals$flw_count_t>0),amount=0.02))
lines(x=size_dummy,y=logistic(coef(poal_flow_mod1)[1]+coef(poal_flow_mod1)[2]*size_dummy))

## now add endophyte
poal_flow_mod2<- glm(flw_count_t>0~endo_01+log(size_t),family="binomial", data=poaals)
plot(log(poaals$size_t),jitter(as.integer(poaals$flw_count_t>0),amount=0.02))

coef(poal_flow_mod2)

lines(x=size_dummy,y=logistic(coef(poal_flow_mod2)[1]+coef(poal_flow_mod2)[3]*size_dummy), col="goldenrod", lwd=2)
lines(x=size_dummy,y=logistic(coef(poal_flow_mod2)[1]+coef(poal_flow_mod2)[2]+coef(poal_flow_mod2)[3]*size_dummy), col="slateblue", lwd=2)

## now add interaction between endophyte and size
poal_flow_mod3<- glm(flw_count_t>0~endo_01*log(size_t),family="binomial", data=poaals)
coef(poal_flow_mod3)
plot(log(poaals$size_t),jitter(as.integer(poaals$flw_count_t>0),amount=0.02))

lines(x=size_dummy,y=logistic(coef(poal_flow_mod3)[1]+coef(poal_flow_mod3)[3]*size_dummy), col="goldenrod", lwd=2)
lines(x=size_dummy,y=logistic(coef(poal_flow_mod3)[1]+coef(poal_flow_mod3)[2]+(coef(poal_flow_mod3)[3]+coef(poal_flow_mod3)[4])*size_dummy), col="slateblue", lwd=2)

## now add interaction year random effects
poal_flow_mod4<- glmer(flw_count_t>0~endo_01*log(size_t)+(1|year_t),family="binomial", data=poaals)
summary(poal_flow_mod4)
coef(poal_flow_mod4)
ranef(poal_flow_mod4)
fixef(poal_flow_mod4)

ranef(poal_flow_mod4)$year_t[2,1]

flow_mod4_Eminus_intercepts<-coef(poal_flow_mod4)$year_t[,1]
flow_mod4_Eminus_slopes<-coef(poal_flow_mod4)$year_t[,3]
flow_mod4_Eplus_intercepts<-coef(poal_flow_mod4)$year_t[,1]+coef(poal_flow_mod4)$year_t[,2]
flow_mod4_Eplus_slopes<-coef(poal_flow_mod4)$year_t[,3]+coef(poal_flow_mod4)$year_t[,4]

plot(log(poaals$size_t),jitter(as.integer(poaals$flw_count_t>0),amount=0.02))

lines(x=size_dummy,y=logistic(flow_mod4_Eminus_intercepts+flow_mod4_Eminus_slopes*size_dummy), col="goldenrod")
lines(x=size_dummy,y=logistic(flow_mod4_Eplus_intercepts+flow_mod4_Eplus_slopes*size_dummy), col="slateblue")


plot(poaals$year_t,jitter(as.integer(poaals$flw_count_t>0),amount=0.02))

lines(x=x_yrs,y=logistic(flow_mod4_Eminus_intercepts+flow_mod4_Eminus_slopes), col="goldenrod")
lines(x=x_yrs,y=logistic(flow_mod4_Eplus_intercepts+flow_mod4_Eplus_slopes), col="slateblue")

## now add year random slopes
poal_flow_mod5<- glmer(flw_count_t>0~endo_01*log(size_t)+(endo_01|year_t),family="binomial", data=poaals)
ranef(poal_flow_mod5)
coef(poal_flow_mod5)$year_t
summary(poal_flow_mod5)

plot(poaals$year_t,jitter(as.integer(poaals$flw_count_t>0),amount=0.02))

flow_mod5_Eminus_intercepts<-coef(poal_flow_mod5)$year_t[,1]
flow_mod5_Eminus_slopes<-coef(poal_flow_mod5)$year_t[,3]
flow_mod5_Eplus_intercepts<-coef(poal_flow_mod5)$year_t[,1]+coef(poal_flow_mod5)$year_t[,2]
flow_mod5_Eplus_slopes<-coef(poal_flow_mod5)$year_t[,3]+coef(poal_flow_mod5)$year_t[,4]

lines(x=x_yrs,y=logistic(flow_mod5_Eminus_intercepts), col="goldenrod", lty=2, type="b")
lines(x=x_yrs,y=logistic(flow_mod5_Eplus_intercepts), col="slateblue", lty=2, type="b")

##Tom adding standardization of size variable
hist(log(poaals$size_t))
poaals$log_tillers_centered <- log(poaals$size_t) - mean(log(poaals$size_t),na.rm=T)
##equivaently:
hist(scale(log(poaals$size_t),center=T,scale=F))
## now refit model
poal_flow_mod6<- glmer(flw_count_t>0~endo_01*log_tillers_centered+(endo_01|year_t),family="binomial", data=poaals)
flow_mod6_Eminus_intercepts<-coef(poal_flow_mod5)$year_t[,1]
flow_mod6_Eplus_intercepts<-coef(poal_flow_mod5)$year_t[,1]+coef(poal_flow_mod5)$year_t[,2]
lines(x=x_yrs,y=logistic(flow_mod6_Eminus_intercepts), col="goldenrod", lty=2, type="b")
lines(x=x_yrs,y=logistic(flow_mod6_Eplus_intercepts), col="slateblue", lty=2, type="b")

plot(x=x_yrs,y=logistic(flow_mod6_Eminus_intercepts), col="goldenrod", lty=2, type="b")
lines(x=x_yrs,y=logistic(flow_mod6_Eplus_intercepts), col="slateblue", lty=2, type="b")


###############################################################################
##SURVIVAL AS RESPONSE

##model for the probability of surviving
## simple model of the mean for survival probability

poal_surv_mod0<- glm(surv_t1>0~1,family="binomial", data=poaals)
logistic(coef(poal_surv_mod0)[1])
summary(poal_surv_mod01)
#OR 
poal_surv_mod01<- glm(surv_t1~1,family="binomial", data=poaals)
logistic(coef(poal_surv_mod0)[1])

## add size dependence
poal_surv_mod1<- glm(surv_t1>0~1+log(size_t),family="binomial", data=poaals)

plot(log(poaals$size_t),jitter(as.integer(poaals$surv_t1>0),amount=0.02))
lines(x=size_dummy,y=logistic(coef(poal_surv_mod1)[1]+coef(poal_surv_mod1)[2]*size_dummy))

## now add endophyte
poal_surv_mod2<- glm(surv_t1>0~endo_01+log(size_t),family="binomial", data=poaals)
plot(log(poaals$size_t),jitter(as.integer(poaals$surv_t1>0),amount=0.02))

coef(poal_surv_mod2)

lines(x=size_dummy,y=logistic(coef(poal_surv_mod2)[1]+coef(poal_surv_mod2)[3]*size_dummy), col="goldenrod", lwd=2)
lines(x=size_dummy,y=logistic(coef(poal_surv_mod2)[1]+coef(poal_surv_mod2)[2]+coef(poal_surv_mod2)[3]*size_dummy), col="slateblue", lwd=2)

## now add interaction between endophyte and size
poal_surv_mod3<- glm(surv_t1>0~endo_01*log(size_t),family="binomial", data=poaals)
coef(poal_surv_mod3)
plot(log(poaals$size_t),jitter(as.integer(poaals$surv_t1>0),amount=0.02))

lines(x=size_dummy,y=logistic(coef(poal_surv_mod3)[1]+coef(poal_surv_mod3)[3]*size_dummy), col="goldenrod", lwd=2)
lines(x=size_dummy,y=logistic(coef(poal_surv_mod3)[1]+coef(poal_surv_mod3)[2]+(coef(poal_surv_mod3)[3]+coef(poal_surv_mod3)[4])*size_dummy), col="slateblue", lwd=2)

## now add interaction year random effects
poal_surv_mod4<- glmer(surv_t1>0~endo_01*log(size_t)+(1|year_t),family="binomial", data=poaals)
summary(poal_surv_mod4)
coef(poal_surv_mod4)

plot(log(poaals$size_t),jitter(as.integer(poaals$surv_t1>0),amount=0.02))

surv_mod4_Eminus_intercepts<-coef(poal_surv_mod4)$year_t[,1]
surv_mod4_Eminus_slopes<-coef(poal_surv_mod4)$year_t[,3]
surv_mod4_Eplus_intercepts<-coef(poal_surv_mod4)$year_t[,1]+coef(poal_surv_mod4)$year_t[,2]
surv_mod4_Eplus_slopes<-coef(poal_surv_mod4)$year_t[,3]+coef(poal_surv_mod4)$year_t[,4]

lines(x=size_dummy,y=logistic(surv_mod4_Eminus_intercepts+surv_mod4_Eminus_slopes*size_dummy), col="goldenrod")
lines(x=size_dummy,y=logistic(surv_mod4_Eplus_intercepts+surv_mod4_Eplus_slopes*size_dummy), col="slateblue")

plot(poaals$year_t,jitter(as.integer(poaals$surv_t1>0),amount=0.02))

lines(x=x_yrs,y=logistic(surv_mod4_Eminus_intercepts+surv_mod4_Eminus_slopes), col="goldenrod")
lines(x=x_yrs,y=logistic(surv_mod4_Eplus_intercepts+surv_mod4_Eplus_slopes), col="slateblue")

## now add year random slopes
poal_surv_mod5<- glmer(surv_t1>0~endo_01*log(size_t)+(endo_01|year_t),family="binomial", data=poaals)
coef(poal_flow_mod5)$year_t
summary(poal_flow_mod5)

plot(poaals$year_t,jitter(as.integer(poaals$surv_t1>0),amount=0.02))

surv_mod5_Eminus_intercepts<-coef(poal_surv_mod5)$year_t[,1]
surv_mod5_Eminus_slopes<-coef(poal_surv_mod5)$year_t[,3]
surv_mod5_Eplus_intercepts<-coef(poal_surv_mod5)$year_t[,1]+coef(poal_surv_mod5)$year_t[,2]
surv_mod5_Eplus_slopes<-coef(poal_surv_mod5)$year_t[,3]+coef(poal_surv_mod5)$year_t[,4]

lines(x=x_yrs,y=logistic(surv_mod5_Eminus_intercepts), col="goldenrod", lty=2, type="b")
lines(x=x_yrs,y=logistic(surv_mod5_Eplus_intercepts), col="slateblue", lty=2, type="b")

##Tom adding standardization of size variable
hist(log(poaals$size_t))
poaals$log_tillers_centered <- log(poaals$size_t) - mean(log(poaals$size_t),na.rm=T)
##equivaently:
hist(scale(log(poaals$size_t),center=T,scale=F))
## now refit model
poal_surv_mod6<- glmer(surv_t1>0~endo_01*log_tillers_centered+(endo_01|year_t),family="binomial", data=poaals)
surv_mod6_Eminus_intercepts<-coef(poal_surv_mod5)$year_t[,1]
surv_mod6_Eplus_intercepts<-coef(poal_surv_mod5)$year_t[,1]+coef(poal_surv_mod5)$year_t[,2]

plot(x=x_yrs,y=logistic(surv_mod6_Eminus_intercepts), col="goldenrod", lty=2, type="b")
lines(x=x_yrs,y=logistic(surv_mod6_Eplus_intercepts), col="slateblue", lty=2, type="b")

#adding other effects variables
poal_surv_mod7<- glmer(surv_t1>0~endo_01*log_tillers_centered+original+(endo_01|year_t)+(1|plot),family="binomial", data=poaals)
summary(poal_surv_mod7)
coef(poal_surv_mod7)

surv_mod7_Eminus_intercepts<-coef(poal_surv_mod7)$year_t[,1]
surv_mod7_Eplus_intercepts<-coef(poal_surv_mod7)$year_t[,1]+coef(poal_surv_mod7)$year_t[,2]

plot(x=x_yrs,y=logistic(surv_mod7_Eminus_intercepts), col="goldenrod", lty=2, type="b")
lines(x=x_yrs,y=logistic(surv_mod7_Eplus_intercepts), col="slateblue", lty=2, type="b")

