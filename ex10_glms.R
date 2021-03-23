rm(list=ls()) # clears workspace

##Week 10##

###1. GLMS###

?"glm"

liz = read.csv("lizards.csv")

unique(liz$time)
g1 = glm(N~time,data=liz, family="poisson");
summary(g1)

library(effects)
plot(allEffects(g1))

library(emmeans)
emmeans(g1, pairwise~time)
emmeans(g1, pairwise~time, type="response")


###BINOMIAL##
bat = read.csv("bat_data.csv")
head(bat)
bat$date = as.Date(bat$date, "%m/%d/%y")
#2/27/2015
str(bat$date)
#if you have a PC, it may be this:
#bat$date = as.Date(bat$date, "%m/%d/%Y")

library(ggplot2)
plot1=ggplot(data=bat,aes(x=date,y=gd,color=species))+
  geom_point(size=2,shape =1) #this adds points to graph
plot1

#run the binomial glm
g2 = glm(gd~date+species,data=bat, family="binomial");
summary(g2)

#What does this tell us?
dat.new=expand.grid(date=seq(from = min(bat$date),to = max(bat$date),length.out = 100),
                    species = unique(bat$species))

#weird output if we don't use "response:
predict(g2,newdata = dat.new)

#much more sensible output
dat.new$yhat  = predict(g2,type="response",newdata = dat.new)
bat$yhat2 = predict(g2,type="response")
head(dat.new)

head(bat)

plot1=ggplot(data=bat,aes(x=date,y=gd,color=species))+
  geom_point(size=2,shape =1) +
  geom_line(data=dat.new, aes(x=date,y=yhat,col = species))#+
  #geom_line(aes(x=date,y=yhat2,col = species))
plot1

#what if we have aggregated data?
b1<-aggregate(gd~date+species,FUN=mean, data=bat)
b2<-aggregate(gd~date+species,FUN=length, data=bat)
b1$N = b2$gd
head(b1)


#run the binomial glm with sample sizes as weights
g3 = glm(gd~date+species,data=b1,weights = N, family="binomial");
summary(g3)

summary(g2);summary(g3)
#these are identical

#sometimes plotting the aggregated data looks better too
plot1=ggplot(data=b1,aes(x=date,y=gd,color=species))+ #aggregated data file
  geom_point(size=2,shape =1) + #add real data points to plot
  geom_line(data=dat.new, aes(x=date,y=yhat,col = species)) #plot smooth lines from dat.new
plot1

##back to powerpoint

#ses and confidence intervals from binomial data

#binomial proportion confidence interval
head(b1)
b1$Nsuccess = b1$gd*b1$N
head(b1)
#load package epitools
library(epitools)
#calculate 95% confidence intervals
cis = binom.exact(b1$Nsuccess, b1$N)
#note this calculates an exact CI, and there are different methods for doing this which we won't get into here..
cis
b1$lCI = cis$lower
b1$uCI = cis$upper
head(b1)

#you can read more here:
#https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval

###getting prediction intervals##
preds  = predict(g2,type="link",newdata = dat.new, se.fit = T)
dat.new = cbind(dat.new, preds[1:2])#bind together se's and fitted points on your newdata
#get the inverse link function for your glm
ilink <- family(g2)$linkinv
#back transform the CIs (not the SEs!)
dat.new <- transform(dat.new, 
                     Fitted = ilink(fit), 
                     Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit)))
#fitted should be the same as yhat
head(dat.new)
#plot the output
plot1=ggplot(data=bat,aes(x=date,y=gd,color=species))+
  geom_point(size=2,shape =1) +
  facet_wrap(~species)+
  geom_line(data=dat.new, aes(x=date,y=yhat,col = species))+
  geom_ribbon(data = dat.new, aes(ymin = Lower, ymax = Upper, x = date,y=yhat),
              fill = "steelblue2", alpha = 0.2) 
plot1

##back to powerpoint

###Gamma GLMs###
set.seed(101)
bat$gdL2 = rgamma(length(bat$gdL),shape=1,rate=1)
g4 = glm(gdL2~date+species,data=bat, family=Gamma(link = inverse));
summary(g4)
#this is the default

#MAYBE Better,
g5 = glm(gdL2~date+species,data=bat, family=Gamma(link = log));
summary(g5)

#back to ppt##

##Poisson GLM##
head(liz)
g6 = glm(N~time,data=liz, family=poisson);
summary(g6)


#we have count data, so it might be a good idea to check for overdisperison
#one option - calculate directly
summary(g6)
326.27 / 20

library(AER)
#performs a dispersion test
dispersiontest(g6)

#ok - we have some indication that our data our overdispersed

#lets use a negative binomial instead
library(MASS)
#run a negative binomial glm - no need to use family
g7 = glm.nb(N~time,data=liz)
summary(g7)

#quasi-poisson
#g8 = glm(N~time,data=liz, family = "quasipoisson")
#summary(g8)

