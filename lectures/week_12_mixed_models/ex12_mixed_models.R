
rm(list=ls()) # clears workspace

#ex12_mixed_models
##load important packages##
library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)

#probably a new package
#install.packages("lme4")
library(lme4)
head(sleepstudy)
#another package
#install.packages("glmmTMB")
library(glmmTMB)

#lets take a look at the data
library(ggplot2); theme_set(theme_bw())
q0 <- (ggplot(sleepstudy, aes(Days, Reaction, colour = Subject))
       + geom_point())  ## points only, use later
print(q0+geom_line())

#run models
lm2 <- lmer(Reaction~Days + (1|Subject),data=sleepstudy) ## rand intercept
lm3 <- lmer(Reaction~Days + (Days|Subject),data=sleepstudy) ## rand slopes and intercepts

#lets look at the first model
summary(lm2)
summary(lm3)

#we don't get a p-value, only a t value
#this is because understanding the df with random effects is hard
#my recommendation: estimate the random effects conservative (e.g. the largest number of df they could potentially take up)
#here, we have 18 subjects, plus the fixed effect Days (which is continous)
#therefore, our df = Numbers of obs (180) - (all the levels of fixed effects) - (all the levels of random effects)
#180 - 18 - 1
180 - 18 - 1
#161
t.value = 13.02
p.value = 2*pt(t.value, df = 161, lower=FALSE)
#draw from the t distribution to get the probability (p-value) at your calculated t-value
p.value
#a good rule of thumb is that if you have a lot of data, you want a t-value > 2

#what are diffs between the models above?
pp <- expand.grid(Days=0:9,Subject=levels(sleepstudy$Subject))
#create a new data frame with all the stuff for each subjuet
pp2 <- cbind(pp,Reaction=predict(lm2,newdata=pp))
#make a new dataframe, pp2, and predict lm2
pp3 <- cbind(pp,Reaction=predict(lm3,newdata=pp))
#make a new dataframe, pp2, and predict lm3

print(q0
      + geom_line(data=pp2)
      + geom_line(data=pp3,lty=2))
#the dashed lines are nested and the solid line are not!


##NOW FOR A GLMM##
bat = read.csv("bat_data.csv")
head(bat)

#make a year column
bat$date = as.Date(bat$date, "%m/%d/%y")
bat$year = format(bat$date, "%y")
bat$year = as.numeric(bat$year)
bat$year = bat$year - min(bat$year)

#drop a species that dies too early
bat = subset(bat, species!="MYSE")
#and substrate
bat = subset(bat, species!="SUBSTRATE")

library(lme4)
#glms - binomial
gm1 = glmer(gd~species + (1|site),data=bat, family = "binomial")
summary(gm1)
#the GLM version of this will give us p-values
library(car)
Anova(gm1, type=3)

gm2 = glmer(gd~species*year + (1|site),data=bat, family = "binomial")
summary(gm2)
library(effects)
plot(allEffects(gm2))

anova(gm1,gm2)

#let's predict g2
newdat = expand.grid(species = unique(bat$species),
                     year=seq(min(bat$year),max(bat$year),by = .05),
                     site = unique(bat$site)
                     )
newdat$yhat = predict(gm2,newdata= newdat,re.form=NA,type="response")
#note - I use response to transform from logit space to response variable space
#I used re.form to drop random effects because I don't care about the site effects

#let's plot the prediction
r=ggplot(data=newdat, aes(x=year,y=yhat,col=species))+ 
  geom_line(size=1)+
  geom_point(data = bat, aes(x = jitter(year), y = gd),size=3, shape = 1)+
  ylab("Pd Prevalence")+
  xlab("Year")+
  coord_cartesian(ylim=c(-0.1,1.1))+ #zoom in
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)

#what if I hadn't dropped the site effects?

newdat$yhat = predict(gm2,newdata= newdat,type="response")
r=ggplot(data=newdat, aes(x=year,y=yhat,col=species))+ 
  geom_line(size=1)+
  geom_point(data = bat, aes(x = jitter(year), y = gd),size=3, shape = 1)+
  ylab("Pd Prevalence")+
  xlab("Year")+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)

#yikes

##what if we want to predict categorical
newdat2 = expand.grid(species = unique(bat$species),
                     site = unique(bat$site)
)
newdat2$yhat = predict(gm1,newdata= newdat2,re.form=NA,type="response")
#note - I use response to transform from logit space to response variable space
#I used re.form to drop random effects because I don't care about the site effects

#let's plot the prediction
r=ggplot(data=newdat2, aes(x=species,y=yhat))+ 
geom_point(color="red")  +
geom_point(data = bat, aes(x = species, y = gd),size=3, shape = 1)+
  ylab("Pd Prevalence")+
  xlab("Year")+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)


#negative binomial example with mixed models#
#ex12_nb_mixed

liz = read.csv("lizards.csv")

library(lme4)

## run mixed model
m.nb <- glmer.nb(N ~ time*light + (1|height), data=liz)
#look at mixed model
summary(m.nb)

#poisson version
m.po <- glmer(N ~ time*light + (1|height), data=liz, family = poisson)
#look at mixed model
summary(m.po)

AIC(m.po, m.nb)
##glmmTMB

#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
#Note - this package is rapidly being developed!

head(liz)

tmb.mod1 = glmmTMB(N ~ time*light + (1|height), data=liz, family=nbinom2())
summary(tmb.mod1)
#same result as above
#look at weird way we coded family

#specify zero inflated

tmb.mod2 = glmmTMB(N ~ time*light + (1|height), data=liz, 
                   ziformula = ~1,
                   family=poisson)
summary(tmb.mod2)
#similar output


tmb.mod3 = glmmTMB(N ~ time*light + (1|height), data=liz, 
                   family=poisson)
summary(tmb.mod3)

AIC(tmb.mod1, tmb.mod2, tmb.mod3)

#hurdle version

hurdle.tmb = update(tmb.mod2,
       ziformula=~.,
       data=liz,
       family=truncated_nbinom1)
hurdle.tmb
summary(hurdle.tmb)
##this is not fitting friends! do not ignore!


