#ex_zero_inflated_models

#load packages
library(MASS)

#new packages
#install.packages("lmtest")
library(lmtest)
#install.packages("pscl")
library(pscl)
#install.packages("nlme")
library(nlme)

#load the lizard data

liz = read.csv("lizards.csv")

head(liz)

#first, using LRT to compare distributions
#poisson model
g.pois = glm(opalinus~time,data=liz, family = "poisson")
summary(g.pois)

#overdispersed?
159.26/20
#probably
library(AER)
dispersiontest(g.pois)
#okay definitely

#nb model
g.nb = glm.nb(opalinus~time,data=liz)#this is negative binomial
summary(g.nb)


#lrt with p-value
#note - null first
#these models are nested because a poisson is a "special case" 
#of the negative binomial with one parameter

lrtest(g.pois,g.nb)

#AIC tells us the same
AIC(g.pois,g.nb)

#let's use a zero-inflated model
#question - do we think that is biologically appropriate?
#we will use a lizard species that has zeroes

###how to interpret output
#https://rpubs.com/kaz_yos/pscl-2
#also this
#https://stats.idre.ucla.edu/r/dae/zip/

#hurdle model first
g.hurd <- hurdle(opalinus ~ time,
                 dist    = "negbin",
                 data    = liz)
summary(g.hurd)

liz$yhat.hurd = predict(g.hurd, type = "response")


#zero-inflated (mixture) model
g.zinf <- zeroinfl(opalinus ~ time, 
                   dist = "negbin",
                   data = liz)
summary(g.zinf)

liz$yhat.zinf = predict(g.zinf, type = "response")
##compare predictions##
View(liz)
library(tidyverse)
liz %>%
  group_by(time)%>%
  summarise(hurd = mean(yhat.hurd),zinf = mean(yhat.zinf))
#these are nearly the same because there actually aren't very many 0s in this dataset

##another way
library(emmeans)
emmeans(g.zinf, specs = ~time)
emmeans(g.hurd, specs = ~time)

#we can compare this latter model to models with just Poisson or NB
vuong(g.pois, g.zinf)
#this says model 2 is better than model 1
vuong(g.nb, g.zinf)
#this says model 2 is not better than model 1


###INTERPRETATION##
## The hurdle part can be fit as follows.

liz$opalinus.p = 1
liz$opalinus.p[liz$opalinus<1] = 0

liz %>%
  group_by(time, opalinus.p)%>%
  summarise(n.zeroes = n())


hurdlePart <- glm(formula = opalinus.p ~ time,
                  data    = liz,
                  family  = binomial(link = "logit"))
coef(summary(hurdlePart))

##coefficients comparison
Coef <- coef((g.hurd))
Coef <- matrix(Coef, ncol = 2)
rownames(Coef) <- names(coef(hurdlePart))
colnames(Coef) <- c("Count_model","Zero_hurdle_model")
Coef

#Interpretation: (Hurdle model) The occurence of oplalinus early (baseline) odds of having a positive count vs zero is 1.09. 
#These odds of a positive count are increased by 0.85 late in the day, and 0.69 times by midday. 
#(Positive count model) Among occurences where opalinus occur, 
#the average count is 4.6 (coef 1.52), early in the day (See emmeans - coefs in Poisson space).
#This is decreased by late in the day by -0.34, and increase 
#midday 0.63 when opalinus is present.


##Interepration of true zero-inflated model
Coef <- coef((g.zinf))
Coef <- matrix(Coef, ncol = 2)
rownames(Coef) <- names(coef(hurdlePart))
colnames(Coef) <- c("Count_model","Zero_inflation_model")
Coef

#Interpretation: (Zero-inflation model) 
#The baseline odds of opalinus being ABSENT (early) is is -2.27. 
#The odds of opalinus being absent by time late is lower (-8.04) and time midday (-0.81). 
#(Count model) The baseline number of opalinus (including some 0s) is 1.63 early in the day.
#this decreases slightly late and increases midday. 

##How to run a zero-inflated mixed model

#using lizards, but note that my random effect is actually inappropriate
#not enough levels
install.package("glmmTMB")

library(glmmTMB)
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
#https://journal.r-project.org/archive/2017/RJ-2017-066/RJ-2017-066.pdf

fit_poisson <- glmmTMB(opalinus~time+(1|height),
                         data=liz,
                         #ziformula=~1,#apply 0 inflation to all observations and account for it
                         #ziformula=~time,#some 0 inflation is due to time
                         family=poisson)
summary(fit_poisson)

#install.packages("DHARMa")
library(DHARMa)
res <- simulateResiduals(fit_poisson)
plot(res)


fit_zipoisson <- glmmTMB(opalinus~time+(1|height),
                         data=liz,
                         #ziformula=~1,#apply 0 inflation to all observations and account for it
                         ziformula=~time,#some 0 inflation is due to time
                         family=poisson)
summary(fit_zipoisson)


res <- simulateResiduals(fit_zipoisson)
plot(res)


fit_zinbinom <- update(fit_zipoisson,family=nbinom2)

summary(fit_zinbinom)

library(bbmle)
AICtab(fit_poisson,fit_zipoisson,fit_zinbinom)
#note - slightly different answers here

##fit with a hurdle model
fit_hnbinom1 <- update(fit_zinbinom,
                       ziformula=~.,
                       data=liz,
                       family=truncated_nbinom1)

summary(fit_hnbinom1 )


