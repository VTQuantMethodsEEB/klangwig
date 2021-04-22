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

#nb model
g.nb = glm.nb(opalinus~time,data=liz)
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
#write this before next class!
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
vuong(g.nb, g.zinf)


###INTERPRETATION##
## The hurdle part can be fit as follows.

liz$opalinus.p = 1
liz$opalinus.p[liz$opalinus<1] = 0
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
#the average count is 4.6 early in the day. This is decreased by late in the day by -0.34, and increase 
#midday 0.63 when opalinus is present.


##Interepration of true zero-inflated model
Coef <- coef((g.zinf))
Coef <- matrix(Coef, ncol = 2)
rownames(Coef) <- names(coef(hurdlePart))
colnames(Coef) <- c("Count_model","Zero_inflation_model")
Coef

#Interpretation: (Zero-inflation model) 
#The baseline odds ofopalinus being ABSENT (early) is is -2.27. 
#The odds of opalinus being absent by time late is lower (-7.62) and time midday (-0.81). 
#(Count model) The baseline number of opalinus (including some 0s) is 1.63 early in the day.
#this decreases slightly late and increases midday. 


