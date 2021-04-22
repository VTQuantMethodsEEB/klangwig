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

#zero-inflated (mixture) model
g.zinf <- zeroinfl(opalinus ~ time, 
                   dist = "negbin",
                   data = liz)
summary(g.zinf)

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

## Exponentiated coefficients
expCoef <- exp(coef((g.hurd)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(hurdlePart))
colnames(expCoef) <- c("Count_model","Zero_hurdle_model")
expCoef

#Interpretation: (Hurdle model) The occurence of oplalinus early (baseline) odds of having a positive count vs zero is 3.00. 
#This odds is increased by 2.33 times by late in the day, and 2 times by midday. 
#(Positive count model) Among occurences where opalinus occur, 
#the average count is 4.6 early in the day. This is increased by late in the day by 0.71, and midday 1.9 
#when opalinus is present.


##Interepration of true zero-inflated model
expCoef <- exp(coef((g.zinf)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(hurdlePart))
colnames(expCoef) <- c("Count_model","Zero_inflation_model")
expCoef

#Interpretation: (Zero-inflation model) 
#The baseline odds ofopalinus being present is is 0.102. 
#The odds is increase by time late (0.00005) and time midday 0.44. 
#(Count model) The baseline number of opalinus (including some 0s) is 5.10 early in the day. 
#this is increased late and midday. 


