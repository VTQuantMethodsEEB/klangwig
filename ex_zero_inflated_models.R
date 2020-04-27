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

#let's us a zero-inflated model
#question - do we think that is biologically appropriate?
#we will use another lizard species that has zeroes

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

