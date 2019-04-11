#ex11_model_comparison
##load important packages##
library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)
#these may be new
library(car)
library(AICcmodavg)
#library(qpcR)

#make a function for easier printing

pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)

#read in data
bat = read.csv("bat_data.csv")
head(bat)
bat$date = as.Date(bat$date, "%m/%d/%y")

#Forward step-wise selection
g1 = glm (gd~species, data = bat, family = binomial)
g2 = glm (gd~site, data = bat, family = binomial)
g3 = glm (gd~date, data = bat, family = binomial)
g4 = glm (gd~count, data = bat, family = binomial)
g5 = glm (gd~temp, data = bat, family = binomial)

#Car:Anova gives you the variable-level p-value that you would want to step-wise regression
Anova(g1)
Anova(g2)
Anova(g3)
Anova(g4)
Anova(g5)

#species and date are equally significant - select one
g1a = glm (gd~species+date, data = bat, family = binomial)
g1b = glm (gd~species+site, data = bat, family = binomial)
g1c = glm (gd~species+count, data = bat, family = binomial)
g1d = glm (gd~species+temp, data = bat, family = binomial)

Anova(g1a)
Anova(g1b)
Anova(g1c)
Anova(g1d)
#1a is best - include species + date - moving on!

g1a = glm (gd~species+date+site, data = bat, family = binomial)
g1b = glm (gd~species+date+count, data = bat, family = binomial)
g1c = glm (gd~species+date+temp, data = bat, family = binomial)
#get variable level p-values
Anova(g1a)
Anova(g1b)
Anova(g1c)

#site is the best predictor - let's add that
g1a = glm (gd~species+date+site+count, data = bat, family = binomial)
g1b = glm (gd~species+date+site+temp, data = bat, family = binomial)
Anova(g1a)
Anova(g1b)
#Okay - we must have our best model now because count and temperature aren't significant
#Is that true? What are some of the issues with this?

g1.new = glm (gd~count*temp, data = bat, family = binomial)
Anova(g1.new)
pr(g1.new)


##BACKWARDS STEPWISE##
liz = read.csv("lizards.csv")
head(liz)
l1 = glm.nb(grahami~height+diameter+light+time,data = liz)
Anova(l1)
#let's remove height then...but, wait
l2 = glm.nb(grahami~height+diameter+light*time,data = liz)
Anova(l2)
#now height is significant! The structure of the other variables makes a difference!
pr(l2)
pr(l1)

##Likelihood Ratio Tests###

head(liz)

l1 = glm.nb(grahami~light,data = liz)
l2 = glm.nb(grahami~light+time,data = liz)
l3 = glm.nb(grahami~light*time,data = liz)
l4 = glm.nb(grahami~1,data = liz) #null model example!

#this test light vs light +time
anova(l1,l2)
#this test light vs light *time
anova(l1,l3)
#can look at all 3
anova(l1,l2,l3,l4)

#anova works slightly differently depending on which models we are comparing
h1 = glm(grahami~light*time,data = liz, family = poisson)
h2 = glm(grahami~light+time,data = liz, family = poisson)
#DRAW ATTENTION TO THIS! NEED TO RE-ORDER!
#with a regular glm, we need to specify LRT
anova(h1,h2, test = "LRT")
anova(h2,h1, test = "LRT")

#the nice thing is if we use the wrong test, it will tell us!
anova(h1,h2, test = "F")

##Information Criteria###

###AIC first##
#the formula for AIC is very simple
#2*number of parameters - 2 ln(lik)
h1 = glm(grahami~light*time,data = liz, family = poisson)
h2 = glm(grahami~light+time,data = liz, family = poisson)
h3 = glm(grahami~light,data = liz, family = poisson)
h4 = glm(grahami~time,data = liz, family = poisson)
h5 = glm(grahami~1,data = liz, family = poisson)
#simple version
AIC(h1,h2,h3,h4,h5)

#tabular
aictab(cand.set=list(h1,h2,h3,h4,h5),modnames=c("h1","h2","h3","h4","h5"))#AIC table

#this function will give a nice AIC table, but calculating weights and delta AIC is very straightforward

#here all of the weight is for the first model
#lets look at a more nuanced version
n1 = glm.nb(grahami~height,data = liz)
n2 = glm.nb(grahami~diameter,data = liz)
n3 = glm.nb(grahami~diameter+height,data = liz)
n4 = glm.nb(grahami~diameter*height,data = liz)

#for AICc
n=nrow(liz)#or whatever the length of your df is
tabA = AIC(n1,n2,n3,n4)
#it would be nice to have AICC for a dataset this small
tabA$k<-c(n1$rank,n2$rank,n3$rank,n4$rank)
tabA$aiccs<-tabA$AIC+((2*tabA$k*(tabA$k+1))/(n-tabA$k-1))
#now order from smallest to biggest
tabA=tabA[order(tabA$aiccs),]
#calculate delta AIC
tabA$dAIC = tabA$aiccs - min(tabA$aiccs)
#you use the next two lines to get weights
tabA$edel<-exp(-0.5*tabA$dAIC) 
tabA$wt<-tabA$edel/sum(tabA$edel)
tabA

#what issue do we have here?
n5 = glm.nb(grahami~1,data = liz)

#now run this all again with n5!
tabA = AIC(n1,n2,n3,n4,n5)
#it would be nice to have AICC for a dataset this small
tabA$k<-c(n1$rank,n2$rank,n3$rank,n4$rank,n5$rank)
tabA$aiccs<-tabA$AIC+((2*tabA$k*(tabA$k+1))/(n-tabA$k-1))
#now order from smallest to biggest
tabA=tabA[order(tabA$aiccs),]
#calculate delta AIC
tabA$dAIC = tabA$aiccs - min(tabA$aiccs)
#you use the next two lines to get weights
tabA$edel<-exp(-0.5*tabA$dAIC) 
tabA$wt<-tabA$edel/sum(tabA$edel)
tabA

#what else?
pr(n2)
#the best model has a p-value of 0.1!


