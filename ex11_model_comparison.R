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
anova(l4,l3)

#anova works slightly differently depending on which models we are comparing
h1 = glm(grahami~light*time,data = liz, family = poisson); summary(h1)
h2 = glm(grahami~light+time,data = liz, family = poisson); summary(h2)
#DRAW ATTENTION TO THIS! NEED TO RE-ORDER!
#with a regular glm, we need to specify LRT
anova(h1,h2, test = "LRT")
anova(h2,h1, test = "LRT") #this is correct!Order from least complicated to most complicated


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
aictab(cand.set=list(h1,h2,h3,h4,h5),modnames=c("h1","h2","h3","h4","h5"), second.ord = F)#AIC table

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


###ADVANCED - K-FOLD CROSS-VALIDATION###
library(pROC)

nreps<-1000 # setting my reps
num.in.each.fold = nrow(bat)/5 #dividing by the total number of folds I want 
Folds<-c(rep(1, num.in.each.fold), rep(2, num.in.each.fold), rep(3, num.in.each.fold), rep(4, num.in.each.fold), rep(5, num.in.each.fold))
bat$Prob<-NA
AUC<-rep(NA, nreps)
for(j in 1:length(AUC)) {
bat$Fold<-sample(Folds, replace=F) #this says - assign a group (e.g. a fold) to each observation
#Test 1
model_LOGISTIC = glm(gd~species+site,family=binomial(),data=bat[bat$Fold!=1,] )
summary(model_LOGISTIC)
bat$Prob[bat$Fold==1] = predict(model_LOGISTIC, newdata=bat[bat$Fold==1,], type="response", re.form=NA)

#Test 2
model_LOGISTIC = glm(gd~species+site,family=binomial(),data=bat[bat$Fold!=2,] )
summary(model_LOGISTIC)
bat$Prob[bat$Fold==2] = predict(model_LOGISTIC, newdata=bat[bat$Fold==2,], type="response", re.form=NA)

#Test 3
model_LOGISTIC = glm(gd~species+site,family=binomial(),data=bat[bat$Fold!=3,] )
summary(model_LOGISTIC)
bat$Prob[bat$Fold==3] = predict(model_LOGISTIC, newdata=bat[bat$Fold==3,], type="response", re.form=NA)

#Test 4
model_LOGISTIC = glm(gd~species+site,family=binomial(),data=bat[bat$Fold!=4,] )
summary(model_LOGISTIC)
bat$Prob[bat$Fold==4] = predict(model_LOGISTIC, newdata=bat[bat$Fold==4,], type="response", re.form=NA)

#Test 5
model_LOGISTIC = glm(gd~species+site,family=binomial(),data=bat[bat$Fold!=5,] )
summary(model_LOGISTIC)
bat$Prob[bat$Fold==5] = predict(model_LOGISTIC, newdata=bat[bat$Fold==5,], type="response", re.form=NA)
#Get AUC
AUC[j]<-as.numeric(auc(roc(response = bat$gd, predictor = bat$Prob, plot=FALSE, ci=TRUE)))
print(paste(j, AUC[j], sep=":"))
}

#Plot the ROC curve and get the AUC estimate 
ROC<-roc(response = bat$gd, predictor = bat$Prob, plot=TRUE, ci=TRUE, legacy.axes=TRUE)
ROC$auc
#Area under the curve: 0.7275
#95% CI: 0.6885-0.7664 (DeLong)
# The AUC can be defined as the probability that the fit model will score a randomly drawn 
#positive sample higher than a randomly drawn negative sample

hist(AUC, main = NA)
mean(AUC) #0.73 
abline(v=mean(AUC), lty=2)
summary(AUC)
lil<-0.025
big<-0.975
quantile(AUC, c(lil, big))
#2.5%     97.5% 
#0.72 0.74
Data<-as.data.frame(AUC); head(Data)
b=ggplot(data=Data, aes(AUC)) +
  geom_histogram(binwidth = 0.005) +
  geom_vline(xintercept = mean(AUC), linetype="dashed") +
  xlab("AUC")+
  ylab("Number of iterations")+
  theme_bw()+
  theme(panel.grid = element_blank(),axis.title=element_text(size=15),
        axis.text=element_text(size=10), axis.line=element_line(),
        legend.position = "none", legend.text = element_text(size=20,face="italic"),strip.text = element_text(size=25,face="italic"))
b

#plot output from single run
plot.roc(ROC)
a=ggroc(ROC, size=1, legacy.axes = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  theme_bw()+
  theme(panel.grid = element_blank(),axis.title=element_text(size=15),
        axis.text=element_text(size=10), axis.line=element_line(),
        legend.position = "none", legend.text = element_text(size=20,face="italic"),strip.text = element_text(size=25,face="italic"))
a
