rm(list=ls())

##linear models lecture##
##week 6###

###some data###
forest <- c(9, 6, 4, 6, 7, 10)
field  <- c(12, 9, 12, 10)
ants <- data.frame(
  place=rep(c("field","forest"),
            c(length(field), length(forest))),
  colonies=c(field,forest),
  observers=c(1,3,2,1,5,2,1,2,1,1)
)

## One-parameter variables
head(ants)

l1 <- lm(colonies~observers, data = ants)
lm(colonies~observers, data = ants)
summary(l1)
#[now write out equations]
# y (number of colonies) = ax +b
# y (number of colonies) = 0.1007x + 8.3087


##Diagnostic plots
## Diagnostic plots in code...

head(mtcars)

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
#what is the influence of displacement on MPG?
mod_1 <- lm(mpg ~ disp, data=mtcars)  # linear model
plot(mod_1)

resid(mod_1)
hist(resid(mod_1))
shapiro.test(resid(mod_1))
#null hypothesis is that the sample comes from a normally distributed population
#p < 0.05 is not normal
hist(mtcars$mpg)

##back to presentation

## Correlation plots
##examine collinearity among variables
##which pairs are correlated?
dev.off() 

library(car)
mod2 <- lm(mpg ~ ., data=mtcars)
summary(mod2)
#this says mpg by all vars

library(corrplot)
## corrplot 0.84 loaded
M <- cor(mtcars)#correlation matrix
corrplot(M, method = "circle")
corrplot(M, method = "number")
corrplot(cor(mtcars[, -1])) #give me everything but what is being predicted (mpg)
#be careful with vars that are dark blue and dark red
#=> Interpreted from plot.
#=> Correlated pairs: 
#=> - disp, cyl, hp, wt
#=> - gear, am
#=> - hp, carb
#- dark blue = bad

#One way to measure multicollinearity is the variance inflation factor (VIF), 
#assesses how much the variance of an estimated regression coefficient increases if your predictors are correlated. 
#If no factors are correlated, the VIFs will all be 1
#> 8-10 is cause for concern (sometimes ppl say even lower) 
vif(mod2)
#this tells us that some of the predictors are highly correlated with some other things in the model
#cyl and disp are really bad
#which ones? 
M <- cor(mtcars)
M
#which one to remove is both scientific and practical
dev.off()

## Basic tools
l2 <- lm(colonies~observers, data = ants)
summary(l2)
plot(l2)

l3 <- lm(colonies~place, data = ants, subset = place!="playground")
summary(l3)

#coefplot(l2)

## Multiple comparisons

#create a dataset with 3 places - forest, field, playground
forest <- c(9, 6, 4, 6, 7, 10)
field  <- c(12, 9, 12, 10)
playground <- c(1,0,3,2,1)
ants <- data.frame(
  place=rep(c("field","forest","playground"),
            c(length(field), length(forest),length(playground))),
  colonies=c(field,forest,playground)
)

ants

l2 = aov(colonies~place, data = ants);
summary(l2)
#aov fits an lm, but the main diff is the way summary prints out
#this type of summary output is a more traditional way of displaying results
#of an ANOVA; it doesn't tell us about differences between each parameter
#just the overall effect of the variable 'place'
l2 = lm(colonies~place, data = ants)
l2 #this is not very useful
summary(l2) #this is our bread and butter output
#but it is giving us the comparison to field only!
#it is telling how forest and playround differ from field
#forest has -3.75 ant colonies on average less than field (we know this from permutations)
#playground has - 9.35 fewer ant colonies than field
#this does not tell us about the difference between forest and playground
#although we can guestimate based on the standard errors
anova(l2)
#we can get the overall variable level effect
#using 'anova()' on our model object gives the same output as 'aov'
#in future lectures, we will use anova to compare two different models

l2 = aov(colonies~place, data = ants);summary(l2)
TukeyHSD(l2)
#this will give us all the differences from eachother
#with a Tukey p-value adjustment
#it only works on aov objects

library(multcomp)
ga = glht(l2, linfct = mcp(place = "Tukey"))
summary(ga)
#this package gives us another way of analyzing this

## Plotting ##
summary(l2)
yhats=predict(l2,interval = "confidence")
#note this gives a value for each value of your existing dataset
?predict.lm
#can specificy newdata frame to predict - will go through in detail
dat.new=expand.grid(place=c("playground","field","forest"))
#dat.new=data.frame(observers=seq(1:10))

yhats=predict(l2,newdata=dat.new,interval = "confidence")


##ggplot2##
head(mtcars)
library(ggplot2)
r=ggplot(data=mtcars, aes(x=wt, y=mpg))+ 
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)

#if you are running an anova - e.g. you have categories - you'll want:
#stat_summary(fun="mean", colour = "red", size = 1)+
  
r=ggplot(data=ants, aes(x=place, y=colonies))+ 
  geom_point()+
  stat_summary(fun.data = "mean_se", colour="red", size=1)+ 
  #these are the means and SEs- should be similar to anova output
  #for non-parametric bootstrapped confidence limits instead:
  #stat_summary(fun.data = "mean_cl_boot", colour="red", size=1)+ 
  #bootstrapping means to sample randomly, but replace the values
  #e.g. put them back in the jar
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)
