rm(list=ls()) # clears workspace

##linear model paramters##
##week 7###

###some data###
forest <- c(9, 6, 4, 6, 7, 10)
field  <- c(12, 9, 12, 10)
ants <- data.frame(
  place=rep(c("field","forest"),
            c(length(field), length(forest))),
  colonies=c(field,forest),
  observers=c(1,3,2,1,5,2,1,2,1,1)
)

##utility function for pretty printing
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)

## Simple models
head(ants)

lm1 <- lm(colonies~place, data = ants)
summary(lm1)

pr(lm1 <- lm(colonies~place,data=ants))

#- The `(Intercept)` row refers to $\beta_1$, which is the mean density in the "field" sites ("field" comes before "forest").

##Get the estimates per group, rather than the differences
pr(lm0 <- lm(colonies~place-1,data=ants))
#now, these models are the values of the variables rather than the differences
#Note - how you would get these "groups" may be specific to your model formula. Be careful generalizing.


##How should I interpret my output?

#my go to:
predict(lm1,newdata=data.frame(place=c("field","forest")),
        interval="confidence")

#you can also predictions to your dataframe
ants$yhat=predict(lm1)
head(ants)

#you can also predict using a new dataset
lm2 <- lm(colonies~observers, data = ants)
summary(lm2)

#note - this model isn't significant so we don't need to predict it, but pretend it is...
new.ants = expand.grid(place = c("field","forest"), observers = seq(0:10))
new.ants$yhat=predict(lm2, newdata = new.ants)
View(new.ants)


##other helpful packages
pr(lm1 <- lm(colonies~place,data=ants))

library(effects)
summary(allEffects(lm1))

#lsmeans
library(lsmeans)
#this package will be deprecated soon and we will have to move, but for now, let's keep using it!
lsmeans(lm1,specs = ~place)
#this is still a useful vignette:
#https://cran.r-project.org/web/packages/lsmeans/vignettes/using-lsmeans.pdf

#plot the effects
plot(allEffects(lm1))

pr(lm3 <- lm(colonies~place*observers,data=ants))
plot(allEffects(lm3))

##More than two levels
lizards <- read.csv("lizards.csv")



l2 <- lm(colonies~observers*place, data = ants)
summary(l2)

##diagnostic plots
## Diagnostic plots in code...

head(mtcars)

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
#what is the influence of dispersion on MPG?
mod_1 <- lm(mpg ~ disp, data=mtcars)  # linear model
plot(mod_1)

## Correlation plots
##examine collinearity among variables
##which pairs are correlated?
library(car)
mod2 <- lm(mpg ~ ., data=mtcars)
summary(mod2)
#this says mpg by all vars

library(corrplot)
corrplot(cor(mtcars[, -1])) #give me everything but what is being predicted (mpg)
#be careful with vars that are dark blue
#=> Interpreted from plot.
#=> Correlated pairs: 
#=> - disp, cyl, hp, wt
#=> - gear, am
#=> - hp, carb
#- dark blue = bad


library(arm)
dev.off()

## Basic tools
l2 <- lm(colonies~observers*place, data = ants)
summary(l2)

#coefplot(l2)

## Multiple comparisons
forest <- c(9, 6, 4, 6, 7, 10)
field  <- c(12, 9, 12, 10)
playground <- c(1,0,3,2,1)
ants <- data.frame(
  place=rep(c("field","forest","playground"),
            c(length(field), length(forest),length(playground))),
  colonies=c(field,forest,playground)
)

ants
##check car package##Anova###
l2 = aov(colonies~place, data = ants);summary(l2)
l2 = lm(colonies~place, data = ants);anova(l2)

summary(l2); l2
TukeyHSD(l2)


library(multcomp)
ga = glht(l2, linfct = mcp(place = "Tukey"))
summary(ga)

## Plotting

yhats=predict(l2,interval = "confidence")
#note this gives a value for each value of your existing dataset
?predict.lm
#can specificy newdata frame to predict
dat.new=expand.grid(place=c("playground","field","forest"))
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


