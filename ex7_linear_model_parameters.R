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

#you can also add predictions to your dataframe
ants$yhat=predict(lm1)
head(ants)

#you can also predict using a new dataset
lm2 <- lm(colonies~observers, data = ants)
summary(lm2)

#note - this model isn't significant so we don't need to predict it, but pretend it is...
new.ants = expand.grid(place = c("field","forest"), observers = seq(1:10))

new.ants$yhat=predict(lm2, newdata = new.ants)
View(new.ants)


##other helpful packages
pr(lm1 <- lm(colonies~place,data=ants))

library(effects)
summary(allEffects(lm1))

#lsmeans
library(lsmeans)
#this package will be deprecated soon and we will have to change, but for now, let's keep using it!
lsmeans(lm1,specs = ~place)
#this is still a useful vignette:
#https://cran.r-project.org/web/packages/lsmeans/vignettes/using-lsmeans.pdf

#plot the effects
plot(allEffects(lm1))

pr(lm3 <- lm(colonies~place*observers,data=ants))
plot(allEffects(lm3))

##More than two levels
lizards <- read.csv("lizards.csv")

#plot the data
library("ggplot2"); theme_set(theme_bw()+
                               theme(panel.spacing=grid::unit(0,"lines")))
library(dplyr);library(tidyr)
mliz <- lizards %>%
  select(grahami,height,diameter,light,time) %>%
  gather(variable,value,-grahami)

ggplot(mliz,aes(x=value,y=grahami))+
  geom_boxplot(fill="lightgray")+
  facet_wrap(~variable,scale="free_x",nrow=1)+
  geom_hline(yintercept=mean(lizards$grahami),colour="red",lwd=1,alpha=0.4) #line is at mean number of lizards

#run a regression on the time variable
pr(lm(grahami~time,data=lizards))


#time is categorical, not continous, so our contrasts don't make much sense
lizards <- mutate(lizards,
                 time=factor(time,
                             levels=c("early","midday","late")))
#mutate computes and adds new variable(s). Preserves existing variables.
levels(lizards$time)

#using relevel
levels(lizards$time) #here are the existing levels
lizards$time <- relevel(lizards$time, ref = "late" ) #change reference to late
pr(lm(grahami~time,data=lizards))

#get back to earlier levels
lizards <- mutate(lizards,
                  time=factor(time,
                              levels=c("early","midday","late")))

library("MASS")
pr(lm(grahami~time,data=lizards,contrasts=list(time=contr.sdif)))

## Multiple treatments and interactions
## Categorical variables

## Additive models

#Consider the `light` variable in addition to `time`
#light and time might both affect lizard perching behavior
head(lizards)
pr(lmTL1 <- lm(grahami~time+light,data=lizards))
#the intercept is early, shady. timemiday and timelate are the changes in the "shady" line. 
#lightsunny, gives us the intercept difference for all the time variables under sunny conditions - e.g. each point moves down -19.32

## we can plot this to make sure

pp <- with(lizards,expand.grid(time=levels(time),light=levels(light)))
pp$grahami <- predict(lmTL1,newdata=pp)

ggplot(pp,aes(x=time,y=grahami,colour=light))+
  geom_point()+
  geom_line(aes(group=light))

#what are the p-values giving us?
## Other ways of doing multiple comparisons

library(lsmeans)
library(multcompView)
lsmeans(lmTL1, specs = "time", contr = "pairwise")

lsm1<-lsmeans(lmTL1,pairwise~time)
cld(lsm1,by = NULL, Letters = "ABCDEFGHIJ") 

## Interactive Models
pr(lmTL2 <- lm(grahami~time*light,data=lizards))

## Releveling interactive models
lizards$time <- relevel(lizards$time, ref = "midday" )
pr(lmTL2 <- lm(grahami~time*light,data=lizards))

## Using lsmeans
# we can add the interaction for all pairwise comparisons
lsm2<-lsmeans(lmTL2,pairwise~time*light)
lsm2

##plotting the interactive model

pp2 <- pp
pp2$grahami <- predict(lmTL2,newdata=pp)

ggplot(pp2,aes(x=time,y=grahami,colour=light))+geom_point()+
  geom_line(aes(group=light))

##more graphics

lizards <- mutate(lizards,
                  time=factor(time,
                              levels=c("early","midday","late")))

pr(lmTL2 <- lm(grahami~time*light,data=lizards))

library(dotwhisker)

dwplot(list(additive=lmTL1,interaction=lmTL2))+
  geom_vline(xintercept=0,lty=2)

plot(allEffects(lmTL2))


