library(tidyverse)
library(effects)
library(emmeans)


# read in new dataset
lizards <- read.csv("lizards.csv")

# additive model
mod1 = lm(grahami~time + light,data=lizards)
summary(mod1)

# using effects to help with interpretation
plot(allEffects(mod1))

# using emmeans to help with comparisons
# output values for each condition
emmeans(mod1,specs=~time+light)
#pairwise for time only
emmeans(mod1, specs = "time", contr = "pairwise")
#all contrasts
all_comparisons <- emmeans(mod1, pairwise ~ time + light)
all_comparisons$contrasts
# all contrast from each other (tukey adj p-values - dont do this unless you want all these contrasts)

# interactive models
mod2 <- lm(grahami~time*light,data=lizards)
summary(mod2)

#change order of factors
lizards <- mutate(lizards,
                  time=factor(time,
                              levels=c("late","midday","early")))

# run model again
mod2 <- lm(grahami~time*light,data=lizards)
summary(mod2)


#use effects
plot(allEffects(mod2))

#use emmeans
emmeans(mod2,specs=~time*light)

# use all comparisoms
all_comparisons <- emmeans(mod2, pairwise ~ time * light)
all_comparisons$contrasts

##plotting the interactive model (additive not shown)
lizards$yhat = predict(mod2)

##plotting the interactive model
pp <- with(lizards,
           expand.grid(time=unique(time),
                       light=unique(light)))
#make a new dataframe with all the unique values of time
#and all the unique values light
#expand.grid says "give me every combination possible"

pp
#look at new data frame

pp$grahami <- predict(mod2,newdata=pp)
#this says- predict the number of grahami lizards using my new data frame, pp
#since pp has every combination possible, you will get a prediction for every value


ggplot(pp,aes(x=time,y=grahami,colour=light))+
  geom_point()+
  geom_line(aes(group=light))
#this is a plot of our model predictions
#but we need to add our actual data

###ADD RAW data to plot###
plot1 = ggplot(pp,aes(x=time,y=grahami,colour=light))+ #set up plot using predictions dataset
  geom_point(color="red")+ #plot the prediction
  geom_line(aes(group=light))+ #draw lines b/t predictions, group them by light conditions
  geom_point(data=lizards, aes(x=time,y=grahami,colour = light)) #add the observed data to the plot
plot1
#note - I am calling the original dataframe - lizards
#i am therefore setting up aes again


##what if I had a continuous variable?
#the number of opalinus lizards, which varies by time of day, influences the number of grahami
mod3 <- lm(grahami~time*opalinus, data=lizards)
summary(mod3)
allEffects(mod3)
plot(allEffects(mod3))
#emmtrends()
#kate will look up this code

#
lizards$yhat = predict(mod3)

#make a new dataframe
new.dat.combos <- with(lizards, #use lizards
                       expand.grid(time=unique(time), #give me all the unique values of time
                                   opalinus=seq(min(opalinus),max(opalinus), by=1)
                                   #give me a sequence of numbers from the min of opalinus to the max of opalinus, space the numbers "by 1"
                       ))

#predict number of grahami using new data frame
new.dat.combos$grahami <- predict(mod3,newdata=new.dat.combos)


###plotting prediction + data with continuous example#
ggplot(data=new.dat.combos,aes(x=opalinus,y=grahami,colour=time))+ #set up plot using predictions dataset
  geom_line(aes(group=time))+ #draw lines that are predictions, group them by light conditions
  geom_point(data=lizards, aes(x=opalinus,y=grahami,colour = time)) #add the observed data to the plot
#note - I am calling the original dataframe - lizards
#i am therefore setting up aes again

##if you have two continuous variables, you will prob want to bin to ease visualization##
bats = read.csv("bat_data.csv")
head(bats)
bats$lgdL = log10(bats$gdL)

lmBAT <- lm(lgdL~count*temp, data= bats)
summary(lmBAT)
allEffects(lmBAT)
plot(allEffects(lmBAT))
#this knows to bin the temperature data

new.bat.combos = with(bats,
                      expand.grid(
                        count = seq(min(count, na.rm=T), max(count, na.rm=T), by=1),
                        temp = c(2, 6, 10) #just pick 3 temps to demonstrate effect
                      ))


#predict lgdL using new data frame - preserve original predictor structure (e.g. use temp, not temp.bin)
new.bat.combos$lgdL <- predict(lmBAT,newdata=new.bat.combos)


###plotting prediction + data with two continuous variables, binning one#
ggplot(new.bat.combos,aes(x=count,y=lgdL,colour=temp))+ #set up plot using predictions dataset
  geom_line(aes(group=temp))+ #draw lines that are predictions, group them by light conditions
  geom_point(data=bats, aes(x=count,y=lgdL,colour = temp)) #add the observed data to the plot
