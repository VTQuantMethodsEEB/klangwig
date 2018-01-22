#le2_data_manipulation  

#load important packages##
library(tidyr)
library(dplyr)

#using the bat data - read
batdat=read.csv("bat_data.csv") # you will need to have the bat data file in the directory you are using
#note, I've updated this since the previous lecture

head(batdat)  
unique(batdat$species)
hist(batdat$gd)

batdat$lgdL=log10(batdat$gdL)#log the amount of fungus
batcounts<-aggregate(count~species+site+date,data=batdat, FUN=mean)  #make a df of bat counts
batcounts.wide<-spread(batcounts, species,count,convert=T) #spread that dataframe

head(batcounts)
head(batcounts.wide)

batloads<-aggregate(lgdL~species+site+date,data=batdat, FUN=mean)
batloads.wide<-spread(batloads, species,lgdL,convert=T)
head(batloads)
head(batloads.wide)

batwide=merge(batloads.wide,batcounts.wide,by=c("site","date"))
head(batwide)

# Look at some example data that comes with the tidyr package:

smiths
gather(smiths)
#collect the vars we want
print(smelt <- gather(smiths, key="var", value="value",
                      c(age,weight)))
#collect the vars we want by telling what we DO NOT want
#minus sign
gather(smiths, key="var", value="value",
       -c(subject,time))

## Make a column for each subject (= a row for each measurement)
spread(smelt, key=subject, value)

## Make a column for each value (= a row for each person):
spread(smelt, key=var, value)

## Take the mean for each variable:
smelt %>% group_by(var) %>% summarise(mean=mean(value, na.rm=T))

## Report how many values are in each mean:
smelt %>% group_by(var) %>% 
  summarise(mean=mean(value,na.rm=TRUE),
            n=length(na.omit(value)))

