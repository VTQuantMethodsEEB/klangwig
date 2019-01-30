#le2_data_manipulation  

#load important packages##
library(tidyr)
library(dplyr)

#using the bat data - read
batdat=read.csv("bat_data.csv") # you will need to have the bat data file in the directory you are using

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

## Group by, Mutate, and Summarise
batdat %>% 
  group_by(species) %>% 
  summarise(mean.fungal.loads=mean(lgdL,na.rm=TRUE))
#this gives you a summary table, it doesn't change batdat

#if you want to call this table something you would need to assign it
fungal.load.table = batdat %>% 
  group_by(species) %>% 
  summarise(mean.fungal.loads=mean(lgdL,na.rm=TRUE))

fungal.load.table

##Summarise versus Mutate
batdat_with_sample_size = batdat %>% 
  #create a new dataframe  called batdat_with_sample_size
  group_by(site,species,date) %>% 
  #you can group_by multiple things
  mutate(sample.size=length(swab_id))
#this adds a column to the dataframe
batdat_with_sample_size

##we could have also just added this column to batdat
batdat = batdat %>% 
  #create a new dataframe  called batdat_with_sample_size
  group_by(site,species,date) %>% 
  #you can group_by multiple things
  mutate(sample.size=length(swab_id))
#this adds a column to the dataframe
batdat
