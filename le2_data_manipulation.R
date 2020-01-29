#le2_data_manipulation  

#load important packages##
library(tidyverse)
library(tidyr)
library(dplyr)

#using the bat data - read
batdat=read.csv("bat_data.csv") # you will need to have the bat data file in the directory you are using

head(batdat)  
unique(batdat$species)
hist(batdat$gd)

batdat$lgdL=log10(batdat$gdL)#log the amount of fungus
batcounts<-aggregate(count~species+site+date,data=batdat, FUN=mean)  #make a df of bat counts

#starting with the "old" way - using spread
batcounts.wide<-spread(batcounts, species,count,convert=T) #spread that dataframe

head(batcounts)
head(batcounts.wide)

batloads<-aggregate(lgdL~species+site+date,data=batdat, FUN=mean)
head(batloads)

batloads.wide<-spread(batloads, species,lgdL,convert=T)
head(batloads.wide)

#can merge together but duplicates columns
batwide=merge(batloads.wide,batcounts.wide,by=c("site","date"))
head(batwide)
#now we have a dataframe with columns for counts and for loads

# Or "match" and keep in long format
batloads
batcounts
#create a unique row id - in this case it is the species, site, and date which denote the entries we want to match
batloads$unique.row.id = paste(batloads$species,batloads$site,batloads$date)
batcounts$unique.row.id = paste(batcounts$species,batcounts$site,batcounts$date)
#dataframe you are bringing to first, and the one you matching from second
batloads$count = batcounts$count[match(batloads$unique.row.id,batcounts$unique.row.id)]

# Look at some example data that comes with the tidyr package
# "New' way using 'pivot_wider'
fish_encounters

#let's say we want a new df where each row is a station
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

#we didn't include 0's at statins where we didn't observe fish so we can substitute those in
# Fill in missing values
fish_encounters %>%
  pivot_wider(
    names_from = station,
    values_from = seen,
    values_fill = list(seen = 0)
  )

#what if we have a wide dataframe and want to make it "tidy"
head(relig_income)
relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "count")
#the minus sign says don't include the column religion
#now you have a variable income and a count of the number of people that were in that income class

head(billboard)

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )

billboard %>% 
  pivot_longer (
    cols = starts_with("wk"), 
    names_to = "week", 
    names_prefix = "wk",
    names_ptypes = list(week = integer()),
    values_to = "rank",
    values_drop_na = TRUE
  )
#we can also smush our bat data back together
head(batcounts.wide)

#we want both site and date to be columns
batcounts.wide %>%
  pivot_longer(-c(site,date), names_to = "species", values_to = "count")


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
  mutate(sample.size=n())
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
