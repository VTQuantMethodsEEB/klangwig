rm(list=ls()) # clears workspace

#play data
#example with directory

#example of read.csv with directory
dm=read.csv("/Users/klangwig/Desktop/VT/teaching/quant grad course/github/klangwig/bat_data.csv")
#this file is in my project folder so I can read it in without calling the directory
dm=read.csv("bat_data.csv")
#you will need to change this to match your directory


#view the data
View(dm)

#look at the first 5 rows
head(dm)

#explore the structure of different colunmns
str(dm$species) #character
str(dm$gdL) #number
str(dm$date) #character! will need to change to a date to work with!

#dataframe dimensions
dim(dm)
#670 rows, 11 columns

###selecting things###
#use square brackets
dm[dm$site=="HORSESHOE BAY",]

#look at the column in View
View(dm[dm$site=="HORSESHOE BAY",])

#this gives you all the values where species is MYSE and the site is Horseshoe Bay
dm[dm$species=="MYSE"&dm$site=="HORSESHOE BAY",]
#there are only 4 observations of MYSE at Horshoeshoe Bay

#compare this with or which is the | symbol (that a straight line, not an I)
dm[dm$species=="MYSE" | dm$site=="HORSESHOE BAY",]
#there are many more observations including all the species

#summary
summary(dm)

#add a column
dm$log.loads = log10(dm$gdL)

#make sure the column has added
head(dm)
#why are there NAs? Because you can't log a negative value!

#always check the dimensions when adding a column
dim(dm)
#yes - now there are 12 columns

#sometimes you might want to remove all the NAs from you dataframe

#install package example - can also use console tab at right - 'Packages'
#install.packages("tidyverse")

#need to load the package after installing
library(tidyverse)

#make a new dataframe with NAs removed from the loads column
dm.new = dm %>%
  #here I am using tidyverse to do this
  #this symbol is called a pipe
  #we will talk more about this next week
  drop_na(log.loads)

#aggregate
f1 = aggregate(log.loads~site+species, FUN = mean, data = dm.new)
print(f1)
