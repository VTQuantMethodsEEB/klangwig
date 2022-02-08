#clear workspace
rm(list=ls()) # clears workspace

#read in file
batdat = read.csv("bat_data.csv")

#load packages
library(tidyverse)

#get help
?unique

#examine data
head(batdat) #first 6 rows
tail(batdat) #last 6 rows
View(batdat) #show an excel like version of your data
names(batdat) #names of the columns of your data

str(batdat) #structure of each object in your dataframe
dim(batdat) #dimensions of your dataframe
nrow(batdat) #number of rows in dataframe
ncol(batdat) #number of columns in dataframe

#look at a specific column
unique(batdat$species) #give me all the unique values in this column
unique(batdat$site) 
unique(batdat$state)

#calculation with data
log10(batdat$gdL) #take the log of this column

#make a new column
batdat$log.loads = log10(batdat$gdL) #make a new column that is the log of this column

#subset data
#using subset
MYSE.dat = subset(batdat, species=="MYSE") #a factor/character, so need == and quotes
dim(MYSE.dat) #what are the dimensions of the new data frame?

warm.temps = subset(batdat, temp>6) #a number, so no quotes
dim(warm.temps)

#same thing using square brackets (say "where")
MYSE.dat = batdat[batdat$species=="MYSE",]
dim(MYSE.dat)

warm.temps = batdat[batdat$temp>6,]

#use aggregate to summarize data
bat.summary = aggregate(log.loads~species, FUN=mean, data = batdat) #aggregate data using the mean to give fungal loads by species
bat.summary

bat.summary2 = aggregate(log.loads~species+site, FUN=sum, data = batdat) =
bat.summary2


#make a histogram of your data
hist(batdat$log.loads)
hist(batdat$temp)

#write out summary file, for example

write.csv(bat.summary, "bat.summary.csv",row.names = F)
