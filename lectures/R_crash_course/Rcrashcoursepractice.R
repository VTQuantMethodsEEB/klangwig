#clear workspace
rm(list=ls())

#read in your data
batdat = read.csv("bat_data.csv")

#load and install packages
#install.packages("ggplot2")

library(ggplot2)

#getting help
?"unique"
unique(batdat$date)

#examine yourdata
unique(batdat$species)
str(batdat)
head(batdat)
tail(batdat)
dim(batdat)
names(batdat)
nrow(batdat)
ncol(batdat)

#do a calculation 
log(batdat$gdL)

#add the log values as a column
batdat$log.loads = log10(batdat$gdL)

batdat$log.loads
names(batdat)

#subset data
MYSE.dat = subset(batdat, species=="MYSE")
dim(MYSE.dat)

warm.temp = subset(batdat, temp>6)
dim(warm.temp)

#using square brackets
MYSE.dat = batdat[batdat$species=="MYSE",]
dim(MYSE.dat)

#use aggregate to summarise data
bat.summary = aggregate(log.loads~species, FUN = mean, data = batdat)
bat.summary

#make a histogram
hist(batdat$log.loads)

#write out a data file
write.csv(bat.summary, "bat.summary.csv", row.names = F)

