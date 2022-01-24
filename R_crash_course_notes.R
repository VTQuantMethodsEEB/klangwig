#R crash course code

rm(list=ls()) # clears workspace
rm(list=ls())

batdat = read.csv("bat_data.csv")
#/Users/klangwig/Desktop/VT/teaching/quant grad course/github/klangwig

batdat = read.csv("/Users/klangwig/Desktop/VT/teaching/quant grad course/github/klangwig/bat_data.csv")

#load install and packages
#install.packages("ggplot2")

#load package
library(ggplot2)

#get help
?"unique"

#examine your data
unique(batdat$species)
str(batdat)
str(batdat$count)
head(batdat)
tail(batdat)
nrow(batdat)
ncol(batdat)
dim(batdat)

#do a calculation with your data
batdat$lgdL=log(batdat$gdL)
batdat$lgdL=log10(batdat$gdL)
3+3
batdat$lgdL=log10(batdat$gdL)
#vector
#log.loads = log10(batdat$gdL)

#subset data
MYSE.dat = subset(batdat, species=="MYSE")
dim(MYSE.dat)

MYSE.dat = batdat[batdat$species=="MYSE",]
dim(MYSE.dat)

#numeric subset
warm.temps = subset(batdat, temp>6)
dim(warm.temps)

#using aggregate
#creating mean log loads of bats\
batdat$log.loads = log10(batdat$gdL)

bat.summary = aggregate(log.loads~species, FUN = mean, data=batdat)
#using aggregate or table to summarize
bat.summary
print(bat.summary)

hist(batdat$log.loads)

#write out dataframe
write.csv(bat.summary, "bat.summary.csv", row.names = FALSE)
write.csv(bat.summary, "/Users/klangwig/Desktop/VT/teaching/quant grad course/github/klangwig/kate.super.bat.data.csv")


head(batdat)

library(tidyverse)
batdat<-batdat %>%
  rowwise() %>%
  mutate(weird.thing=max(count,lgdL))

batdat$mean.weird.thing = colMeans(batdat$count, batdat$lgdL)
batdat$mean.weird.thing = rowMeans(batdat[,c(batdat$count, batdat$lgdL)], na.rm=TRUE)
