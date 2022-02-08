


rm(list = ls())   

#batdat = read.csv("Users/klangwig/Dropbox/teaching/data/bat_data.csv")
batdat = read.csv("bat_data.csv")

#load package
library(tidyverse)

?unique

#examine your data
head(batdat)
tail(batdat)
#View(batdat)
names(batdat)
str(batdat)

log10(batdat$gdL)

batdat$log.loads = log10(batdat$gdL)

#subset
MYSE.dat = subset(x = batdat, subset=species=="MYSE") 
dim(MYSE.dat)
dim(batdat)
nrow(MYSE.dat)
ncol(MYSE.dat)
nrow(batdat)
length(unique(MYSE.dat$swab_id))

table(batdat$species)

MYSE.dat = batdat[batdat$species=="MYSE",]
dim(MYSE.dat)

warm.temps = batdat[batdat$temp>6,]

bat.summary = aggregate(log.loads~species, FUN = mean, data = batdat)
bat.summary
aggregate(batdat$log.loads~batdat$species, FUN = mean)


bat.summary2 = aggregate(log.loads~species+site, FUN = mean, data = batdat)
bat.summary2 = aggregate(log.loads~species+site, FUN = sum, data = batdat)

hist(batdat$log.loads)

write.csv(bat.summary,"mean.bat.loads.csv", row.names = F)
