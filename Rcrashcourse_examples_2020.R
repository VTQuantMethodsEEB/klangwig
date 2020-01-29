
#read in my data

batdat = read.csv("bat_data.csv")
head(batdat)

# load and install packages
#install.packages("ggplot2")
library(ggplot2)

#create a vector
species = batdat$species
species = c("mylu","mylu","myse")

# get help
?"unique"

# examine my data
unique(batdat$species)
str(batdat)
head(batdat)
tail(batdat)
dim(batdat)
#rows columns
names(batdat)
nrow(batdat)
ncol(batdat)

#do a calculation with my data
batdat$nlog.loads = log(batdat$gdL)
head(batdat)
batdat$log10.loads = log10(batdat$gdL)

#subset data
#using subset command
MYSE.dat = subset(batdat, species=="MYSE")
dim(MYSE.dat)

##using warm temperatures
warm.temps = subset(batdat, temp>6)
dim(warm.temps)
warm.temps

#same thing using square brackets
MYSE.dat = batdat[batdat$species=="MYSE",]

#last way - tidyverse filter
library(tidyverse)
MYSE.dat = batdat %>%
  filter(species=="MYSE") %>%
  mutate(mean.load = mean(log10.loads, na.rm=T))
dim(MYSE.dat)
head(MYSE.dat)

#aggregate vs tidyverse
bat.summary = aggregate(log10.loads~species, FUN=mean, data = batdat)
bat.summary

#tidyverse
bat.summary = batdat %>%
  group_by(species) %>%
  summarise(mean.loads = mean(log10.loads, na.rm=T))
bat.summary

#make histogram of a column
hist(batdat$log10.loads)

# write out dataframe
write.csv(bat.summary, "bat.summary.csv", row.names = F)
