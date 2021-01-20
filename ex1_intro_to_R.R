
rm(list=ls()) # clears workspace


#load packages
library(tidyverse)
library(reshape2)


#simplifying code with pipes
#http://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html
#introduce this idea on Thursday

#piping example
library(magrittr)
#iris is a built-in package
View(iris)
#do you see what they've done with species here? grrr.
iris %>%
  subset(Sepal.Length > 5) %>%
  aggregate(. ~ Species, ., FUN=mean)

#this is the same as
iris.sub=subset(iris,Sepal.Length >5)
aggregate(.~Species,FUN=mean, data=iris.sub)

b1<-aggregate(Sepal.Length~Species,FUN=mean, data=iris.sub)


#Here, I am making a report for an agency or IACUC.
#We often have to report how many individuals we sampled. Here is a nifty way to do that without copying and pasting and formatting. 

#play data
#example with directory
dm<-read.csv("/Users/klangwig/Dropbox/teaching/quant grad course/lectures/examples/bat_data.csv")
#this file is in my project folder so I can read it in without calling the directory
dm<-read.csv("bat_data.csv")

#this file is in the datasets folder on canvas

###selecting things###
head(dm)
dm[dm$site=="HORSESHOE BAY",]
View(dm[dm$site=="HORSESHOE BAY",])

#change date format
unique(dm$date)
#what does this look like? 
#If the year is two-digits 'y' should be lowercase
#If the year is four-digits 'Y' should be uppercase
dm$date=as.Date(dm$date, "%m/%d/%y") 
#if you have a pc, this line may be:
#dm$date=as.Date(dm$date, "%m/%d/%Y") 

dm=subset(dm, date > as.Date("2016-04-01") )
#use only data from dates past april 1 2016

##prep for dm
names(dm)
dm$species=as.factor(toupper(dm$species))#make all species names uppercase (in case of errors)
names(dm)
dm$tally=1
#this is a trick to "count" the number individuals sampled by summing with aggregate
#this creates a long format dataframe.
f1=aggregate(tally~site+date+species,FUN=sum,data=dm)
#long
f1=f1[order(f1$site,f1$date),];f1 
head(f1)

#More advanced R users....
#although long format is great for data manipulation, it's not always awesome for reports.
#Here is how you would do a wide table using tidyverse. 
#We will go through tidyverse in more detail next week, but here is a quick intro

v1 = dm %>% #this says take the dm and make a new dataframe called v1
  group_by(site,date,species)%>% #group by site, date, and species
  summarise(N = n()) #make a new dataset that counts the number of observations in each group

v1 #this should look identical to the table you made with aggregate

mydf = v1 %>%
  pivot_wider(names_from = species, values_from = N,values_fill = 0) 
#pivot_wider is a tidyverse function that changes data from long to wide format

head(mydf)


##MAKE THE TABLE PRETTY
#make the flex table
library(flextable)
#https://cran.r-project.org/web/packages/flextable/vignettes/overview.html
ft <- flextable(data = mydf) 
ft <- theme_booktabs(ft) 
ft<-  autofit(ft) 
ft

##MAKE REALLY PRETTY REPORT
#https://davidgohel.github.io/officer/articles/word.html
library(officer)
library(flextable)
library(magrittr)

#magrittr uses piping %>% to simplifly code
#the officer package has whole-hog embraced piping - remember to say the word "then"
#http://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html
#https://www.datacamp.com/community/tutorials/pipe-r-tutorial

my_doc <- read_docx() %>% #create a word document
  body_add_par(value = "Wisconsin Sampling Report", style = "heading 1") %>% 
  body_add_par("Table 1. Summary of samples collected in Wisconsin by species.", style = "Normal") %>% #add a caption to your table
  body_add_flextable(ft)%>% #add your flex table (ft) to your report
  print(target = "WI_sampling_report.docx") #output the file

