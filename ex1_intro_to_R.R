#some changes...

##putting package officer through some tests
rm(list=ls()) # clears workspace

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

#Here, I am making a report for an agency or IACUC.
#We often have to report how many individuals we sampled. Here is a nifty way to do that without copying and pasting and formatting. 

#play data
dm<-read.csv("/Users/klangwig/Dropbox/teaching/quant grad course/lectures/examples/bat_data.csv")
#this file is in the datasets folder on canvas

#change date format
dm$date=as.Date(dm$date, "%m/%d/%y")
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

#although long format is great for data manipulation, it's not always awesome for reports.
#Here is how you would do a wide table. 
#I want a table with a column for each species, site, and date, and rows for the number of individuals sampled.
dm$site_date=paste(dm$site,dm$date,sep="_")
#tables are a bit annoying to work with, so I use trick that concatenates site and date (by paste) so I can aggregate on a single variable
#i'm using the _ (underscore) to separate because number of site names do have periods, spaces, and the dates have dashes
tab1=table(dm$site_date,dm$species); tab1

#tables aren't an awesome format, so turn them into a data frame
mat1=as.data.frame.matrix(tab1) ; mat1
#create a new column for date
mat1$date=NA 

#force tables into dataframes for manipulation
mydf <- cbind(rownames(mat1), mat1)
#this says bring the first column and the rest of the table together
rownames(mydf) <- NULL
#we don't need the row names in a data frame so get rid of them
colnames(mydf)
colnames(mydf) <- c("SITE","EPFU","MYLU","PESU","SUBSTRATE","DATE")
mydf

#we don't want the dates as part of the site name, so lets break them out
library(reshape2)
x=colsplit(mydf$SITE,"_",c("sitename","date"));x 
#this splits site and date into multiple columns, splitting on the period.
mydf$DATE=x$date
mydf$SITE=x$sitename
mydf

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
  print(target = "/Users/klangwig/Dropbox/teaching/quant grad course/lectures/examples/WI_sampling_report.docx") #output the file

