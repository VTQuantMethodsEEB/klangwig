rm(list=ls()) # clears workspace

#le3_data_visualization

#load important packages##
library(ggplot2)
library(gridExtra)
library(viridis)



batdat=read.csv("/Users/klangwig/Dropbox/teaching/quant grad course/github/klangwig/bat_data.csv") 
#or
batdat=read.csv("bat_data.csv") 
head(batdat)
batdat$lgdL=log10(batdat$gdL)


g1=ggplot(data=batdat,aes(x=species,y=lgdL,color=site))+
  geom_point(size=2) #this adds points to graph
g1
#a simple graph with species on x, and lgdL (loads) on Y, site is color

#this is a little too simple, but ggplot2 allows us to buid

g1=ggplot(data=batdat,aes(x=species,y=lgdL,color=site))+
  geom_boxplot() #this add a boxplot to the graph
g1
#just showing the box obscures the data - lets get rid of site, and add the points back in

g1=ggplot(data=batdat,aes(x=species,y=lgdL))+
  geom_boxplot()
g1
#this is okay, but how many samples come from these?

g1=ggplot(data=batdat,aes(x=species,y=lgdL))+
  geom_boxplot()+
  geom_point(aes(color=site)) #now assign color here so we don't have seperate boxes
g1
#now we can look at it in more compact form, but still see the data come from multiple sites


#we only need "species" because we are using the 4 letter codes, but these are unnecessary
batdat$species=as.character(batdat$species)
batdat$nspecies[batdat$species=="EPFU"]="E. fuscus"
batdat$nspecies[batdat$species=="MYLU"]="M. lucifugus"
batdat$nspecies[batdat$species=="MYSE"]="M. sentrionalis"
batdat$nspecies[batdat$species=="PESU"]="P. subflavus"
batdat$nspecies[batdat$species=="SUBSTRATE"]="Substrate"

g1=ggplot(data=batdat,aes(x=nspecies,y=lgdL))+
  geom_boxplot()+
  geom_point(aes(color=site))+
  ylab(expression(log[10]~fungal~loads))+ #change y axes (use an expression to get a log value)
  xlab("Species") #change x axes
g1
#now the axes are better...

#now, let's improve look of the figure
g1=ggplot(data=batdat,aes(x=nspecies,y=lgdL))+
  geom_boxplot()+
  geom_point(aes(color=site))+
  ylab(expression(log[10]~fungal~loads))+
  xlab("")+
  #scale_colour_viridis(discrete = T)+ #this comes with the viridis package
  scale_colour_manual(values=c("#45B29D","#EFC94C","#E27A3F","#DF4949","blue"))+
  theme_bw()
g1
#this looks much better, but I'd like some additional options

g1=ggplot(data=batdat,aes(x=nspecies,y=lgdL))+
  geom_boxplot()+
  geom_point(aes(color=site),size=2,shape=1,stroke=1)+#shape specifies donut, and stroke changes thickness
  ylab(expression(log[10]~fungal~loads))+
  xlab("")+
  scale_colour_viridis(discrete = T)+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="top",
      legend.title = element_blank(),
      legend.text = element_text(size=20),
      legend.background = element_blank(),
      legend.key=element_rect(fill="white",color="white"))
g1
#save the file
ggsave(file="/Users/klangwig/Dropbox/teaching/quant grad course/lectures/examples/loads_by_spp.pdf", 
       plot=g1,
       width=7,height=7,units="in",
       useDingbats=FALSE) #use Dingbats is surprisingly useful because it prevents points from being turned in o's in some programs


#show the same data, but show multiple "smalls"
g1=ggplot(data=batdat,aes(x=nspecies,y=lgdL))+
  facet_wrap(~site,ncol=1,nrow=3)+ #this is creating multiple "panels" for site
  geom_boxplot()+
  geom_point(aes(color=site),size=2)+
  ylab(expression(log[10]~fungal~loads))+
  xlab("")+
  scale_colour_viridis(discrete = T)+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))
g1

#what data visualization principle are we violating?

####THAT IS CATEGORICAL, BUT WHAT ABOUT TWO CONTINUOUS VARIABLES?###

#let's set our theme to condense the code a bit...
theme_set(theme_bw()+
  theme(axis.title=element_text(size=23),
               axis.text=element_text(size=15),
               panel.grid = element_blank(), 
               axis.line=element_line(),
               axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
               legend.position="top",
               legend.title = element_blank(),
               legend.text = element_text(size=15),
               legend.background = element_blank(),
               legend.key=element_rect(fill="white",color="white")))

#note - we can get back to default by resetting to theme_gray

#continuous graph
g2=ggplot(data=batdat,aes(x=lgdL,y=temp,color=nspecies))+
  geom_point(size=2)
g2

#should we consider site effects?
g2=ggplot(data=batdat,aes(x=temp,y=lgdL,color=nspecies,shape=site))+
  geom_point(size=2)
g2


###LET'S look at counts over time
g3=ggplot(data=batdat,aes(x=date,y=count,color=nspecies,shape=site))+
  geom_point(size=2)+
  geom_line()
g3

#why aren't there lines? Ohhh..our old friend date!

batdat$date.new=as.Date(batdat$date, "%m/%d/%y")

g3=ggplot(data=batdat,aes(x=date.new,y=count,color=nspecies,shape=site))+
  geom_point(size=2)+
  geom_line()
g3
#now we have lines, but the date axis is ridiculous

#http://ggplot2.tidyverse.org/reference/scale_date.html

g3=ggplot(data=batdat,aes(x=date.new,y=count,color=nspecies,shape=site))+
    geom_point(size=2)+
    geom_line()+
    scale_x_date(date_breaks = "2 months", date_labels = "%m/%Y",limits = as.Date(c('2015-01-01','2016-05-01')) )
  g3
  