#survey analysis
library(tidyverse)

#sur = read.csv("survey.csv")
sur = read.csv("survey_results.csv")
head(sur)
ncol(sur)
sur = sur[-1]

sur2 = sur %>%
gather(key="key", value = "value")#, -Username

head(sur2)
#sur2 = subset(sur2,Username!="sbutton@vt.edu")
sur = sur2

#boxplot
r=ggplot(data=sur, aes(x=key,y=value))+ 
  geom_violin()+
  geom_point()+#aes(color=Username)
  ylab("Score")+
  xlab("")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=10,angle = 45),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)


tab = aggregate(value~key,FUN=mean,data=sur)
tab2 = aggregate(value~key,FUN=sd,data=sur)
tab$se = tab2$value / sqrt(10)
head(tab)
tab = tab[order(tab$value),]
tab

#point w/ se
r=ggplot(data=tab, aes(x=key,y=value))+ 
  geom_point()+
  geom_errorbar(aes(ymin = value - se, ymax = value + se))+
  ylab("Score")+
  xlab("")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=10,angle = 45),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)

#formal analyses
l1 = lm(value~key,data = sur)
summary(l1)
library(lsmeans)
lsmeans(l1, specs = "key", contr = "pairwise")

