#survey analysis

sur = read.csv("survey.csv")
head(sur)

#boxplot
r=ggplot(data=sur, aes(x=topic,y=score))+ 
  geom_violin()+
  geom_point()+
  ylab("Score")+
  xlab("")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=10,angle = 45),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)


tab = aggregate(score~topic,FUN=mean,data=sur)
tab2 = aggregate(score~topic,FUN=sd,data=sur)
tab$se = tab2$score / sqrt(14)
head(tab)
tab = tab[order(tab$score),]

#point w/ se
r=ggplot(data=tab, aes(x=topic,y=score))+ 
  geom_point()+
  geom_errorbar(aes(ymin = score - se, ymax = score + se))+
  ylab("Score")+
  xlab("")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=10,angle = 45),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)
