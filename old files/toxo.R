
toxo = read.csv("toxo.csv")
head(toxo)

r=ggplot(data=toxo, aes(x=GDP, y=prevalence))+ 
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)
