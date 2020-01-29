
##vitamins
height=sample(seq(-.01,0.02,by=0.001),15, replace=T)
iron=sample(seq(0,0.01,by=0.0001),50, replace=T)

#test height difference from 0
heightt=t.test(height)
#test iron difference from 0
iront=t.test(iron)

#look at t-tests
heightt
iront

iront$conf.int[1]
iront$conf.int[2]

heightcis=c(heightt$conf.int[1],heightt$conf.int[2])

dat=data.frame(lab=c(rep("height",length(height)),rep("iron",length(iron))),var=c(height,iron))
dat$lcl[dat$lab=="height"]=heightt$conf.int[1]
dat$ucl[dat$lab=="height"]=heightt$conf.int[2]
dat$lcl[dat$lab=="iron"]=iront$conf.int[1]
dat$ucl[dat$lab=="iron"]=iront$conf.int[2]

library(ggplot2)
ggplot(data=dat,aes(x=lab,y=var))+
  #geom_point(size=2,shape=1,stroke=1)+
  stat_summary(fun.y = "mean",geom="point",color="red",size=3)+
  ylab("Proportional Increase")+
  xlab("")+
  geom_hline(yintercept=0,color="red")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(hjust = 1,size=25),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))

t.test(var~lab, data = dat)


##heights

heightA=sample(seq(92,105,by=0.1),25, replace=T)
mean(heightA)
heightB=sample(seq(85,105,by=0.1),25, replace=T)
mean(heightB)
dat=data.frame(lab=c(rep("heightA",length(heightA)),rep("heightB",length(heightB))),var=c(heightA,heightB))

ggplot(data=dat,aes(x=lab,y=var,color=lab))+
  geom_point(size=2,shape=1,stroke=1)+
  stat_summary(fun.y = "mean",geom="point",color="black",size=3)+
  ylab("Height")+
  xlab("")+
  #geom_hline(yintercept=0,color="red")+
  #geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(hjust = 1,size=25),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))


head(dat)
dat$random=sample(dat$lab)

ggplot(data=dat,aes(x=random,y=var,color=lab))+
  geom_point(size=2,shape=1,stroke=1)+
  stat_summary(fun.y = "mean",geom="point",color="black",size=3)+
  ylab("Height")+
  xlab("")+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(hjust = 1,size=25),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))

heightA;heightB
diff=NA;diff_all=NA 

for (i in 1:1000){
  dat$random=sample(dat$lab)
  diff=mean(dat$var[dat$random=="heightA"])-mean(dat$var[dat$random=="heightB"])
  diff_all=c(diff_all,diff)
}

diff_all=sort(diff_all)

length(diff_all)
diff_all[diff_all>=3.67]
3/1000
