
#zero-inflated (mixture) model
g.zinf <- zeroinfl(opalinus ~ time, 
                   dist = "negbin",
                   data = liz)
summary(g.zinf)
#zero-inflated (mixture) model
h.zinf <- zeroinfl(opalinus ~ time, 
                   dist = "poisson",
                   data = liz)
summary(h.zinf)

vuong(h.zinf, g.zinf)
AIC(h.zinf, g.zinf)
lrtest(h.zinf, g.zinf)

liz$yhat = predict(g.zinf)

#let's plot the prediction
r=ggplot(data=liz, aes(x=time,y=yhat))+ 
  geom_point(color="red", shape = "-",size=22)  +
  geom_point(data = liz, aes(x = time, y = opalinus),size=3, shape = 1)+
  ylab("Lizard")+
  xlab("Time")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)
