#ex_nls_models


##nls models - Michaelis-Menten
pred.dens = seq(1,10, length.out=10) #create prey density
rates = c(.1,0.4,.5,.6,.5,.7,.8,.82,.825,.8257) #make up some capture rates
pred.dat = data.frame(pred.dens,rates) 

a = 1
b = 3
preds = (a*(pred.dens))/((b)+(pred.dens))

pred.mod<-nls(rates~((a*(pred.dens))/((b)+(pred.dens))),
              data=pred.dat, 
              start=list(a=1.36,b=5.87), #need to supply starting values
              trace = TRUE)

summary(pred.mod)

new.preds = seq(1,10,by=.1)
ftd=predict(pred.mod,newdata = list(pred.dens = new.preds)) #note need to include list!
plot(rates~pred.dens, col="red") #actual data
lines(new.preds,ftd) #fitted data

hist(resid(pred.mod))
shapiro.test(resid(pred.mod))


##nls model - monomolecular models##

dose=c(0.009, 0.09, 0.9, 9, 90, 900, 9000, 90000)
pos=c(0,0,1,8,8,7,5,3)
tot=c(7,7,7,11,9,8,7,3)

pi_o=pos/tot
data=data.frame(dose,pos,tot,pi_o)

dose.nls<-nls(pi_o~(1-exp(-(theta*(dose)))),
              data=data, 
              start=list(theta=0.05), 
              trace = TRUE)

summary(dose.nls)


ftd=fitted(dose.nls)
plot(ftd~log10(dose), pch=15) #fitted curve
points(pi_o~log10(dose),col="red") #actual points

dose.curve=ggplot(data=data, aes(x=log10(dose),y=pi_o))+ 
  geom_point(color="blue")+
  geom_line(aes(y=ftd), size=1)+
  xlab(expression(log[10]~dose))+
  ylab("Proportion infected")+
  coord_cartesian(ylim=c(-0.1,1.1))+ #zoom in
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(dose.curve)


##calculate mean-squared error
resid.sq = (resid(dose.nls)^2)
mse = mean(resid.sq)
print(mse)

##GAM##
pred.dens = seq(1,10, length.out=10) #create prey density
rates = c(.1,0.4,.5,.6,.5,.7,.8,.82,.825,.8257) #make up some capture rates
pred.dat = data.frame(pred.dens,rates) 

library(mgcv)
a <- gam(rates~s(pred.dens),data=pred.dat, method="REML")
summary(a)
#Using summary with the model object will give you the significance of the smooth term
#and the variance explained.
#edf - estimated degrees of freedom, larger means more wiggle, values closer to 1 are closer to linear
#gam.check(a)
plot(a,pages=1,seWithMean=TRUE) #
pred <- predict.gam(a)
pred.dat$yhat = pred

library(ggplot2)
r=ggplot(data=pred.dat, aes(x=pred.dens,y=rates))+ 
  geom_point()+
  geom_line(aes(y=yhat), size=1)+
  ylab("Rate of prey capture")+
  xlab("Predator density")+
  #coord_cartesian(ylim=c(-0.1,1.1))+ #zoom in
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)

