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

##calculate mean-squared error
resid.sq = (resid(dose.nls)^2)
mse = mean(resid.sq)
print(mse)
