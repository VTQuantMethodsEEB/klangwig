##non-linear model example###

dose=c(0.009, 0.09, 0.9, 9, 90, 900, 9000, 90000)
pos=c(0,0,1,8,8,7,5,3)
tot=c(7,7,7,11,9,8,7,3)

pi_o=pos/tot
data=data.frame(dose,pos,tot,pi_o)

#simple exponential
time = c(1,3,5,10,12)
cohort.exposed = c(0.1,0.1,0.5,.8,1)


dis.nls<-nls(cohort.exposed~a*(exp((theta*(time)))),
              data=data, 
              start=list(a=1,theta=0.0001), 
              trace = TRUE)

summary(dis.nls)

new.time = seq(1,100,by=1)
ftd=predict(dis.nls,newdata = list(time = new.time))
plot(cohort.exposed~time, col="red") #actual data
lines(new.time,ftd) #fitted data


##monomolecular models

dose.nls<-nls(pi_o~(1-exp(-(theta*(dose)))),
              data=data, 
              start=list(theta=0.05), 
              trace = TRUE)

summary(dose.nls)

ftd=fitted(dose.nls)
plot(ftd~log10(dose), pch=15) #fitted curve
points(pi_o~log10(dose),col="red") #actual points


##nls models - S shaped
pred.dens = seq(1,10, length.out=10)
rates = c(.0,.02,.05,.3,.5,.7,.8,.82,.825,.8257)
pred.dat = data.frame(pred.dens,rates)

pred.mod<-nls(rates~((a*(pred.dens^2))/((b^2)+(pred.dens^2))),
              data=pred.dat, 
              start=list(a=0.05,b=1), 
              trace = TRUE)

summary(pred.mod)

new.preds = seq(1,10,by=.1)
ftd=predict(pred.mod,newdata = list(pred.dens = new.preds))
plot(rates~pred.dens, col="red") #actual data
lines(new.preds,ftd) #fitted data

##ricker function
pop.size <- seq(100, 1000, length.out = 100)
library(metafolio)
birth.rate <- ricker(pop.size, a = 1.9, b = 900)
birth.rate = birth.rate +runif(n= length(pop.size), -20,20)
plot(pop.size, birth.rate)
rick.dat = data.frame(pop.size,birth.rate)

ric.nls<-nls(birth.rate~a*pop.size*(exp(-b*(pop.size))),
             data=data, 
             start=list(a=1.9,b=900), 
             trace = TRUE)
