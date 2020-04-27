#ex_time_series_models
########TIME-SERIES MODELS########
mos = read.csv("mosqtemp.csv")
head(mos)

#plot all the data overtime
with(mos,matplot(x=Year,y=cbind(SppRichness,DDT,Urban,Temp/2,Precip),ylim=c(0,8),type=c(rep("b",2),rep("l",3)),lty=c(rep(1),5),lwd=c(rep(1,5)),pch=c(rep(19,2),rep(0,3)),col=c(1,13,3,2,4),
                 ylab="# Species, T (C),DDT, Precip,Urbanization")) #plot data
legend(x=1979,y=8.2,c("Mosq. Species", "DDT","Urbanization","Temperature","Precip"),pch=c(rep(19,2),32,32,32),lty=1, lwd=c(rep(1,5)),col=c(1,13,3,2,4),bty="n")

plot(SppRichness~Temp,data=mos,ylab="#  species",xlab="Temperature (C)",subset=(Year>1959),col=2) #plot mosq vs temp for last 50 years of data
points(SppRichness~Temp,data=mos,subset=(Year<1960),col=1)#add older data in black

#but remember
plot(Temp~Year,data=mos,ylab="Temp",xlab="Year",col=2) 

f1 = lm(SppRichness ~ Temp, data = mos)
summary(f1)

#autocorrelation plot
acf(resid(f1))
#this says - what is the correlation b/t 1 v2, 1 v 3, 1 v4, etc. 
#we worry about things above the line

#which of the residuals pairs are significantly higher?
pacf(resid(f1))
#this says what is the correlation b/t 1 v 2, 1 v3 (accounting for correlation between 1 & 2), and so on
#we worry about POSITIVE lines that cross the blue

#which of the models are best?
#we can use an AR model

#the syntax of gls is like glm
f1a = gls(SppRichness ~ Temp, data = mos, correlation = corAR1())
summary(f1a)
f1b = gls(SppRichness ~ Temp, data = mos, correlation = corARMA(p=1))
summary(f1b)
#these give identical outputs because we haven't specified q, the moving average part of the model

#but we actually want AR2, because that is when the lag disappear
f2=gls(SppRichness~Temp,data=mos,correlation=corARMA(p=2))
summary(f2)#fit model with AR(2) correlation structure
#at the top it will give us the parameter estimates of the correlation matric structure
pacf(resid(f2, type ="normalized")) #plot of residuals - need "normalized" to show resids that incorporate correlation structure

#an important thing to remember is that we are looking at the correlation of the residuals
#sometimes we can include other variables that will make this correlation disappear
f3 <- lm(SppRichness~Temp+Urban+Precip+DDT,data=mos);summary(f3)# add other variables
pacf(resid(f3))
#with all the other variables, temp is no longer significant, and the autocorrelation isn't bad
f4 <- lm(SppRichness~Urban+Precip+DDT,data=mos);summary(f4)#dropped temp
pacf(resid(f4)) #examine residuals - no longer significant correlation
f4b <- gls(SppRichness~Urban+Precip+DDT,data=mos,correlation=corARMA(p=1));summary(f4b) #fit gls to same model - results nearly identical and AR(1) term very small
pacf(resid(f4b, type ="normalized")) #resids also show no correlation

