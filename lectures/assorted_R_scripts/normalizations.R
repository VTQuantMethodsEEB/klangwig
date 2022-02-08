
#install
install.packages("bestNormalize")
library(bestNormalize)
install.packages("VGAM")
library(VGAM)

#sample data
y = rgamma(100,shape=1,rate=1/2)
hist(y)
bestNormalize(y)
yeojohnson(y)#bestNormalize package
y = yeo.johnson(y, lambda = -.41) #VGAM package
hist(y)
