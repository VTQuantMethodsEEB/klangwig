rm(list=ls())

##Code corresponding to LE5 - Statistical Tests

##permutation tests####

##this data is originally from Gotelli and Ellison Primer of Ecology##

forest <- c(9, 6, 4, 6, 7, 10)
field  <- c(12, 9, 12, 10)
mean(field) - mean(forest)

ants <- data.frame(
  place=rep(c("field","forest"),
            c(length(field), length(forest))),
  colonies=c(field,forest)
)

View(ants)



library(ggplot2)
theme_set(theme_bw())

###Look at the data (with stat_sum() to visualize overlapping data points; 
#jittering is also a possibility, but stat_sum() is prettier).  
ggplot(ants,aes(x = place,y = colonies))+
  stat_sum(aes(size=..n..),colour="darkgray")+
  #aes(size=..n..) tells stat_sum() to use the number of overlapping points, 
  #not the proportion of points within a category, as the summary statistic
  scale_size_area(breaks=1:2,max_size=4)+
  #scale_size_area() tells ggplot to scale the area of the points proportional to the size 
  #(breaks=1:2 tells it what values to show in the legend). 
  geom_boxplot(fill=NA)

#with jitter
ggplot(ants,aes(place,colonies))+
  geom_point(aes(place,jitter(colonies,factor=1.5)),shape=2,size=3)+
#jitter uses factor to control the amount of the jitter, 
  #shape specifies triangles, and size is the size of the points
  geom_boxplot(fill=NA)

##how to write your own permutation test##
set.seed(101)
#set.seed will set R's random number generator to start at the same place
#this ensures that when you, and I, and anyone else, does the test, we will all get the same results

res <- NA ## set aside space for results

#you always need to do something like this when you run a "for" loop
#you could also write res <- numeric(1000), which would give you a list of 1000 0's
#the important thing to have a vector already named "res"
comb_ff = c(field,forest)
sample(comb_ff,5, replace=T)

for (i in 1:10000) {
  colonyboot <- sample(c(field,forest)) ## scramble
  ## pick out forest & field samples
  forestboot <- colonyboot[1:length(forest)] #this says assign the first six colonies to forest
  fieldboot <- colonyboot[(length(forest)+1):length(colonyboot)] #assign the rest of the colonies to field
  ## compute & store difference in means
  res[i] <- mean(fieldboot)-mean(forestboot) #calculate the difference in the field means and the forest means
  #[i] says "where i", and i is a counter, after running this loop, i should be 1000
}

#what is our observed mean difference?
obs <- mean(field)-mean(forest)
obs

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

##so how do we get our p-value?
res[res>=obs]
length(res[res>=obs])
230/10000
mean(res>=obs)        
#using mean(permutations>=obs)) is a trick to calculate the proportion: 
#the logical statement returns a logical (FALSE/TRUE) vector, which then gets converted to a 0/1 vector when you ask R to take the mean, 
#so this is equivalent to counting the number of true values and dividing by the length


#what if we want a two-tailed?

#we could double the p-value
2*mean(res>=obs)          ## doubling

#or we could count the area in both tails
mean(abs(res)>=abs(obs))

###########t-tests####################
#one-sample t-test - is the mean equal to 0?
tt_one <- t.test(field)
tt_one


#true student t-test
tt <- t.test(colonies~place,data=ants,var.equal=TRUE)
tt

#welch's t-test
tt <- t.test(colonies~place,data=ants)
tt

#a paired t-test doesn't make sense for this data because they aren't the same
#imagine we did an experiment where we cut down the trees in the forest, and then re-surveyed our plots
forest_pre=forest
forest_post=c(9+2,  6+1,  4+1,  6+1,  7+1, 10+1)

#try changing this - what happens?
#forest_post=c(9+2,  6+1,  4+1,  6+1,  7+1, 10+1)

ttp<- t.test(forest_pre,forest_post,paired=T)
ttp
##see how flexible t-test is? We didn't even need to create a dataframe!

forest_treat <- data.frame(
  trmt=rep(c("forest_pre","forest_post"),
            c(length(forest_pre), length(forest_post))),
  colonies=c(forest_pre,forest_post)
)
forest_treat

# we also could have written this
ttp<- t.test(colonies~trmt,data=forest_treat,paired=T)
ttp

#what if we didn't specify paired?
ttp<- t.test(colonies~trmt,data=forest_treat,var.equal=T)
ttp


#####Shapiro-Wilk Test#######
#Are our data normally distributed?##
#The null hypothesis is that the data are normally distributed
#P<0.05 indicates NOT normal

swt<-shapiro.test(ants$colonies)
swt

swt_field<-shapiro.test(field)
swt_field

swt_forest<-shapiro.test(forest)
swt_forest

#####Correlation Tests#####
#Pearsons - for linear data
#use cor for unpaired, and cor.test for paired
pt <- cor.test(forest_pre,forest_post)
pt

#below is incorrect - these samples are paired! 
pt <- cor(forest_pre,forest_post)

#Kendall - for non-linear data
#use cor for unpaired, and cor.test for paired
pt_k <- cor.test(forest_pre,forest_post, method = "kendall")
pt_k

#which correlation coefficient is higher?


#####Fisher Exact Test######
east <- c(10,1)
west <- c(1,8)
pin=rbind(east,west)
colnames(pin)=c("condor","no.condor")
pin
fisher.test(pin)

##fisher exact test's can be useful for quick sample size calculations##
east <- c(4,0)
west <- c(0,4)
pin=rbind(east,west)
colnames(pin)=c("condor","no.condor")
pin
fisher.test(pin)

###wilcoxon signed-rank test####
forest_pre=forest
forest_post=c(9+2,  6+1,  4+1,  6+1,  7+1, 10+1)


forest_treat <- data.frame(
  trmt=rep(c("forest_pre","forest_post"),
           c(length(forest_pre), length(forest_post))),
  colonies=c(forest_pre,forest_post)
)
forest_treat

##paired
ww<-wilcox.test(forest_pre,forest_post,paired=T)
ww

##unpaired
ww2<-wilcox.test(forest,field)
ww2



####COIN - permutation tests with coin######
install.packages("coin")
library(coin)

oneway_test(colonies~place,data=ants)

oneway_test(colonies~place,data=ants,distribution="exact")

oneway_test(colonies~place,data=ants,distribution=approximate(nresample=9999))
