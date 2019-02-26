##Examples from distributions lecture##

number.of.bats = 100
rbinom(n = 10,size = number.of.bats,prob = 0.48)/number.of.bats
#this says "draw randomly from a normal distribution", with ten trials (sites) of 100 bats, 
#each with a probability of being infected equal to 48%

curve (dnorm,-5,5)
curve(pnorm,-5,5)

#density distributions
?dbinom
dbinom(x=1,size = 25, prob = 0.2)
#we use x=1 because we want to know the probability that a single trial (e.g. 1 bat of 25) 
#is infected at a global infection prevalence of 20%
dbinom(1,25, 0.2)
#this is the same as above

#p functions
?pnorm
#what is the probability a lizard weighs less than 19 grams?
pnorm(q = 19, mean = 17, sd =3)

#what is the probability a lizard weighs MORE than 19 grams?
pnorm(q = 19, mean = 17, sd =3, lower.tail = F)

#what is the probability that no more than 1 bat is infected?
pbinom(q=1,size = 25, prob = 0.2)

#what is the probability that no more than 5 bats are infected?
pbinom(q=5,size = 25, prob = 0.2)

#this can be useful for quick sample size calculations
#what is the probability that 0 bats are infected with my sample size?
pbinom(q=0,size = 25, prob = 0.2)

#what is the 85th percentile of a fossil distribution?
#quantile functions
qnorm(0.85, mean=115, sd=15)

#random draws'
#Poisson
?rpois
rpois(100, lambda = 3)
x = rpois(100, lambda = 3)
hist(x)

#Negative Binomial
?rnbinom
#here, mu is the mean, and size is the dispersion parameter (k)
#note - this is the preferred parameterization in ecology
x = rnbinom(50, size = 10, mu = 2)
hist(x)
#Here, I take 50 random draws from a neg. bin. distribution with a a dispersion (k)
#of 10 and a mean = 2 


##fitting a distribution to actual data
liz = read.csv("lizards.csv")
head(liz)
hist(liz$N)

library(MASS)
set.seed(123)
nb.fit = fitdistr(liz$N,densfun = "negative binomial" )
nb.fit

##if you need to supply start list
nb.fit = fitdistr(liz$N, densfun = "negative binomial", start = list(size = 1, mu = 15))


length(liz$N)

hist(rnbinom(23,size = 1.38, mu = 24.52))

new.liz = rnbinom(23,size = 1.38, mu = 24.52)
hist(new.liz)

#what is the 80 percentile of number of lizards perching?
qnbinom(p = .8, mu = 24.52, size = 1.38)

#what is the probability of exactly 1 lizard perching?
pnbinom(q=1,mu = 24.52, size = 1.38)
