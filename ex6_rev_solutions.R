##examples - solutions

#A wolf population has 500 pairs remaining.  
#If each wolf pair normally has around a 4 pups, 
#but the fraction of wolves born with a lethal recessive allele is 0.2, 
#how many live offspring are likely to be born alive?
#(Hint – you need to draw randomly from 2 different distributions!)

#wolves
pupstot =rpois(500,lambda =4)
livebirths = rbinom(500,size =pupstot,p=.8)
sum(livebirths)
#check work
sum(livebirths) / sum(pupstot)



#If there are 1000 seeds in a seedbank 
#and each seed has a 2% chance of being eaten 
#and a 1% probability of sprouting each day, how many 
#seedlings would we expect to see sprouting each day? 

#seedbank
#one day
seeds.not.eaten = rbinom(1000,size=1,p = .98)
seeds.sprouted = rbinom(n = seeds.not.eaten,size=1, p=.01 )
sum(seeds.sprouted)


#The number of eggs laid by squid varies dramatically 
#among individuals. The mean number of eggs is 300, 
#but the dispersion is 1. What is the 85th percentile of the number of eggs laid?

#squid
#Given a p between 0 and 1, qnorm will like up the p-th quantile of the distribution
qnbinom(p=.85,mu = 300, size=1)
quantile(rnbinom(1000,mu = 300, size=1))

#I census 15 populations of pikas on 15 mountains. 
#I find that the average population size is 10. What is 
#the probability of  a population of more than 12 individuals?
#(Hint – there is information here that you don’t need…)


#pikas
#p functions give you the CDF or prob that something is more than your value
#what is the probability that the value is less than or equal to x
ppois(q = 12, lambda = 10, lower.tail = F)

plot(ecdf(rpois(1000,lambda = 10)))
