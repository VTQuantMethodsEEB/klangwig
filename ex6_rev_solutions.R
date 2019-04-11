##examples - solutions
#wolves
pupstot =rpois(500,lambda =4)
livebirths = rbinom(500,size =pupstot,p=.8)
sum(livebirths)
#check work
sum(livebirths) / sum(pupstot)


#seedbank
#one day
seeds.not.eaten = rbinom(1000,size=1,p = .98)
seeds.sprouted = rbinom(n = seeds.not.eaten,size=1, p=.01 )
sum(seeds.sprouted)

#100 days #this isn't right - need to iteratively draw
sum(rbinom(n = seeds.not.eaten,size=100, p=.01 ))

#squid
qnbinom(p=.85,mu = 300, size=1)
quantile(rnbinom(1000,mu = 300, size=1))

#pikas
ppois(q = 12, lambda = 10, lower.tail = F)

plot(ecdf(rpois(1000,lambda = 10)))
