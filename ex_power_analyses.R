library(pwr)
pwr.t.test(d = 0.8, sig.level =0.05, power =.8 )
#d is effect size, and power is how certain you want to be
pwr.t.test(d = 0.5, sig.level =0.05, power =.8 )
#if you think the effect is small, you need to be more certain
#we basically just make up these numbers because we don't really know

#we can also change the parameters to see how underpowered we will be
#given realistic sample sizes
pwr.t.test(n=10,d = 0.5, sig.level =0.05)
#this isn't great - we have pretty small power
pwr.t.test(n=20,d = 0.5, sig.level =0.05)

##see this for more info
#https://www.statmethods.net/stats/power.html

##example of power analysis simulation##
##TO ADD FOR THURSDAY