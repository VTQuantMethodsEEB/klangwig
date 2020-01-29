#Example R Script for Marissa

#example dat
dat = data.frame(bird=letters[1:10], 
                 eye_score = rpois(10, 3),
                 seconds_with_flock = round(rnorm(10,mean=250,sd=40)), 
                 total_time_observed = 35*60)
View(dat)
head(dat)
#I made up this dataframe where each row is a different bird with a different letter (e.g. bird A)
#Each bird has some eye score (in this case I just drew randomly from a Poisson)
#the seconds with the flock is a just 10 draws from a normal distribution with a mean 250, and a sd=40
#the total time observed is the total number of seconds you watched the bird

dat$prop_with_flock = dat$seconds_with_flock/dat$total_time_observed
mod1=glm(prop_with_flock~eye_score,
         weights = total_time_observed,
         data=dat,family="binomial")

summary(mod1)
