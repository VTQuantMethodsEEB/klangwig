#ugly plots

library(tidyverse)

set.seed(101)

time = seq(1,100)

imposter.syndrome = sort(rnorm(mean=5,sd=5,100))
hist(imposter.syndrome)

career_stage = c("elementary","hs","college_early","collage_late","grad_school","asst_prof","assoc_prof","full_prof")

dat =data.frame(time, imposter.syndrome)

dat$career_stage[dat$imposter.syndrome<0.25]="full_prof"
dat$career_stage[dat$imposter.syndrome>10]="grad_school"
dat$career_stage[dat$imposter.syndrome>12]="asst_prof"
dat$career_stage[dat$imposter.syndrome>0.25&dat$imposter.syndrome<10]="college_early"
dat$career_stage[dat$imposter.syndrome>0.25&dat$imposter.syndrome<7]="assoc_prof"
dat$career_stage[dat$imposter.syndrome>0.25&dat$imposter.syndrome<5]="hs"
dat$career_stage[dat$imposter.syndrome>0.25&dat$imposter.syndrome<3]="college_late"

library(ggthemes)

dat$career_stage = factor(dat$career_stage, c("hs", "college_early","college_late","grad_school","asst_prof","assoc_prof","full_prof") )

ggplot(data=dat, aes(x=career_stage, y=imposter.syndrome, color=career_stage))+
  geom_boxplot()+
  geom_point(shape=1, size=5, stroke=2)+
  ylab("Magnitude of Imposter Syndrome")+
  xlab("Career Stage")+
  theme_solarized()+
  geom_hline(yintercept=0)
