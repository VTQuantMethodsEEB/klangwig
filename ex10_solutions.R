#ex10_glms - solutions

#run to the bottom of ex10_glms
#day and light conditions for lizards

q1 = glm.nb(N~time*light,data=liz)
summary(q1)

library(effects)
plot(allEffects(q1))
library(lsmeans)
lsmeans(q1, pairwise~time*light)

##probit link
q3a = glm(gd~date+species,data=bat, family=binomial(link=logit))
summary(q3a)

q3b = glm(gd~date+species,data=bat, family=binomial(link=probit))
summary(q3b)

