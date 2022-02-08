#ex12_nb_mixed

liz = read.csv("lizards.csv")

library(lme4)

## run mixed model
m.nb <- glmer.nb(N ~ time*light + (1|height), data=liz)
#look at mixed model
summary(m.nb)
