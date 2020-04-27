#ex13_solutions


g.zinf1 <- zeroinfl(opalinus ~ time, 
                    dist = "poisson",
                    data = liz)

g.zinf2 <- zeroinfl(opalinus ~ time, 
                    dist = "negbin",
                    data = liz)

lrtest(g.zinf1,g.zinf2)
AIC(g.zinf1,g.zinf2)

##predict model
head(liz)
liz$yhat <- predict(g.zinf2,type="response")

library(ggplot2)
ggplot(liz, aes(x = time, y = yhat)) +
  geom_point(aes(x = time, y = opalinus)) +
  geom_point(color="blue") 
