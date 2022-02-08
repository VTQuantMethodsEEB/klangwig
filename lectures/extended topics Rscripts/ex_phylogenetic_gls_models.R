#ex14_advanced_models_cont
#load libraries
#possibly new
library(ape)
library(geiger)
library(phytools)

#maybe have
library(nlme)

#Simulate a small tree
phy <- pbtree(n=5, tip.label=c("A", "B", "C", "D", "E")) 
brlength <- signif(phy$edge.length, 3) #branch length, rounded

#plotting tree
par(mar=c(0,1,0,1))
plot(phy, label.offset=0.05)
edgelabels(brlength, adj=c(0.5, -0.25)) #adding branch length abels

#which of these "species" would we expect to have most similar trait values?

#displaying the corresponding phylogenetic variance-covariance matrix
vcv.phylo(phy)

#simulate a large tree
phy <- pbtree(n=100)
plot(phy)

#Liam's fastBM() creates continuous trait under the BM model (plus BM with trend, and bounded BM)
x <- fastBM(phy, sig2=0.2) # sig2 is the the instantaneous rate variance of the BM process
y0 <- fastBM(phy, sig2=0.2) # y is here simulated completely independent of x
y <- 0.7*x + y0 # y is forced to be strongly correlated with x

sim.data <- as.data.frame(cbind(x=x, y=y))

#now we can calculate the PGLS (phylo GS) model with Pagel's lambda fixed at 1
#apparently this assumes an ornstein-uhlenbeck model of evolution
fit1 <- gls(y ~ x, data=sim.data, correlation=corPagel(1, phy, fixed=TRUE))
summary(fit1)

#we can also calculate the PGLS model with Pagel's lambda fixed at 0 (=OLS)
fit0 <- gls(y ~ x, data=sim.data, correlation=corPagel(0, phy, fixed=TRUE))


#The models are not equally good
anova(fit1, fit0)

#or jointly estimate the optimal lambda for the residual values of x and y with maximum likelihood
fitML <- gls(y ~ x, data=sim.data, correlation=corPagel(0.8, phy, fixed=FALSE), method="ML")
summary(fitML)
#here, we give 0.8 as a starting value, and say it isn't fixed

#we can also use other models of evolution - here is Brownian motion
fit3 <- gls(y ~ x, correlation = corBrownian(phy = phy),
                  data = sim.data, method = "ML")
summary(fit3)
anova(fit3)



