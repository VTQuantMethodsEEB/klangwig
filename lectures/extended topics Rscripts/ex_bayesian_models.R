library(brms)
library(dplyr)
library(ggplot2)
library(ggthemes)

bat  = read.csv("bat_data.csv")

head(bat)
names(bat)

prior=set_prior("normal(0,10)", class = "b")
#set_prior is used to define prior distributions for parameters in brms models. 
#To put the same prior on all population-level effects at once, 
#we may write as a shortcut set_prior("<prior>", class = "b")
#set_prior("normal(0,5)", class = "b", coef = "x1") 
#will define each coef seperately and you can combine them

bmod_inf=brm(gd~species+(1|site),family="bernoulli", 
             data=bat, prior = prior,chains = 4,
             control = list(adapt_delta = 0.95),cores=2,
             save_all_pars = TRUE)

summary(bmod_inf, waic = TRUE)
#interpretation: intercept is SPECIES: EPFU
# MYLU has higher prob of infected (95% CI doesn't overlap 0)
# MYSE has higher prob of infected but 95% CI does overlap 0, this wouldnt be sig. in a frequentist context
#same with PESU and substrate
#you want Rhat = 1 (deviations from this you need smaller steps, increase adapt delta)
# read more here: https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf

plot(bmod_inf)
#chains should looks like fuzzy catepillar
#posteriors are shown as distributions

conditional_effects(bmod_inf)
#this is the equivalent of our 'effects' from frequentist stats

#####
newdat_l=expand.grid(species = factor(c("MYLU","MYSE","PESU","EPFU", "SUBSTRATE")))
head(newdat_l)
fit= as.data.frame(fitted(bmod_inf,newdata=newdat_l,re_formula=NA,type="response"))
newdat_l=bind_cols(newdat_l,fit)

head(newdat_l)

###aggregate data for plotting
newdat_l1<-data.frame(newdat_l)

names(newdat_l1)

##make the figure##
p1=ggplot(data=bat, aes(x=species, y=gd, color=species))+
  geom_jitter(height=0.1)+
  geom_point(data=newdat_l1, aes(x=species,y=Estimate), size=4, color="black")+#size=2.5
  geom_pointrange(data=newdat_l1,aes(x=species, y=Estimate, ymin = Q2.5, ymax = Q97.5), 
                  color="black", size=.5)+
  ylab("Infection")+
  xlab("Species")+
  theme_tufte()+
  guides(scale="none", colour ="none" )+ #don't plot the color legend
  theme(strip.background = element_blank(),legend.key.width = unit(.4, "cm"), axis.title=element_text(size=20),strip.text = element_text(size=10),
        axis.text.x=element_text(angle=45,hjust=1,size=8,face = "italic"),axis.text=element_text(size=15),
        panel.grid = element_blank(), axis.line=element_line(),legend.position = c(0.92, 0.9),
        legend.text=element_text(size=7),legend.title=element_blank(),legend.box = "horizontal",legend.direction = "horizontal")
p1
