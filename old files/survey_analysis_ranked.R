#ranked_choice

library(RankAggreg)
library(clValid)

#data(mouse)
#express <- mouse[1:100,c("M1","M2","M3","NC1","NC2","NC3")]
#rownames(express) <- mouse$ID[1:100]
#set.seed(100)
#result <- clValid(express, 5,
#                    + clMethods=c("hierarchical","fanny","model", "kmeans","sota","pam","clara",
#                                  + "agnes", "diana"), validation=c("internal","stability"))

#analyse survey data
###NOTE: BEFORE READING IN, REPLACE COLUMN HEADERS IN EXCEL WITH RANK!
surv = read.csv("survey_2021.csv")

library(tidyverse)
head(surv)

res = surv %>%
  pivot_longer(cols=starts_with("X"),
               names_to = "rank",
               names_prefix = "X",
               values_to = "topic",
               values_drop_na = TRUE)

head(res)
res$rank = as.numeric(res$rank)

res %>%
  group_by(topic)%>%
  summarise(avg.rank = mean(rank, na.rm=T), 
            stdev = sd(rank, na.rm=T), 
            se = sd(rank, na.rm=T)/sqrt(n()))%>%
  arrange(avg.rank)

ggplot(res, aes(topic, rank))+
  geom_point() +
  stat_summary()+
  theme(strip.background = element_rect(fill="gray97"),strip.text.x = element_text (size = 8,hjust = 0.5, vjust = 0.5,face="italic"),axis.title=element_text(size=12),axis.text=element_text(size=12, angle = 90),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.text = element_text(size=13,face="italic"),legend.title = element_blank(),legend.background = element_blank(),legend.key=element_rect(fill="white",color="white"))

