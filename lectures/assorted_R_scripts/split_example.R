?colsplit

library(reshape2)

id = 1:4
vegdat = c("blackberry", "blackberry,rasp","blackberry", "blackberry, rasp")

bears = data.frame(id, vegdat)

x = colsplit(bears$vegdat, ",", c("plant1", "plant2"))

bears$plant1 = x$plant1
bears$plant2 = x$plant2

bears

#batcounts.wide %>%
#  pivot_longer(-c(site,date), names_to = "species", values_to = "count")

#billboard %>% 
#  pivot_longer (
#    cols = starts_with("wk"), 
#    names_to = "week", 
#    names_prefix = "wk",
#    values_to = "rank",
#    values_drop_na = TRUE
#  )


long.bears = bears %>%
  pivot_longer(cols = c("plant1", "plant2"),
                          names_to = "vegdat.type",
                          values_to = "encounter",
                              values_drop_na = TRUE
               
)

long.bears

unique(long.bears$encounter)

long.bears$encounter[long.bears$encounter==""]=NA

long.bears.consume = long.bears %>%
  drop_na(encounter)
long.bears.consume

long.bears.consume$tally = 1

aggregate(tally~encounter, data = long.bears.consume, FUN=sum)
