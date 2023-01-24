#le2_data_manipulation  

#load important packages##
library(tidyverse)
library(tidyr)
library(dplyr)

#### Read in your data

batdat = read.csv("bat_data.csv")
# The function read.csv() reads in your .csv file containing all your data
# The green text within the " " marks indicates where your file is (its directory),
# which you will need to specify. 

head(batdat) 
# This returns the first 6 lines of data from your new dataframe, "batdat"

str(batdat)
# This tells you the type of data contained in your batdat dataframe. 
# As you can see, our "date" column is currently being read by R 
# as a character, not as a date. We need to change that!

#### Check your data

head(batdat)
# Good first pass at checking your data

unique(batdat$species)
# Returns unique values in the species column. If we had naming inconsistencies,
# this would be a good way to find them. 

####---------------------tidyverse----------------------####

# resources:
# https://tidyverse.tidyverse.org/
# https://dplyr.tidyverse.org/  
# https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf

# the tidyverse is a powerful set of separate packages
# with functions that make working with data in R much easier
# however tidyverse functions often rely heavily on piping
# this is a pipe : %>%
# the pipe says the word, 'then'

#install package if you don't have it
#remove the hashtag to install the packages
#install.packages('tidyverse')

# load the tidyverse package
library(tidyverse)


# my favorite tidyverse command - group_by!
# group_by is kind of like using excels version of filter
# to create small a small dataset that is grouped by the variables
# for example, if I group_by species, date
# this is the equivalent in excel of selecting a species (e.g. MYLU)
# and a specific date (e.g. March 1 2016, 3/1/16)
# except group_by does this for every species and date combination in your dataset
# this is incredibly useful because in other programs, you might need
# to write a for loop to have this capability

## Group by, Mutate, and Summarise

#first add a column to the infection data that is the log10 of fungal loads
batdat$lgdL = log10(batdat$gdL)

# Summarise
batdat %>% 
  group_by(species) %>% 
  summarise(mean.fungal.loads=mean(lgdL,na.rm=TRUE))
# this gives you a summary table, it doesn't change batdat!

# if you want to call this table something you would need to assign it
# when using summarise, you always want to call your summary table something different
# this is like making a pivot_table in excel

fungal.load.table = batdat %>% 
  group_by(species) %>% 
  summarise(mean.fungal.loads=mean(lgdL,na.rm=TRUE))

fungal.load.table

## Summarise versus Mutate

# Mutate adds a column
# when mutating, you can just call the object you are making
# the same thing as your original dataframe
# because you are adding a column based on your
# original dataframe
batdat = batdat %>% 
  #replace batdat with a new dataframe that has something you want
  group_by(site,species,date) %>% 
  #you can group_by multiple things
  mutate(sample.size=n())
#this adds a column to the dataframe
batdat



## Joining

#Just as an example, let's make a bat counts dataset 
#(imagine this is a new file we are reading in)
batcount<-aggregate(count~species+site+date,data=batdat, FUN=mean)  #make a df of bat counts

# joining datasets together is a useful skill
# especially if we have two datasets we need to match on a specific column
# or set of columns
# https://dplyr.tidyverse.org/reference/mutate-joins.html

# lets join add the bat count data with the infection data

# always call your new dataframe something new
# don't write over and old dataframe in case you make
# a mistake joining

#inner_join(): includes all rows in x and y.
#when inner joining, non-matching rows will be dropped!

#left_join(): includes all rows in x.
# when left joining, every row in x is kept, but only those matching
# x are kept in y

#right_join(): includes all rows in y.
# when right joining, every row in y is kept, but only those matching
# y are kept in x


#full_join(): includes all rows in x or y.
# every row is kept in both x and y

batdat_count = left_join(
  x = batdat,
  y = batcount,
  by = c("site","species","date")
)

View(batdat_count)
# now, every row in batdat remains, and we have merged in counts

## Pivoting
# https://tidyr.tidyverse.org/articles/pivot.html
#sometimes our datasets are not in the format we want for an analysis

head(batdat_count)
# right now, species is in long format
# but we could imagine wanting to test the abundance of one species 
# and how that influences another
# for example, does the number of MYLU influence the number of MYSE?
# for that, we would need to make columns of each species count
# with a row for a site and a date

#pivot_wider is how we take long data, and make it wide

batcounts.wide<- batcount %>% 
  #this says - make a new df called batcounts.wide using bat counts
  pivot_wider(names_from = species, 
              values_from = count
  ) 
##make columns for each of the values in the species column and fill those columns with values from the count column
View(batcounts.wide)


# when we perform this, it automatically fills missing with NAs
# but we know that missing actually mean 0

batcounts.wide = batcount %>% 
  pivot_wider(
    names_from = species, 
    values_from = count,
    values_fill = 0
  )

# more often, we might be working in the opposite direction
# IMO, a more common issue is that programs output SO much
# information in columns that we would rather have in rows

batcounts.long = batcounts.wide %>% 
  pivot_longer(
    cols = c("EPFU","MYLU","MYSE","PESU"), 
    #what are the existing columns I want to make into rows?
    names_to =   "species",
    #put the names of the columns in a column called 'species'
    values_to = "count"
    #the values that were in each of the columns get moved to a column called 'count'
  )

View(batcounts.long)


##some other example in notes if you need more clarity
##will likely skip for time
fish_encounters

#let's say we want a new df where each row is a station
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

#we didn't include 0's at stations where we didn't observe fish so we can substitute those in
# Fill in missing values
fish_encounters %>%
  pivot_wider(
    names_from = station, #make the column names based on the unique values of the station column
    values_from = seen, #fill in these columns based on data in the 'seen' column
    values_fill = list(seen = 0) #if there isn't a value in seen, give it a zero (the fish wasn't observed)
  )

#what if we have a wide dataframe and want to make it "tidy"
head(relig_income) #welp this is awful

relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "count") 
#the minus sign says don't include the column religion
#make all of the other column names rows in a column called 'income'
#make another column called 'count' and associate the values in each of the income classes with that count
#now you have a variable income and a count of the number of people that were in that income class

head(billboard)
#this is a dataset of songs and their ranking

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), #take all the cols that start with 'wk' 
    names_to = "week", #stack them in a column called "week"
    values_to = "rank",#the values that were in those columns before should be stored in column called rank
    values_drop_na = TRUE #get rid of the NAs - it means they weren't on the chart. Alternatively could be '101'
  )

#we probably want week to be an integer, not a character
billboard %>% 
  pivot_longer (
    cols = starts_with("wk"), 
    names_to = "week", 
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )
