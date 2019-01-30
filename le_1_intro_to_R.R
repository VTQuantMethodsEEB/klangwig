rm(list=ls()) # clears workspace


#assign something
x <- 23
#or
x=23

#what is x again?
print(x)
#or
x

#do something with x
x + 17
y <- x + 17
y

#[switch to lecture]
#exploring values
num <- 3
str(num) 

char <- "Hello, class?"
str(char)

logic <- TRUE ## Note lack of quotes
str(logic)

#factors
# c() mean concatenate, and puts elements together into a vector
species<-as.factor(c("epfu","mylu","mylu","mysee"))
#whoops - mysee should be myse
#square brackets read like "where"
#so the command below says in the vector species, where the vector species has the value "mysee", change it to "myse"
species[species=="mysee"]="myse"
#we get a warning message
print(species)
#now that is a missing value because "myse" wasn't one of our original factors
#an easy solution is to change text to a character
species<-as.factor(c("epfu","mylu","mylu","mysee"))
species<-as.character(c("epfu","mylu","mylu","mysee"))

species[species=="mysee"]="myse"
print(species)

#vectors
words <- c("Mary", "had", "a", "little", "lamb")
# c() puts elements together into a vector
str(words)

print(v <- 1:5)  ## 'm:n' creates a sequence from m to n
str(v)

#vector math
v <- 1:5
w <- c(0, 1, 1, 2, 4)
v+w
2*w #2 is the scalar

#[back to lecture]
#lists
L <- list(1:3, "Apple tree", TRUE)
str(L)
str(L[1]) ## L[1] picks out the first element of the list
LL <- list(L, c(2, 7, 9))
str(LL)
str(LL[1])


#functions
mean(c(2, 5, 11))

x <- 1:10
m_x <- mean(x)
print(m_x)

#getting help
?mean

x <- (c(c(1:10),NA,23))
mean(x)
#produced an NA because there is a missing value
mean(x, na.rm=T)

#[switch to lecture]

#matrices
m <- matrix(c(1, 3, 0, 1), nrow=2)
print(m)
# R arranges vectors into matrices in COLUMN-FIRST order, by default

#dataframes
weight<-1:10
height<-3:12
mydata=data.frame(weight,height)
#now I have a dataframe with two columns, x1, and x2
mydata
#head is very useful, it shows just the top of the data frame
head(mydata)
#what did I call those variables again?
names(mydata)

#add a new column that is just height and weight
mydata$sum <-  mydata$height + mydata$weight
head(mydata)
#another exceptionally useful command - View (note capital!)
View(mydata)
#this opens a new window that looks like excel. Here you can filter commands and examine your data for errors a bit more easily. 

#selecting example
# InsectSprays is a built-in data set in R
class(InsectSprays)
head(InsectSprays)
names(InsectSprays)


# Select a subset of InsectSprays containing only the first list element
InsectSprays[1]

InsectSprays[1,]
# Select the first column of InsectSprays
InsectSprays[, 1]

# Select part of the first column of InsectSprays
InsectSprays[1:10, 1]

# Select the first row of InsectSprays
InsectSprays[1,]

# Select the fifth row of InsectSprays
InsectSprays[5,]

#in practicality, we often don't know where a particular element is in the dataframe
InsectSprays[InsectSprays$spray=="A",]
#this gives you all the values where spray is A
InsectSprays[InsectSprays$spray=="A"&InsectSprays$count<10,]
#there is only one value where insect spray is less than 10

#compare this with or which is the | symbol (that a straight line, not an I)
InsectSprays[InsectSprays$spray=="A" | InsectSprays$count<10,]

?as.Date


#example of read.csv
r2=read.csv("/Users/klangwig/Dropbox/Contact rate MS/Network graph/Network data/enviro_master.csv")
#you will need to change this to match your directory

#date example
head(r2)
unique(r2$date)
r2$date2 = as.Date(r2$date, "%m/%d/%y" )
?julian
unique(julian(r2$date2))

#example of install
#install.packages("ggplot2")

#load installed packages
library(ggplot2)
  
         
