#we will be talking about data.frames

#Let's import some data
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

library(tidyverse)
dplyr::filter #specifies which package a function is supposed to used from, 
#if multiple functions with the same name exist

surveys <- read_csv("data_raw/portal_data_joined.csv") 
#alternative: base R function read.csv (makes less information available on the first glance)
#read.csv2 - for files which use a semicolon as a delimiter
#in read.table different seperators, quotes etc. can be specified
head(surveys, 10)
tail(surveys)
View(surveys)
str(surveys)
dim(surveys) #which dimension does the dataframe have? output: rows, cols
nrow(surveys)
ncol(surveys)
names(surveys)
#equivalent to
colnames(surveys)
rownames(surveys)
summary(surveys) #summary statistics

#Indexing and subsetting
surveys[1, 6] #select row and column
surveys[1,]
surveys[, 1]
surveys[c(1,2,3), c(5,6)] #equivalent to:
surveys[1:3, 5:6]
surveys[,-1] #remove column 1
surveys[, "sex"] #outout is a subset table
surveys["sex"] #outout is a subset table
surveys$sex #output is a vector

#exercises
#1
surveys_200 <- surveys[200,]
surveys_200
class(surveys_200)
#2
x <- nrow(surveys)
surveys[x, ]
tail(surveys)
surveys[nrow(surveys),]
surveys_last <- surveys[nrow(surveys),]
class(surveys_last)
nrow(surveys)/2
#3

#list: several vectors of different length --> very flexible
#a dataframe is a specific kind of a list (with all vectors having the same length), 
#whereas not all lists are dataframes
my_list <- list(names = c("Nora", "Lisanna", "Francesco"),
                money = c(1, 6, 7, 3, 5, 8))
my_list
class(my_list)
my_list[[1]]
my_list$names
View(my_list)

surveys[[3]] #extract the 3rd column

#strg + shift + c --> transforms marked lines into comments all at once
