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

#Factors
#useful for categorical data
#predefined values called "levels"
surveys$sex
surveys$sex <- factor(surveys$sex)
#define new factor: factor(c(1, 2, 1, 1, 2))
#default: levels sorted alphabetically
levels(surveys$sex)
nlevels(surveys$sex)

sex <- factor(c("male", "female", "female", "male"))
sex <- factor(sex, levels = c("male", "female"))

#exercise
surveys$taxa <- factor(surveys$taxa)
surveys$genus <- factor(surveys$genus)

class(surveys$taxa) #output factor

View(surveys)
summary(surveys)

levels(surveys$taxa)
sum(surveys$taxa == "Rabbit") #output 75
nlevels(surveys$genus) #output 26

#convert factors
as.character(sex)


year_fct <- factor(c(1990, 1983, 1977, 1997))
as.numeric(year_fct) #output [1] 3 2 1 4
as.numeric(as.character(year_fct)) #works but not the nicest way
as.numeric(levels(year_fct))[year_fct] 
#extract levels, convert into numbers (numeric version of levels, but not vector) 
#--> year_fct used for indexing of levels

#Renaming factors
summary(surveys$sex)
sex <- surveys$sex
plot(sex) #plot with 2 columns
levels(sex) #output [1] "F" "M"
sex <- addNA(sex)
levels(sex) #output [1] "F" "M" NA 
levels(sex)[3] <- "undertermined"
levels(sex) #output [1] "F" "M" "undertermined" 
plot(sex) #plot with additional column "undetermined"

levels(sex)[1:2] <- c("female", "male")
plot(sex)

#change order of columns in plot by changes order of levels in factor
sex <- factor(sex, levels = c("undertermined", "female", "male"))
plot(sex)
