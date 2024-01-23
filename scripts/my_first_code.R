my_weight_kg <- 50
(my_weight <- 50) #combines assignment and output of value
my_weight

round(3.14159)
round(3.14159, 2)
ceiling(9.2)
floor(9.2)
trunc(9.2)
?args

weight_g <- c(50,60,65,82)
animal <- c("mouse", "rat", "dog")
length(animal)
class(animal)
str(animal)
typeof(animals)

animals <- c(animal, "frog")
animals[4] <- "cincilla"
animals <- c("cincilla", animals)

more_animals <- animals[c(1,2,3,2,1,4)]
more_animals

weight_g
weight_g[c(F, F, T, T)]
weight_g > 63
weight_g[weight_g > 63]
weight_g[weight_g > 63 & weight_g < 80]
weight_g[weight_g > 63 | weight_g < 80]
weight_g == 65
# <, >, ==, !=, <=, >=, &, |

animals[animals == "rat" | animals == "frog"]
# %in% helps us find all elements corresponding to a vector of elements of our choice
animals %in% c("rat", "frog", "cat", "duck", "dog")
animals[animals %in% c("rat", "frog", "cat", "duck", "dog")]

heights <- c(2, 4, 4, NA, 6)
mean(heights)
#output NA
mean(heights, na.rm = TRUE)
#output 4
max(heights)
max(heights, na.rm = TRUE)
#identify which are the missing data (a.k.a NA)
is.na(heights)
heights[!is.na(heights)] #returns vector without NAs
#omit the missing data
na.omit(heights)
#extract the comlpete cases
complete.cases(heights)
heights[complete.cases(heights)]

#challenge
heights <- c(63, 69, 60, 65, NA, 68, 61)
heights_no_na <- na.omit(heights)
heights_no_na <- heights[!is.na(heights)]
heights_no_na

median(heights, na.rm = TRUE)
heights_no_na[heights_no_na > 67]
length(heights_no_na[heights_no_na > 67])
sum(heights_no_na > 67)
