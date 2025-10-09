#Directories
datadir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Data/"
sourcedir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Source/"

#load packages
library(readr)
library(dplyr)
library(ggplot2)


# Load Data
housing <- read_csv(paste0(datadir, "housing.csv"))

#21
#Use a box plot indicate whether the price variable has any outliers. If, so how many does it have?
ggplot(housing, aes(y = price)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(title = "Boxplot of Price", y = "Price ($)") +
  theme_minimal()
  

#22
#Use a box plot to determine the value of the upper whisker of Price. What is this value?
summary(housing$price)

#23
#Use a scatter plot matrix to identify the variable in the data set that has the strongest linear relationship with price among bedrooms sqft  baths. Use one that shows correlation coeffecient.
pairs.panels(housing[, c("price", "bedrooms", "sqft", "baths")],
             method = "pearson",   # correlation method
             hist.col = "steelblue", # color of histograms
             ellipses = FALSE,      # no confidence ellipses
             lm = TRUE,             # add blue regression lines
             main = "Plots")


#24
#Use a scatter plot matrix to identify the correlation coefficient between price and the variable in the data set that has the strongest linear relationship with price (no rounding).
cor(housing$price, housing$sqft)
cor(housing$price, housing$bedrooms)
cor(housing$price, housing$baths)

#26
#generate a boxplot for bedrooms 
ggplot(housing, aes(y =  bedrooms)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(title = "Boxplot of Price", y = "Bedrooms") +
  theme_minimal()
#Fewer than 5 houses have 6 bedrooms.
table(housing$bedrooms)

#27
# Which of the following statements are true about the house(s) with highest price in the data set? (Hint:  Use tidyverse and filter by City) 

#These homes have more than 1 bathroom.
# Graph prices vs bathrooms as a scatter plot 
ggplot(housing, aes(x = baths, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Scatter Plot of Price vs Bathrooms", x = "Number of Bathrooms", y = "Price ($)") +
  theme_minimal()

# One of the observation numbers is 53
#what are the obervation numbers of the house(s) with the highest price?(obervation number is the row number).
max_price <- max(housing$price)
highest_price_houses <- housing %>% filter(price == max_price)
highest_price_houses
which(housing$price == max_price)

# The square footage is greater than 5000 of .
#what are the sqfts of the top houses
highest_price_houses$sqft

#28
#What is the total sum of price for all homes in Oxnard the data set? 
oxnard_total_price <- housing %>%
  filter(City == "Oxnard") %>%
  summarise(total_price = sum(price, na.rm = TRUE))  
oxnard_total_price$total_price

#29
#Now look at the total price for all homes in the data set, but organized by City.  Which city has the highest total dollar amount of homes? (Hint:  Use tidyverse and filter by City) 
city_total_prices <- housing %>%
  group_by(City) %>%
  summarise(total_price = sum(price, na.rm = TRUE)) %>%
  arrange(desc(total_price))
city_total_prices

#30
#How many levels are there for the City variable?
num_cities <- n_distinct(housing$City)
num_cities

#load pima data
pima <- read_csv(paste0(datadir, "pima-indians-diabetes.csv"))

#31
#Which of the variables in this data set can we reasonably assume have missing values (hint: think carefully about what each of the columns represents)?
#Use code to count how many zeroes are in each column and how many NAs are in each . print in table form 
zero_counts <- sapply(pima, function(x) sum(x == 0, na.rm = TRUE))
na_counts <- sapply(pima, function(x) sum(is.na(x)))
missing_values <- data.frame(Zero_Counts = zero_counts, NA_Counts = na_counts)
missing_values

#32
#sum total number of zeroes in plasmaglucose, dbp, tricep, insulin, bmi columns
total_zeroes <- sum(zero_counts[c("plasmaglucose", "dbp", "tricep", "insulin", "bmi")])
total_zeroes

#33
#The total percentage of missing data rounded to the nearest % is: 
total_values <- nrow(pima) * ncol(pima)
total_missing <- sum(zero_counts[c("plasmaglucose", "dbp", "tricep", "insulin", "bmi")]) + sum(na_counts)
percentage_missing <- (total_missing / total_values) * 100
round(percentage_missing)
percentage_missing

#34
#The most frequently missing variable among plasmaglucose, dbp, tricep, insulin, bmi is(use percent): 
missing_percentages <- (zero_counts[c("plasmaglucose", "dbp", "tricep", "insulin", "bmi")] + na_counts[c("plasmaglucose", "dbp", "tricep", "insulin", "bmi")]) / nrow(pima) * 100
missing_percentages


#35
#If we were to remove observations with missing variables, we would still have how observations remaining?
pima_cleaned <- pima %>%
  filter(plasmaglucose != 0 & dbp != 0 & tricep != 0 & insulin != 0 & bmi != 0) %>%
  drop_na()
remaining_observations <- nrow(pima_cleaned)
remaining_observations




