#Directories
datadir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Data/"
sourcedir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Source/"

#load packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)

# Load Data
housing <- read_csv(paste0(datadir,"housing-prices.csv"))

#1
#What is the row index of the house with the lowest price?
which.min(housing$Price)

#2
#What is the square footage of the most expensive house?
housing %>% filter(Price == max(Price)) %>% select(Size)

#3
#Use a box plot to determine whether the "Size" variable has any outliers. If so, how many does it have?
boxplot(housing$Size)$out

#4
#What is the maximum value of the outliers in Size?
max(boxplot(housing$Size)$out)

#5
#Create a new data frame with the outliers from the Size variable, then answer the following two questions.
outliers <- housing %>% filter(Size %in% boxplot(housing$Size)$out)
#What is the mean of Size of these outlier houses (rounded to the nearest integer)?
round(mean(outliers$Size))

#6
#Which of the following statements is true about the outlier houses?
#The average price of the houses identified as outliers in the Size variable is around $545,000.
mean(outliers$Price)
#The maximum price of the outlier houses is $599,900.
max(outliers$Price)
#All of these outlier houses are New.
table(outliers$Age)
#Most of the houses with outlier sizes have 3 to 5 bedrooms (as indicated by the Rooms Variable).
table(outliers$Rooms)

#7
#Which of the following statements are true about the box plot of Bath? define finite ylim
boxplot(housing$Baths)

#8
#Use a scatter plot matrix or individual scatter plots to determine the variable in the data set that has the strongest linear relationship with Size. What is it?
pairs(housing[, c("Size", "Price", "Rooms", "Baths")])


cor(housing$Size, housing$Price)
cor(housing$Size, housing$Rooms)
cor(housing$Size, housing$Baths)

#9
#Which of the following statements is true about the histogram of Size?
hist(housing$Size)


#10
#What is the total sum of price for all Old homes in the data set? 
sum(housing$Price[housing$Age == "Old"])

#11
#Make a frequency heat map of Baths and Rooms. Answer the following question. *You will have to convert both Rooms and Baths to a categorical variable. (hint: use as.factor). don't but make a new data frame 
housing2 <- housing %>% mutate(Rooms = as.factor(Rooms), Baths = as.factor(Baths))
ggplot(housing2, aes(x=Rooms, y=Baths)) + geom_bin2d() + scale_fill_gradient(low="lightblue", high="darkblue") + theme_minimal()


# Install It
install.packages("VIM")
# Load the VIM library
library(VIM)

# Load the airquality dataset
data("airquality")

# "airquality" loaded into your working environment. Display the first few rows of the dataset
head(airquality)

#12
#Which of the following variables are 100% complete?
colSums(is.na(airquality))
#count zeroes in each column 
colSums(airquality == 0, na.rm = TRUE)

#13
#Which month (numeric format, e.g., 9 for September) has the highest average temperature across all days?
aggregate(Temp ~ Month, data = airquality, FUN = mean)
which.max(aggregate(Temp ~ Month, data = airquality, FUN = mean)$Temp)

#Create a histogram for Solar.R in the airquality dataset
hist(airquality$Solar.R, main="Histogram of Solar.R", xlab="Solar.R", col="lightblue", border="black")

#16
#The maximum observed Solar Radiation value is approximately
max(airquality$Solar.R, na.rm = TRUE)
#There are no observations of Solar Radiation in the range of 0-50.
sum(airquality$Solar.R >= 0 & airquality$Solar.R <= 50, na.rm = TRUE)
#The Solar Radiation data follows a normal distribution.
shapiro.test(airquality$Solar.R[!is.na(airquality$Solar.R)])
#There are more than 30 Solar Radiation values below or equal to 100.
sum(airquality$Solar.R <= 100, na.rm = TRUE)


#Draw scatterplot matrix for "Ozone", "Solar.R", "Wind", "Temp" in the airquality dataset. Answer the following questions.
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])

#17
#Which of the following statements are true based on what you can observe in the scatterplot matrix?
# Wind is positively correlated with Solar.R.
cor(airquality$Wind, airquality$Solar.R, use = "complete.obs")

#There is a strong negative correlation between Ozone and Temperature.
cor(airquality$Ozone, airquality$Temp, use = "complete.obs")

#Wind is most strongly correlated with what?
cor(airquality$Wind, airquality$Ozone, use = "complete.obs")
cor(airquality$Wind, airquality$Solar.R, use = "complete.obs")
cor(airquality$Wind, airquality$Temp, use = "complete.obs")
cor(airquality$Wind, airquality$Month, use = "complete.obs")

#what two variables have the highest correlation(look at ozone solar wind and temp)
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use = "complete.obs")

#18/19
#Which of the following shows evidence of skew in its distribution? wind ozone or temp 
hist(airquality$Ozone, main="Histogram of Ozone", xlab="Ozone", col="lightblue", border="black")
hist(airquality$Wind, main="Histogram of Wind", xlab="Wind", col="lightblue", border="black")
hist(airquality$Temp, main="Histogram of Temp", xlab="Temp", col="lightblue", border="black")



#Create box plots for "Wind" by "Month" to visualize the distribution of Wind across different Months. 
boxplot(Wind ~ Month, data = airquality, main = "Boxplot of Wind by Month", xlab = "Month", ylab = "Wind")

#20
#Which of the following statements are true based on the box plots?
#There is a trend that, from May (5) to September (9), the median monthly wind speed increases over time.
aggregate(Wind ~ Month, data = airquality, FUN = median)
#What is the month with the smallest interquartile range (lQR) for wind speed?
IQRs <- aggregate(Wind ~ Month, data = airquality, FUN = IQR)
IQRs[which.min(IQRs$Wind), ]
#whats the month with the highest median wind speed
IQRs[which.max(aggregate(Wind ~ Month, data = airquality, FUN = median)$Wind), ]
#What month has the lowest first quartile
IQRs[which.min(aggregate(Wind ~ Month, data = airquality, FUN = function(x) quantile(x, 0.25))$Wind), ]
