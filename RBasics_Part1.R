# ********************************************************************************
#                      R Basics – Part 1
# ********************************************************************************
# Course: SYS 3501-001: Intro to R
# Tips:
#  • Run this script top-to-bottom the first time. Re-run sections as needed.
# ---------------------------------------------------------------------------------
# SETUP: Packages and session info
# ---------------------------------------------------------------------------------

install.packages("psych")
library(psych)

# Best practice: only install packages if missing; then load them.
# (Avoid calling install.packages() unconditionally; it slows class and needs admin rights.)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# p_load(): installs (if needed) and then loads in one call
pacman::p_load(psych, datasets, readr, dplyr)

# Useful to capture versions for reproducibility
sessionInfo()

# ---------------------------------------------------------------------------------
# HELP SYSTEM
# ---------------------------------------------------------------------------------
# ?function_name opens help, e.g.:
?mean

# pacman::p_help() shows package help index in the Help pane (set web = TRUE for browser)
pacman::p_help(psych, web = FALSE)

#if you have the package installed, you can also use:
p_help(psych, web = FALSE)

# ---------------------------------------------------------------------------------
# READING DATA
# ---------------------------------------------------------------------------------

#Systematic setting of working directories for source files and data is extremely important

#first let's see what our working directory is:
getwd()

#now let's set our directory where our data is stored manually:
setwd("~/Library/CloudStorage/GoogleDrive-laura.e.barnes@gmail.com/My Drive/UVA/Courses/ProgSIE/Data/")

#list the files in the current working directory:
list.files()

#or

dir()

?readr

# Read comma-separated file with read.csv 
iriscsv <- read.csv("iris.txt", header = TRUE, sep = ",")

# Quick checks
head(iriscsv)
str(iriscsv)
summary(iriscsv)

# ---------------------------------------------------------------------------------
# BUILT-IN DATASETS
# ---------------------------------------------------------------------------------
# The datasets package ships with R. iris is a classic example.
head(iris)
summary(iris)
str(iris)

# Base R plot: scatterplot matrix
plot(iris)

# ---------------------------------------------------------------------------------
# DESCRIPTIVE STATS with psych::describe()
# ---------------------------------------------------------------------------------
# describe() expects numeric columns. Use is.numeric.
# Identify numeric columns in iris
num_cols <- sapply(iris, is.numeric)

#what does sapply do?

# Subset those columns
num_only <- iris[ , num_cols]

head(num_only)


describe(num_only)
describe(iris$Sepal.Length)

# ---------------------------------------------------------------------------------
# CLEAN UP (Optional in class)
# ---------------------------------------------------------------------------------
# If you do use it, explain what it does and why.
# rm(list = ls())         # removes all objects from the environment
# pacman::p_unload(all)   # unload all non-base packages
# cat("\014")              # clear console (Ctrl+L in RStudio)

# ---------------------------------------------------------------------------------
# DATA TYPES
# ---------------------------------------------------------------------------------
# Numeric (double by default)
n1 <- 15; typeof(n1)
n2 <- 1.5; typeof(n2)

# Character
c1 <- "c"; typeof(c1)
c2 <- "a string of text"; typeof(c2)

# Logical
l1 <- TRUE; typeof(l1)
l2 <- FALSE; typeof(l2)

# ---------------------------------------------------------------------------------
# DATA STRUCTURES
# ---------------------------------------------------------------------------------
# Vectors (1D, same type)
v1 <- c(1, 2, 3, 4, 5); is.vector(v1)
v2 <- c("a", "b", "c"); is.vector(v2)
v3 <- c(TRUE, TRUE, FALSE); is.vector(v3)

# Matrix (2D, same type)
m1 <- matrix(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE), nrow = 2)
m2 <- matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)

# Array (like matrix, but ≥3 dimensions)
a1 <- array(1:24, dim = c(4, 3, 2))

# Data frame (columns can have different types)
vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(TRUE, FALSE, TRUE)

# cbind() forces a matrix (one type); as.data.frame() preserves types
dfa <- cbind(vNumeric, vCharacter, vLogical)      # becomes character matrix
class(dfa)

df  <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
class(df)
str(df)

# Lists (flexible containers)
o1 <- c(1, 2, 3)
o2 <- c("a", "b", "c", "d")
o3 <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
list1 <- list(o1, o2, o3)
list2 <- list(o1, o2, o3, list1)  # lists within lists
# Indexing: single [ ] returns a sublist; [[ ]] returns the element itself
list1[1]     # sublist of length 1
list1[[1]]   # numeric vector (1,2,3)

# ---------------------------------------------------------------------------------
# TYPE COERCION
# ---------------------------------------------------------------------------------
# Automatic coercion to the least restrictive type
coerce1 <- c(1, "b", TRUE); typeof(coerce1)  # -> character

# Numeric to integer
coerce2 <- 5; typeof(coerce2)
coerce3 <- as.integer(5); typeof(coerce3)

# Character to numeric
coerce4 <- c("1", "2", "3"); typeof(coerce4)
coerce5 <- as.numeric(coerce4); typeof(coerce5)

# Matrix to data frame
coerce6 <- matrix(1:9, nrow = 3); is.matrix(coerce6)
coerce7 <- as.data.frame(coerce6); is.data.frame(coerce7)

# ---------------------------------------------------------------------------------
# SEQUENCES & REPEATS
# ---------------------------------------------------------------------------------
# Colon operator creates integer sequences
x1 <- 0:10
x1

x2 <- 10:0
x2

# seq() is more flexible
?seq
x3 <- seq(10)                 # 1..10
x4 <- seq(30, 0, by = -3)     # 30, 27, ..., 0

# Concatenate values with c()
?c
x5 <- c(5, 4, 1, 6, 7, 2, 2, 3, 2, 8)
x5

# scan() reads values from the console (interactive)
?scan
x6 <- scan()
x6

# rep() repeats values
?rep
x7 <- rep(TRUE, 5)
x8 <- rep(c(TRUE, FALSE), 5)     # repeat the vector 5 times
x9 <- rep(c(TRUE, FALSE), each = 5)  # repeat each element 5 times

# ---------------------------------------------------------------------------------
# FACTORS (categorical data)
# ---------------------------------------------------------------------------------
(x1 <- 1:3)
(y  <- 1:9)

df1 <- data.frame(x1, y)
str(df1)

#What do you notice about df1?


# Define factor with specific levels (order matters in modeling/plots)
df3 <- data.frame(x3 = 1:3, y)
df3$x3 <- factor(df3$x3, levels = c(1, 2, 3))
str(df3)

# Apply labels
df4 <- data.frame(x4 = 1:3, y)
df4$x4 <- factor(df4$x4, levels = c(1, 2, 3), labels = c("macOS", "Windows", "Linux"))
str(df4)

# Ordered factors (useful for Likert scales)
df5 <- data.frame(x5 = 1:3, y)
df5$x5 <- ordered(df5$x5, levels = c(3, 1, 2), labels = c("No", "Maybe", "Yes"))
str(df5)

# ---------------------------------------------------------------------------------
# CONTROL STRUCTURES
# ---------------------------------------------------------------------------------
# if / else
x1 <- 200
x2 <- 5000

if (x2 > x1) {
  print("x2 is greater than x1")
} else if (x1 == x2) {
  print("x1 and x2 are equal")
} else {
  print("x1 is greater than x2")
}

# while loop
i <- 1
while (i < 10) {
  print(i)
  i <- i + 1
}

# while with break
i <- 1
while (i < 10) {
  print(i)
  i <- i + 1
  if (i == 9) break
}

# for loop
for (x in 1:10) print(x)

letters_vec <- c("A", "B", "C")
for (x in letters_vec) print(x)

#vectorization vs loops
nums <- 1:10
nums_squared <- nums^2      # vectorized (preferred- faster and cleaner)
nums_squared

# Equivalent with a for loop (longer, less efficient) ------------
# nums_squared_loop <- c()
# for (i in nums) {
#   nums_squared_loop <- c(nums_squared_loop, i^2)
# }
# nums_squared_loop

# ---------------------------------------------------------------------------------
# FUNCTIONS
# ---------------------------------------------------------------------------------
hello_world <- function() {
  print("Hello World!")
}
hello_world()

add_nums <- function(x1, x2) {
  x1 + x2   # implicit return of last expression
}

add_nums(5, 7)

# ---------------------------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------------------------
