#The goal of this assignment Verify you can use Microsoft Copilot / GitHub Copilot Chat alongside RStudio 
#to accelerate—but not replace—your coding.

# ------------------------------
# 1) Paths & file I/O
# ------------------------------

# 1.1 Set working directories -------------------------------------------------
# EDIT THESE for your environment
traindir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Data/"
sourcedir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Source/"

stopifnot(dir.exists(traindir))
setwd(traindir)
message("Working directory set to: ", getwd())

# 1.3 Safe CSV read helper ----------------------------------------------------
safe_read <- function(file) {
  # Matches read.csv defaults from class, but robust NA handling
  read.csv(file, na.strings = c("", "NA", "N/A"," NA", "NA "), stringsAsFactors = FALSE)
}

# 1.4 Read the accident CSV ------------------------------------------------
totacts <- safe_read("Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv")
table(totacts$YEAR)
# ------------------------------

# 1) Filter the data to include only accidents that occurred between the years 2021 and 2024 and name the new dataframe totacts2124.
totacts2124 <- subset(totacts, YEAR >= 2021 & YEAR <= 2024)
#check if that worked
table(totacts2124$YEAR)
#Did it work as intended? (Yes/No)
  #No
#what did the code do Wrong?
  #It took year to be a four digit number, but it is a two digit number
# ------------------------------

# 2) Create a new column in the totacts2124 dataframe that sums TOTINJ + TOTKLD for each accident. Call the column CASINJ.
totacts2124$CASINJ <- totacts2124$TOTINJ + totacts2124$TOTKLD
#check if that worked
head(totacts2124$CASINJ)
#Did it work as intended? (Yes/No)
  #No
#what did the code do Wrong?
  #The output of running head(totacts2124$CASINJ) is integer(0) so it did not work

# Convert the two-digit IYR year column to a four-digit year column. Call the new column YEAR.
totacts2124$YEAR <- ifelse(totacts2124$IYR >= 21, 2000 + totacts2124$IYR, 1900 + totacts2124$IYR)
#check if that worked
table(totacts2124$YEAR)
#Did it work as intended? (Yes/No)
  #NO
#what did the code do Wrong?
  #Printed table of extent 0 so it did not work

#What are the new dimensions of the dataframe totacts2124?
dim(totacts2124)
#Did it work as intended? (Yes/No)
  #Yes
#what did the code do
  # printed 0 145
