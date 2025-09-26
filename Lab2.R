# Directories
traindir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Data/"
outdir <- paste0(traindir, "lab2/")

# Load packages
library(readr)
library(dplyr)

# Read in dataset
train <- read_csv(paste0(traindir, "Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv"))

# Confirm YEAR column exists
unique(train$YEAR)

# Create subfolder for yearly files if it doesn't exist
dir.create(outdir, showWarnings = FALSE)

# Split by YEAR and write each file
train %>%
  group_by(YEAR) %>%
  group_split() %>%
  lapply(function(df) {
    yr <- unique(df$YEAR)
    outpath <- paste0(outdir, "train_", yr, ".csv")
    write_csv(df, outpath)
  })

# List all new CSV files
files <- list.files(outdir, full.names = TRUE)

# Get file sizes
sizes <- file.info(files)$size
names(sizes) <- basename(files)

# Extract year numbers from filenames
years <- as.numeric(gsub("[^0-9]", "", names(sizes)))

#####13
library(ggplot2)
library(dplyr)

# Filter non-missing EQPDMG and keep years 01 and above
train_filtered <- train %>%
  filter(!is.na(EQPDMG), YEAR >= "01", YEAR <= "25")   # character comparison works

# Get all year levels present
years_to_plot <- sort(unique(train_filtered$YEAR))

# Boxplot
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = EQPDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Equipment Damage by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "Equipment Damage (EQPDMG)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####14
#KLD Box Plot
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = TOTKLD)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "killed by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "Killed (TOTKLD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####15
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = TRKDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "damage by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "DMG (TRKDMG)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####16
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = TOTINJ)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "inj by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "INJ (TOTINJ)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####17
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = CARSDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "carsdmg by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "carsdmg (CARSDMG)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####18
max_injury_incident <- train %>%
  filter(!is.na(TOTINJ)) %>%          # remove rows with missing injuries
  filter(TOTINJ == max(TOTINJ)) %>%   # keep only the row(s) with maximum TOTINJ
  select(INCDTNO, TOTINJ)             # keep incident number and total injuries

max_injury_incident

####19
library(tidyr)
# Filter accidents from 2021+ (YEAR >= "21") and select variables of interest
train_2021 <- train %>%
  filter(YEAR >= "21") %>%
  select(TONS, TOTINJ, TEMP)

# Reshape data to long format for easier plotting
train_long <- train_2021 %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# Generate QQ plots faceted by variable
ggplot(train_long, aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "QQ Plots of TONS, TOTINJ, TEMP (2021+ Accidents)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

####20
# Directories
traindir <- "C:/Users/axr2yh/OneDrive - University of Virginia/SYS 3501/Data/"
outdir <- paste0(traindir, "lab2/")

# Load packages
library(readr)
library(dplyr)

# Read in dataset
train <- read_csv(paste0(traindir, "Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv"))

# Confirm YEAR column exists
unique(train$YEAR)

# Create subfolder for yearly files if it doesn't exist
dir.create(outdir, showWarnings = FALSE)

# Split by YEAR and write each file
train %>%
  group_by(YEAR) %>%
  group_split() %>%
  lapply(function(df) {
    yr <- unique(df$YEAR)
    outpath <- paste0(outdir, "train_", yr, ".csv")
    write_csv(df, outpath)
  })

# List all new CSV files
files <- list.files(outdir, full.names = TRUE)

# Get file sizes
sizes <- file.info(files)$size
names(sizes) <- basename(files)

# Extract year numbers from filenames
years <- as.numeric(gsub("[^0-9]", "", names(sizes)))

#####13
library(ggplot2)
library(dplyr)

# Filter non-missing EQPDMG and keep years 01 and above
train_filtered <- train %>%
  filter(!is.na(EQPDMG), YEAR >= "01", YEAR <= "25")   # character comparison works

# Get all year levels present
years_to_plot <- sort(unique(train_filtered$YEAR))

# Boxplot
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = EQPDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Equipment Damage by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "Equipment Damage (EQPDMG)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####14
#KLD Box Plot
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = TOTKLD)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "killed by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "Killed (TOTKLD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####15
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = TRKDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "damage by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "DMG (TRKDMG)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####16
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = TOTINJ)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "inj by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "INJ (TOTINJ)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####17
ggplot(train_filtered, aes(x = factor(YEAR, levels = years_to_plot), y = CARSDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "carsdmg by Year (2001+ as '01')",
       x = "Year (2-digit)",
       y = "carsdmg (CARSDMG)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####18
max_injury_incident <- train %>%
  filter(!is.na(TOTINJ)) %>%          # remove rows with missing injuries
  filter(TOTINJ == max(TOTINJ)) %>%   # keep only the row(s) with maximum TOTINJ
  select(INCDTNO, TOTINJ)             # keep incident number and total injuries

max_injury_incident

####19
library(tidyr)
# Filter accidents from 2021+ (YEAR >= "21") and select variables of interest
train_2021 <- train %>%
  filter(YEAR >= "21") %>%
  select(TONS, TOTINJ, TEMP)

# Reshape data to long format for easier plotting
train_long <- train_2021 %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# Generate QQ plots faceted by variable
ggplot(train_long, aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "QQ Plots of TONS, TOTINJ, TEMP (2021+ Accidents)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#####20
library(ggplot2)
library(dplyr)

# Filter non-missing TEMP and accidents from 2001 onward
train_temp <- train %>%
  mutate(TEMP = as.numeric(TEMP)) %>%
  filter(YEAR >= "01", !is.na(TEMP))

# Compute number of bins using Sturges' rule
num_bins <- nclass.Sturges(train_temp$TEMP)

# Plot histogram
ggplot(train_temp, aes(x = TEMP)) +
  geom_histogram(bins = num_bins, fill = NA, colour = "steelblue") +
  labs(title = "Histogram of Temperature (TEMP) for Accidents (2001+)",
       x = "Temperature (°F)", y = "Count") +
  theme_minimal()


#####21
# Filter non-missing and numeric
train_corr <- train %>%
  mutate(
    TRKDMG = as.numeric(TRKDMG),
    ACCDMG = as.numeric(ACCDMG)
  ) %>%
  filter(YEAR >= "01")

# Compute correlation
cor_val <- cor(train_corr$TRKDMG, train_corr$ACCDMG,
               use = "complete.obs", method = "pearson")

# Round to 2 significant digits
cor_val_2sf <- signif(cor_val, 5)
print(paste("Correlation TRKDMG vs ACCDMG (2 sig figs):", cor_val_2sf))


####22
library(psych)
library(dplyr)

# Filter 2001+ accidents and convert variables to numeric
train_2001_vars <- train %>%
  mutate(
    TRKDMG = as.numeric(TRKDMG),
    EQPDMG = as.numeric(EQPDMG),
    ACCDMG = as.numeric(ACCDMG),
    TOTINJ = as.numeric(TOTINJ),
    TOTKLD = as.numeric(TOTKLD)
  ) %>%
  filter(YEAR >= "01") %>%
  select(TRKDMG, EQPDMG, ACCDMG, TOTINJ, TOTKLD)

# Create scatter plot matrix with correlations
pairs.panels(train_2001_vars,
             method = "pearson",   # correlation method
             hist.col = "steelblue", # color of histograms
             ellipses = FALSE,      # no confidence ellipses
             lm = TRUE,             # add blue regression lines
             main = "Scatter Plot Matrix: 2001+ Accidents")

####24
library(dplyr)

# Find the accident with the largest ACCDMG
max_accdmg_year <- train %>%
  filter(!is.na(ACCDMG)) %>%            # remove missing values
  filter(ACCDMG == max(ACCDMG)) %>%     # keep only the row(s) with max ACCDMG
  select(YEAR)                           # select the YEAR column

# Print the year
max_accdmg_year


####25
library(dplyr)

# Find the accident with the largest ACCDMG
max_accdmg <- train %>%
  filter(!is.na(ACCDMG)) %>%
  filter(ACCDMG == max(ACCDMG))

# View key columns to answer the question
max_accdmg %>%
  select(INCDTNO, TOTKLD, RAILROAD, TYPE)


####26
library(dplyr)

# Maximum value of ACCDMG
max_accdmg_value <- train %>%
  filter(!is.na(ACCDMG)) %>%     # remove missing values
  summarise(max_value = max(ACCDMG)) %>%
  pull(max_value)

max_accdmg_value

####27
library(dplyr)

# Maximum number of deaths in one accident
max_deaths <- train %>%
  filter(!is.na(TOTKLD)) %>%      # remove missing values
  summarise(max_value = max(TOTKLD)) %>%  # find maximum
  pull(max_value)                          # extract numeric value

max_deaths  # prints the result


####28
library(dplyr)

# Find the year with the maximum deaths
year_max_deaths <- train %>%
  filter(!is.na(TOTKLD)) %>%                  # remove missing values
  filter(TOTKLD == max(TOTKLD)) %>%           # keep row(s) with max deaths
  select(YEAR) %>%                             # select the YEAR column
  pull()                                       # extract as value

year_max_deaths


####29
library(dplyr)

# Count accidents with ACCDMG > 1,500,000
num_high_damage <- train %>%
  filter(!is.na(ACCDMG)) %>%      # remove missing values
  filter(ACCDMG > 1500000) %>%    # filter for damage greater than $1.5M
  summarise(count = n()) %>%      # count rows
  pull(count)

num_high_damage

####30
library(dplyr)

# Count accidents with at least one death
num_accidents_with_deaths <- train %>%
  filter(!is.na(TOTKLD)) %>%    # remove missing values
  filter(TOTKLD >= 1) %>%       # keep accidents with 1 or more deaths
  summarise(count = n()) %>%    # count the rows
  pull(count)

num_accidents_with_deaths

####31
library(dplyr)
library(ggplot2)

# Count number of accidents by TYPE
acc_type_counts <- train %>%
  filter(!is.na(TYPE)) %>%         # remove missing values
  group_by(TYPE) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print counts to see most common type
acc_type_counts

# Create barplot
ggplot(acc_type_counts, aes(x = reorder(TYPE, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of Each Accident Type (All Years)",
       x = "Accident Type",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####32
library(dplyr)
library(ggplot2)

# Count accidents by cause
acc_cause_counts <- train %>%
  filter(!is.na(CAUSE)) %>%       # remove missing values
  group_by(CAUSE) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print counts to see the largest cause
acc_cause_counts

# Keep only the five major causes
major_causes <- c(
  "Train operation - Human Factors",
  "Miscellaneous Causes Not Otherwise Listed",
  "Mechanical and Electrical Failures",
  "Signal and Communication",
  "Track, Roadbed and Structures"
)

acc_major <- acc_cause_counts %>%
  filter(CAUSE %in% major_causes)

# Barplot
ggplot(acc_major, aes(x = reorder(CAUSE, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of Major Causes of Accidents (All Years)",
       x = "Cause of Accident",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(dplyr)
library(ggplot2)

#### Problem 33: Boxplot of total accident damage (ACCDMG) ####
# Filter non-missing ACCDMG
acc_damage <- train %>%
  filter(!is.na(ACCDMG))

# Boxplot
ggplot(acc_damage, aes(x = "", y = ACCDMG)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Problem 33: Boxplot of Total Accident Damage (All Years)",
       x = "",
       y = "Total Accident Damage (ACCDMG)") +
  theme_minimal()

# Compute upper whisker
upper_whisker <- boxplot.stats(acc_damage$ACCDMG)$stats[5]
upper_whisker  # Value of the upper whisker

#### Problem 34: Number of accidents greater than the upper whisker (2001–2023) ####
num_extreme_accidents <- train %>%
  filter(YEAR >= "01", YEAR <= "23", !is.na(ACCDMG)) %>%
  filter(ACCDMG > upper_whisker) %>%
  summarise(count = n()) %>%
  pull(count)

num_extreme_accidents

#### Problem 35: Proportion of accidents that are extreme (all years) ####
total_accidents <- nrow(acc_damage)
extreme_accidents <- acc_damage %>%
  filter(ACCDMG > upper_whisker) %>%
  nrow()

prop_extreme <- round((extreme_accidents / total_accidents) * 100)
prop_extreme  # Percent of extreme accidents

#### Problem 36: Cost proportion of extreme accidents relative to all accidents ####
total_cost <- sum(acc_damage$ACCDMG, na.rm = TRUE)
extreme_cost <- sum(acc_damage$ACCDMG[acc_damage$ACCDMG > upper_whisker], na.rm = TRUE)

cost_proportion <- round((extreme_cost / total_cost) * 100)
cost_proportion  # Percent of total accident cost due to extreme accidents

#### 37
library(dplyr)
library(lubridate)

# Combine YEAR, MONTH, DAY into a Date column
train_dates <- train %>%
  filter(!is.na(ACCDMG)) %>%
  mutate(
    YEAR4 = as.numeric(YEAR),
    MONTH = as.numeric(MONTH),
    DAY = as.numeric(DAY),
    DATE = make_date(year = YEAR4 + ifelse(YEAR4 < 50, 2000, 1900), month = MONTH, day = DAY)
  )

# Extract extreme accidents (ACCDMG > $15M)
extreme_accidents <- train_dates %>%
  filter(ACCDMG > 15000000)

# View extreme accidents
extreme_accidents

# Check for Sept 11, 2001
extreme_accidents %>%
  filter(DATE == as.Date("2001-09-11"))

####38
library(dplyr)

# Select extreme accidents (ACCDMG > upper whisker)
extreme_accidents_subset <- train %>%
  filter(!is.na(ACCDMG)) %>%
  filter(ACCDMG > upper_whisker) %>%
  select(INCDTNO, YEAR, MONTH, DAY, TIMEHR, TIMEMIN)

# Count duplicates
num_duplicates <- extreme_accidents_subset %>%
  duplicated() %>%
  sum()

num_duplicates

