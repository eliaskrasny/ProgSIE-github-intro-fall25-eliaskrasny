#*******************************************************************************
#                             R Basics - Part 2  (base R only)
#*******************************************************************************

#---------------------------
# 0.1 Session info
#---------------------------
sessionInfo()   # R version, platform, and currently loaded packages

#---------------------------
# 0.2 Setup: working with files safely (base R)
#    Tip: prefer file.path() over hard-coded slashes; add basic checks.
#---------------------------

# Current working directory (for reference)
getwd()

# Set directories for easy switching between data and source code 
traindir <- "~/Google Drive/UVA/Courses/ProgSIE/Data/TrainData/"
sourcedir <- "~/Google Drive/UVA/Courses/ProgSIE/Code/"

if (!dir.exists(traindir))  stop("Training data directory not found: ", traindir)
if (!dir.exists(sourcedir)) warning("Source code directory not found: ", sourcedir)

# Move to training data directory and list files
setwd(traindir)
dir()   # list files in traindir

# Go to source code directory (optional) and list files
setwd(sourcedir)
dir()

# Clean up a symbol you no longer need (optional)
rm(sourcedir)

# Set the working directory back to training data
setwd(traindir)
getwd()
dir()

#*******************************************************************************
# 1. Reading data
#*******************************************************************************

#---------------------------
# 1.1 Load accident data (2022)
#    Safer read.csv(): specify NA strings; stringsAsFactors=FALSE (R<4.0)
#---------------------------

# If your R < 4.0, stringsAsFactors defaults to TRUE; set explicitly if needed
accident_data_22 <- read.csv(
  "RailAccidents22.csv",
  na.strings = c("", "NA", "N/A")
)

# Quick sanity checks
if (!is.data.frame(accident_data_22)) stop("Read failed: 'accident_data_22' is not a data.frame.")
if (nrow(accident_data_22) == 0L) warning("File loaded but has zero rows.")

#---------------------------
# 1.2 Explore structure
#---------------------------
head(accident_data_22)        # first 6 rows
dim(accident_data_22)         # rows (observations), columns (variables)
summary(accident_data_22)     # per-column summaries
str(accident_data_22)         # types and a preview

# Subset summary of selected variables & error 
sel_vars <- c("ACCDMG", "TOTKLD", "CARS", "STATION")
present  <- sel_vars %in% names(accident_data_22)
present
if (!all(present)) warning("Missing columns: ", paste(sel_vars[!present], collapse = ", "))
str(accident_data_22[, sel_vars[present], ])

# Column names
colnames(accident_data_22)

#*******************************************************************************
# 2. Univariate summaries (be NA-safe)
#*******************************************************************************

# Numeric examples (total killed)
if ("TOTKLD" %in% names(accident_data_22)) {
  cat("Class(TOTKLD): ", class(accident_data_22$TOTKLD), "\n")
  cat("Var(TOTKLD)  : ", var(accident_data_22$TOTKLD, na.rm = TRUE), "\n")
  cat("Mean(TOTKLD) : ", mean(accident_data_22$TOTKLD, na.rm = TRUE), "\n")
  print(table(accident_data_22$TOTKLD, useNA = "ifany"))
  
  # Rounding examples
  print(mean(accident_data_22$TOTKLD, na.rm = TRUE))
  print(round(mean(accident_data_22$TOTKLD, na.rm = TRUE)))
} else {
  warning("Column 'TOTKLD' not found.")
}

# Categorical examples (station)
if ("STATION" %in% names(accident_data_22)) {
  cat("Class(STATION): ", class(accident_data_22$STATION), "\n")
  # Make sure it's a factor if you want levels()
  if (!is.factor(accident_data_22$STATION)) {
    accident_data_22$STATION <- factor(accident_data_22$STATION)
  }
  print(levels(accident_data_22$STATION))
  print(table(accident_data_22$STATION, useNA = "ifany"))
}

#*******************************************************************************
# 3. Simple questions
#*******************************************************************************

# Total accident damage in 2022
if ("ACCDMG" %in% names(accident_data_22)) {
  total_damage <- sum(accident_data_22$ACCDMG, na.rm = TRUE)
  cat("Total ACCDMG (2022): ", total_damage, "\n")
} else {
  warning("Column 'ACCDMG' not found.")
}

# Total damage from the 5 most expensive accidents
if ("ACCDMG" %in% names(accident_data_22)) {
  x <- accident_data_22$ACCDMG
  # Use order() to get indices of top 5 (robust if N < 5)
  n_top <- min(5, sum(!is.na(x)))
  top_idx <- order(x, decreasing = TRUE, na.last = NA)[seq_len(n_top)]
  sumacc5 <- sum(x[top_idx], na.rm = TRUE)
  cat("Sum of top ", n_top, " ACCDMG values: ", sumacc5, "\n", sep = "")
} 

# Index of max and corresponding row
if ("ACCDMG" %in% names(accident_data_22)) {
  max_val <- max(accident_data_22$ACCDMG, na.rm = TRUE)
  max_i   <- which.max(accident_data_22$ACCDMG)   # returns first max index
  cat("Max ACCDMG value: ", max_val, " at row ", max_i, "\n", sep = "")
  if (!is.na(max_i) && max_i >= 1 && max_i <= nrow(accident_data_22)) {
    print(accident_data_22[max_i, , drop = FALSE])
  }
}

# Filter accidents costing more than $1e6
if ("ACCDMG" %in% names(accident_data_22)) {
  bigacc <- accident_data_22[accident_data_22$ACCDMG > 1e6 & !is.na(accident_data_22$ACCDMG), ]
  cat("Rows with ACCDMG > 1e6: ", nrow(bigacc), "\n")
}

# Save a CSV of "big accidents" (> $1e6) for later use
if (exists("bigacc") && nrow(bigacc) > 0L) {
  write.csv(bigacc, file = "big_accidents_over_1M_2022.csv", row.names = FALSE)
}
