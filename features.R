# Load libraries
library(data.table)
library(lubridate)

# Read data
train <- fread("./project/volume/data/raw/Stat_380_train.csv")
test <- fread("./project/volume/data/raw/Stat_380_test.csv")
ex_sub <- fread("./project/volume/data/raw/Example_sub.csv")

# Set seed
set.seed(123)

# Encode train and test
train$train <- 1
test$train <- 0
test$ic50_Omicron <- 0

# Merge train and test into master
master <- rbind(train, test)

# Count rows
master <- cbind(master, sort_col = seq_len(nrow(master)))

# Transform blanks to NA
master <- replace(master, master == "", NA)

# Encode column dose_3
master$dose_3[!is.na(master$dose_3)] <- 1
master$dose_3[is.na(master$dose_3)] <- 0

# Set master key
setkey(master, sample_id)

# Separate back into train and test
train <- master[train == 1][order(sort_col)]
test <- master[train == 0][order(sort_col)]

# Filter out unnecessary columns
train <- subset(train, select = -c(sample_id, train, sort_col))
test <- subset(test, select = -c(sample_id, train, sort_col))

# Encode ic50_Omicron in ex_sub
ex_sub$ic50_Omicron <- 0

# Write data to interim
fwrite(train, "./project/volume/data/interim/train.csv")
fwrite(test, "./project/volume/data/interim/test.csv")
fwrite(ex_sub, "./project/volume/data/interim/ex_sub.csv")