# Foundations of Data Science 
# Data Wrangling Exercise 2
# John Peterson 1/15/17

library(dplyr)
library(tidyr)
library(magrittr)
library(psych)

titanic_1 <- read.csv("titanic_original.csv", header = T, na.strings = c(""," ", "NA"), stringsAsFactors = FALSE)
titanic_1 <-tbl_df(titanic_1)
titanic_1$body <- ifelse(titanic_1$body %in% NA, "NA", titanic_1$body)
titanic_1$home.dest <- ifelse(titanic_1$home.dest %in% NA, "NA", titanic_1$home.dest)

# Fill im missing values in the embarked column with S for Southampton
titanic_1$embarked <- ifelse(titanic_1$embarked %in% NA, "S", titanic_1$embarked)

# Calculate mean age of column and use to fill in missing values
mean_age <- mean(titanic_1$age, na.rm = TRUE)
titanic_1$age <- ifelse(titanic_1$age %in% NA, mean_age, titanic_1$age)

# Fill boat column blanks with None or NA string
titanic_1$boat <- ifelse(titanic_1$boat %in% NA, "NA", titanic_1$boat)

# Create new column has_cabin_number and populate based on if cabin number is present
titanic_1$cabin <- ifelse(titanic_1$cabin %in% NA, "NA", titanic_1$cabin)
titanic_1 <- titanic_1 %>% mutate(has_cabin_number = ifelse(titanic_1$cabin == "NA", 0, 1))

# Write cleaned table to csv file
write.csv(titanic_1, file = "~/Desktop/Foundations of Data Science/Data Wrangling/Exercise 2/titanic_clean.csv")