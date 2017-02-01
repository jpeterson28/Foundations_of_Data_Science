# Foundations of Data Science - Data Wrangling Exercise 1
# John Peterson 1/11/17

library(dplyr)
library(tidyr)
library(magrittr)
library(psych)

# Convert to tbl class
refine <- tbl_df(refine_original)

# Transform brand names
refine$company <- ifelse(refine$company %in% c("Phillips", "philips", "phillipS", "phillps", "fillips", "phlips", "phillips", "phllips"), "philips",refine$company)
refine$company <- ifelse(refine$company %in% c("akzo", "Akzo", "AKZO", "akz0", "ak zo"), "akzo",refine$company)
refine$company <- ifelse(refine$company %in% c("Van Houten", "van Houten","van houten"), "van_houten",refine$company)
refine$company <- ifelse(refine$company %in% c("unilver", "Unilever"), "unilever",refine$company)

# Seperate Product Code and Number
refine <- separate(refine, "Product code / number", c("product_code", "product_number"))

# Add Product Categories
refine <- refine %>% mutate(product_category = ifelse(refine$product_code == "p", "smartphone", ifelse(refine$product_code =="v", "tv", ifelse(refine$product_code == "x", "laptop", ifelse(refine$product_code == "q", "tablet", NA)))))

# Concatenate address into 1 column
refine <- refine %>%
              unite(full_address, address, city, country, sep = ',')

# Create dummy variables for company and product category
refine_company <- dummy.code(refine$company)
colnames(refine_company) <- c("company_philips", "company_akzo", "company_van_houten", "company_unilever")
refine_product <- dummy.code(refine$product_category)
colnames(refine_product) <- c("product_smartphone", "product_tv", "product_laptop", "product_tablet")
refine <- refine %>% 
            cbind(refine_company, refine_product)

# Write cleaned table to csv file
write.csv(refine, file = "~/Desktop/Foundations of Data Science/Data Wrangling/Exercise 1/refine_clean.csv")

