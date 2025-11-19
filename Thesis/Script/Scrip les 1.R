# In this script, we will demonstrate how to load a dataset,
# examine the structure of the data, and perform basic data manipulation in R.

# load the necessary library
# install.packages("dplyr") 
library(dplyr) # Load dplyr for data manipulation


# you can directly read the dataset (CSV/Excel) from the git repository
# data <- read.csv("https://github.com/KKural/crimsyndata/blob/main/csv/fear_of_crime_survey.csv")

# From the easy way, downlaod the csv or excel from git repo and save in your data folder 
# Then read the data from your local machine
data <- read.csv("data/fear_of_crime_survey.csv") # adjust the path as necessary

# if you want to read excel, save in your data folder and use readxl package
# install.packages("readxl") 
# library(readxl) # to read Excel files
# data <- read_excel("data/fear_of_crime_survey.xlsx") # adjust the path as necessary

# you can also read data from other formats, e.g., spss
# install.packages("haven") 
# library(haven) # to read SPSS files
# data <- read_spss("path/to/your/dataset.sav")

# or you can load the data using crimsyncdata package
# install.packages("crimsyncdata")  
# library(crimsyncdata) # to load datasets from crimsyncdata package
# data <- crimsyncdata::fear_of_crime_survey

# Examine the structure of the dataset
str(data)
# View the first few rows of the dataset
head(data)
# view the glimpse of the dataset
glimpse(data)
# view of full data set
View(data)
# Get summary statistics of the dataset
summary(data)

# re- code "feel_safe_day" variable: 1 = 'Very Unsafe', 2 = 'Unsafe', 
# 3 = 'Neutral', 4 = 'Safe', 5 = 'Very Safe'
data <- data |>
  mutate(feel_safe_day_recoded = recode(feel_safe_day   ,
                            '1' = 'Very Unsafe',
                            '2' = 'Unsafe',
                            '3' = 'Neutral',
                            '4' = 'Safe',
                            '5' = 'Very Safe'))

# re- code "feel_safe_night" variable: 1 = 'Very Unsafe', 2 = 'Unsafe', 
# 3 = 'Neutral', 4 = 'Safe', 5 = 'Very Safe'
data <- data |>
  mutate(feel_safe_night_recoded = recode(feel_safe_night   ,
                                    '1' = 'Very Unsafe',
                                    '2' = 'Unsafe',
                                    '3' = 'Neutral',
                                    '4' = 'Safe',
                                    '5' = 'Very Safe'))
# arrange the data, age, gender, income, feel_safe_day_recoded, feel_safe_night_recoded and rest
data <- data|>
  select(age, gender, income, feel_safe_day_recoded, feel_safe_night_recoded, everything())

# view the updated data
View(data)

# we always calculate min, max, mean, median and sd for ratio scale
# it is also called as continuous variables or numeric variables
# example (age, income, no of arrest, no of conviction etc, anything that is in the actual number format)
# lets Table 1 with min, max, mean, median and sd of age and income 

# Create a proper descriptive statistics table with age and income as rows
table1 <- data.frame(
  Variable = c("Age", "Income"),
  Min = c(min(data$age, na.rm = TRUE), min(data$income, na.rm = TRUE)),
  Max = c(max(data$age, na.rm = TRUE), max(data$income, na.rm = TRUE)),
  Mean = c(mean(data$age, na.rm = TRUE), mean(data$income, na.rm = TRUE)),
  Median = c(median(data$age, na.rm = TRUE), median(data$income, na.rm = TRUE)),
  SD = c(sd(data$age, na.rm = TRUE), sd(data$income, na.rm = TRUE))
)

# Round numeric values to 2 decimal places for better presentation
table1[, 2:6] <- round(table1[, 2:6], 2)

# view the table1
print(table1)
