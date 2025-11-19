# In this script, we will demonstrate how to load a dataset,
# examine the structure of the data, and perform basic data manipulation in R.

# load the necessary library
install.packages("dplyr") 
library(dplyr) # Load dplyr for data manipulation


# you can directly read the dataset (CSV/Excel) from the git repository
# data <- read.csv("https://github.com/KKural/crimsyndata/blob/main/csv/fear_of_crime_survey.csv")

# From the easy way, downlaod the csv or excel from git repo and save in your data folder 
# Then read the data from your local machine
data <- read.csv("Data/fear_of_crime_survey.csv") # adjust the path as necessary

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