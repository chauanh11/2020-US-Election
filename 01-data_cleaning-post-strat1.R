#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS
# Author: Annie Chau
# Data: November 2, 2020
# Contact: anh.chau@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/anhhc/Desktop/PS3")
raw_data <- read_dta("data/usa_00002.dta.gz")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(stateicp,
         sex, 
         age, 
         race,
         educd,
         ftotinc)

#Converting full state names to abbreviation
reduced_data$stateicp <- state.abb[reduced_data$stateicp]

#### What's next? ####
## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data <- 
  reduced_data %>%
  count(stateicp, age, sex, race, ftotinc, educd) %>%
  group_by(stateicp) %>%
  group_by(age) %>%
  group_by(sex) %>%
  group_by(race) %>%
  group_by(ftotinc) %>%
  group_by(educd)

# Removing ambiguous income status
reduced_data <- 
  reduced_data %>% 
  filter(ftotinc != "9999999")

# Dividing different range of income into groups
#max(reduced_data$ftotinc)
reduced_data$ftotinc <- as.integer(reduced_data$ftotinc)
reduced_data$ftotinc <- cut(reduced_data$ftotinc, c(-1, 49999,99999,249999,4000000), 
                           labels = c("Less than $50,000", "$50,000 - $99,999", "$100,000 - $249,999", "More than $249,999"))

# Remove irrelevant individuals
reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

# Extracting indivuals that are 18+
reduced_data$age <- as.integer(reduced_data$age)
reduced_data <- 
  reduced_data %>% 
  filter(age > 17)

# Creating age groups
reduced_data$age <- cut(reduced_data$age, c(17,23,29,39,49,59,69,100), 
                        labels = c("18-23", "24-29", "30-39", "40-49", "50-59", "60-69", "70+"))

# Grouping similar ethnicity/race
reduced_data$race <- gsub("white", "White", reduced_data$race)
reduced_data$race <- gsub("black/african american/negro", "Black, or African American", reduced_data$race)
reduced_data$race <- gsub("american indian or alaska native", "Native American", reduced_data$race)
reduced_data$race <- gsub("chinese", "Asian", reduced_data$race)
reduced_data$race <- gsub("japanese", "Asian", reduced_data$race)
reduced_data$race <- gsub("other asian or pacific islander", "Asian", reduced_data$race)
reduced_data$race <- gsub("two major races", "Mixed race", reduced_data$race)
reduced_data$race <- gsub("three or more major races", "Mixed race", reduced_data$race)
reduced_data$race <- gsub("other race, nec", "Some other race", reduced_data$race)
reduced_data$race <- as.factor(reduced_data$race)

# Creating education groups
reduced_data$educd <- gsub("no schooling completed", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("nursery school, preschool", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("kindergarten", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 1", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 2", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 3", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 4", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 5", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 6", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 7", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 8", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 9", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 10", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("grade 11", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("12th grade, no diploma", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("No high school diploma0", "No high school diploma", reduced_data$educd)
reduced_data$educd <- gsub("No high school diploma1", "No high school diploma", reduced_data$educd)

reduced_data$educd <- gsub("regular high school diploma", "High school diploma", reduced_data$educd)
reduced_data$educd <- gsub("ged or alternative credential", "Associate’s degree", reduced_data$educd)
reduced_data$educd <- gsub("associate's degree, type not specified", "Associate’s degree", reduced_data$educd)
reduced_data$educd <- gsub("some college, but less than 1 year", "Some post-secondary", reduced_data$educd)
reduced_data$educd <- gsub("1 or more years of college credit, no degree", "Some post-secondary", reduced_data$educd)

reduced_data$educd <- gsub("bachelor's degree", "Bachelor’s degree", reduced_data$educd)
reduced_data$educd <- gsub("doctoral degree", "Graduate degree", reduced_data$educd)
reduced_data$educd <- gsub("professional degree beyond a Bachelor’s degree", "Graduate degree", reduced_data$educd)
reduced_data$educd <- gsub("master's degree", "Graduate degree", reduced_data$educd)
reduced_data$educd <- as.factor(reduced_data$educd)

# Altering gender string
reduced_data$sex <- gsub("male", "Male", reduced_data$sex)
reduced_data$sex <- gsub("feMale", "Female", reduced_data$sex)
reduced_data$sex <- gsub("female", "Female", reduced_data$sex)
reduced_data$sex <- as.factor(reduced_data$sex)

# reduced_data$race %>% table()
# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         