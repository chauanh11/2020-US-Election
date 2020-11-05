#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Voter Study Group and UCLA Nationscape
# Author: Annie Chau
# Data: November 2, 2020
# Contact: anh.chau@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/anhhc/Desktop/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data2 <- read_dta("data/ns20200625.dta")

# Add the labels
raw_data2 <- labelled::to_factor(raw_data2)

# Just keep some variables
reduced_data2 <- 
  raw_data2 %>% 
  select(vote_intention,
         vote_2020,
         ideo5,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age)

# Remove individuals that are not eligible to vote or will not vote
reduced_data2 <- 
  reduced_data2 %>% 
  filter(vote_intention != "No, I am not eligible to vote")

# Creating age groups 
reduced_data2$age <- as.integer(reduced_data2$age)
reduced_data2$age <- cut(reduced_data2$age, c(17,23,29,39,49,59,69,95), 
                        labels = c("18-23", "24-29", "30-39", "40-49", "50-59", "60-69", "70+"))

# modifying household income groups
reduced_data2$household_income <- as.character(reduced_data2$household_income)
reduced_data2$household_income[reduced_data2$household_income == "Less than $14,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$15,000 to $19,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$20,000 to $24,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$25,000 to $29,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$30,000 to $34,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$35,000 to $39,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$40,000 to $44,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$45,000 to $49,999"] <- "Less than $50,000"
reduced_data2$household_income[reduced_data2$household_income == "$50,000 to $54,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$55,000 to $59,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$60,000 to $64,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$65,000 to $69,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$70,000 to $74,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$75,000 to $79,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$80,000 to $84,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$85,000 to $89,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$90,000 to $94,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$95,000 to $99,999"] <- "$50,000 - $99,999"
reduced_data2$household_income[reduced_data2$household_income == "$100,000 to $124,999"] <- "$100,000 - $249,999"
reduced_data2$household_income[reduced_data2$household_income == "$125,000 to $149,999"] <- "$100,000 - $249,999"
reduced_data2$household_income[reduced_data2$household_income == "$150,000 to $174,999"] <- "$100,000 - $249,999"
reduced_data2$household_income[reduced_data2$household_income == "$175,000 to $199,999"] <- "$100,000 - $249,999"
reduced_data2$household_income[reduced_data2$household_income == "$200,000 to $249,999"] <- "$100,000 - $249,999"
reduced_data2$household_income[reduced_data2$household_income == "$250,000 and above"] <- "More than $249,999"
reduced_data2$household_income <- as.factor(reduced_data2$household_income)

# Grouping certain ethnicities into race
reduced_data2$race_ethnicity <- as.character(reduced_data2$race_ethnicity)
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Asian Indian)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Chinese)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Filipino)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Other)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Japanese)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Korean)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Asian (Vietnamese)"] <- "Asian"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "American Indian or Alaska Native"] <- "Native American"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Pacific Islander (Native Hawaiian)"] <- "Native American"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Pacific Islander (Other)"] <- "Native American"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Pacific Islander (Samoan)"] <- "Native American"
reduced_data2$race_ethnicity[reduced_data2$race_ethnicity == "Pacific Islander (Guamanian)"] <- "Native American"
reduced_data2$race_ethnicity <- as.factor(reduced_data2$race_ethnicity)

# Creating groups for education levels
reduced_data2$education <- as.character(reduced_data2$education)
reduced_data2$education[reduced_data2$education == "3rd Grade or less"] <- "No high school diploma"
reduced_data2$education[reduced_data2$education == "Middle School - Grades 4 - 8"] <- "No high school diploma"
reduced_data2$education[reduced_data2$education == "Completed some high school"] <- "No high school diploma"
reduced_data2$education[reduced_data2$education == "High school graduate"] <- "High school diploma"
reduced_data2$education[reduced_data2$education == "Other post high school vocational training"] <- "Some post-secondary"
reduced_data2$education[reduced_data2$education == "Completed some college, but no degree"] <- "Some post-secondary"
reduced_data2$education[reduced_data2$education == "Associate Degree"] <- "Associate’s degree"
reduced_data2$education[reduced_data2$education == "College Degree (such as B.A., B.S.)"] <- "Bachelor’s degree"
reduced_data2$education[reduced_data2$education == "Completed some graduate, but no degree"] <- "Bachelor’s degree"
reduced_data2$education[reduced_data2$education == "Masters degree"] <- "Graduate degree"
reduced_data2$education[reduced_data2$education == "Doctorate degree"] <- "Graduate degree"
reduced_data2$education <- as.factor(reduced_data2$education)

#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

reduced_data2 <-
  reduced_data2 %>%
  mutate(vote_trump = 
           ifelse(vote_2020 =="Donald Trump", 1, 0))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data2, "outputs/survey_data.csv")

