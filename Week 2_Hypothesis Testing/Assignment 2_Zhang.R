# Data Preparation
library(readr)
library(dplyr)
Adoptions <- read.csv("FCR/NEU/CPS/Analytics_2018/ALY 6015_Intermediate Analytics/Week 2/Adoptions_by_SFY__DCF_Office__Gender_and_Length_of_Stay.csv", head(TRUE))

# Aggregate by office and sfy
by_office_sfy <- Adoptions %>% 
  group_by(Office, SFY) %>%
  summarise(LOS_LessThan24Mnths = sum(LOS_LessThan24Mnths), LOS_MoreThan24Mnths = sum(LOS_MoreThan24Mnths))

# Two Sample t-test
t.test(by_office_sfy$LOS_LessThan24Mnths, by_office_sfy$LOS_MoreThan24Mnths, alternative = c("less"))
