#7/2/25
#plotting raw time series data 

#library 
library(tidyverse)
#install.packages("tidyverse")

#loading data
monitoring_data<-read.csv("data/raw/csv/monitoring_data_1980-2023.csv")

#checking data
View(monitoring_data)

#changing the date format
monitoring_data<-monitoring_data %>% 
  mutate(Expr1=dmy(Expr1),
         year = year(Expr1),  # Extract year
         month = month(Expr1))

View(monitoring_data)

#filtering data for only RB5 data
RB5_only <- monitoring_data %>%
  filter(!is.na(Expr1) & Expr1 >as.Date("1986-04-07")) %>%   # Keep rows where Expr1 is not NA
  select(Expr1, month, year, starts_with("RB5"))   # Keep Expr1 and columns starting with RB5

#checking data
View(RB5_only)

#averaging across months 
RB5_monthly_average<- RB5_only %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup()

#checking data 
View(RB5_monthly_average)

