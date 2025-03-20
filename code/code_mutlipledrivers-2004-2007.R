#19/3
#smaller dataset of 2004-2007
#linear modelling 

rm=list(ls)

library(tidyverse)
library(lmerTest)
library(DHARMa)

alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")

alldata<-alldata %>% mutate(Total.Biovolume=log(Total.Biovolume)) %>% 
  mutate(YearMonth = as.Date(YearMonth)) %>% 
  filter(YearMonth>= as.Date("2004-01-01") & YearMonth <= as.Date("2007-12-01")) %>% 
  select(-X) %>% arrange(YearMonth)

alldatalong <- alldata %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume") %>% 
  mutate(Biovolume= log(Biovolume)) %>% 
  filter(!(Phytoplankton_Group=='Total.Biovolume'))

View(alldata)
View(alldatalong)


