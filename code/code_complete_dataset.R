#6/3/25
#combining the datasets to get a full dataset for easy use 

library(tidyverse)

#load monitoring data
RB5_monitoring_data<-read.csv("data/csv/RB5_monitoring_data_monthly_average_1980-2023.csv")
View(RB5_monitoring_data)

RB5_monitoring_data<-RB5_monitoring_data %>% filter(between(year,2004,2016)) %>% 
  select(-RB5.Cond,-RB5.NO2,-RB5.NH4,-RB5.PP,-RB5.PSiO2,- RB5.SURP,-RB5.TSi,-RB5.TSP)

#load biovol data
biovol_fulldata<-read.csv("data/csv/biovolfulldata_2004-2016.csv")
View(biovol_fulldata)

#load predator data
predator_data<-read.csv("data/csv/crustaceanzooplankton_1980-2023.csv")

predator_data<-predator_data %>% select(Date,Daphnia.hyalina.ind.L) %>% 
  mutate(Date=dmy(Date),
         year = year(Date),  # Extract year
         month = month(Date)) %>% 
  filter(!is.na(Date)) %>%
  mutate(Daphnia= as.numeric(Daphnia.hyalina.ind.L)) %>% 
  select(-Daphnia.hyalina.ind.L) %>% 
  group_by(year, month) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup() %>%  
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-"))  # Create a proper date format
  ) %>%
  filter(YearMonth>= as.Date("2004-01-01") & YearMonth <= as.Date("2016-12-01"))
View(predator_data)

#load water level data 
waterlevelp2<-read.csv("data/csv/waterlevels_1993-2007.csv")
waterlevelp3<-read.csv("data/csv/waterlevels_2008-2013.csv")

waterlevelp2<-waterlevelp2 %>% rename(waterlevel.masl=Harbour.masl) %>% 
  mutate(Date=dmy(Date)) %>% select(Date,waterlevel.masl)

waterlevelp3<-waterlevelp3 %>% rename(Date=SAMPLE_DATE,waterlevel.masl=Level.maod) %>% 
  mutate(Date=dmy(Date)) %>% select(Date,waterlevel.masl)

fullwaterlevel<-bind_rows(waterlevelp2,waterlevelp3)  %>% 
  mutate(year = year(Date),  # Extract year
         month = month(Date)) %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup()%>%
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-")))  %>% # Create a proper date format
  filter(between(year,2004,2016))

View(fullwaterlevel)




