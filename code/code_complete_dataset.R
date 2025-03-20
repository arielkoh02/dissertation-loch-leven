#6/3/25
#combining the datasets to get a full dataset for easy use 

rm(list=ls())

library(tidyverse)

#load monitoring data
RB5_monitoring_data<-read.csv("data/csv/RB5_monitoring_data_monthly_average_1980-2023.csv")
View(RB5_monitoring_data)

RB5_monitoring_data<-RB5_monitoring_data %>% filter(between(year,2004,2016)) %>% 
  select(-RB5.Cond,-RB5.NO2,-RB5.NH4,-RB5.PP,-RB5.PSiO2,- RB5.SURP,-RB5.TSi,-RB5.TSP,-RB5.TON) %>% 
  mutate(YearMonth=as.Date(YearMonth))

#load biovol data
biovol_fulldata<-read.csv("data/csv/biovolfulldata_2004-2016.csv")
biovol_fulldata<-biovol_fulldata %>% mutate(YearMonth=as.Date(YearMonth))

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

#fill in the blank with new temp and sd data
tempsddata<-read.csv("data/csv/Temp_SD_2004-2016.csv")

tempsddata<- tempsddata %>%  mutate(Date = dmy(Date), RB5.SD = as.numeric(RB5.SD), RB5.Temp = as.numeric(RB5.Temp)) %>% 
  mutate(year = year(Date),  # Extract year
         month = month(Date)) %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup()%>%
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-")))

View(tempsddata)

#fill in blanks for pH data 2010-2016
pHdata<-read.csv("data/csv/pH_2010-2016.csv")

pHdata<-pHdata %>% select(Date,Value) %>% 
  mutate(Date=dmy(Date)) %>% 
  rename(RB5.pH=Value) %>% 
  mutate(year = year(Date),  # Extract year
         month = month(Date)) %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup()%>%
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-")))

View(pHdata)

#fill in blanksn for DO 2010-2016
DOdata<-read.csv("data/csv/DO_2010-2016.csv")

DOdata<- DOdata %>% 
  filter(Units=="%") %>% 
  select(Date,Value) %>% 
  mutate(Date=dmy(Date)) %>% 
  rename(RB5.DO=Value) %>% 
  mutate(year = year(Date),  # Extract year
         month = month(Date)) %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup()%>%
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-")))


View(DOdata)

#start combining everything
all_data1 <- full_join(
  RB5_monitoring_data,  
  select(predator_data, YearMonth, Daphnia),  
  by = "YearMonth"
)

View(all_data1)

all_data2 <- full_join(
  biovol_fulldata,  
  select(fullwaterlevel, YearMonth, waterlevel.masl),  
  by = "YearMonth"
)

View(all_data2)

all_data <- full_join(
  all_data2,  
  all_data1,  
  by = "YearMonth"
)

all_data <- all_data %>%
  left_join(tempsddata, by = "YearMonth") %>% 
  mutate(
    RB5.SD = coalesce(RB5.SD.x, RB5.SD.y)) %>%  # Fill missing values
  mutate( RB5.Temp = coalesce(RB5.Temp.x, RB5.Temp.y)
  ) %>% 
  select(-RB5.SD.x, -RB5.SD.y, -RB5.Temp.x, -RB5.Temp.y)  # Remove duplicates


all_data <- all_data %>%
  left_join(pHdata, by = "YearMonth") %>% 
  mutate(
    RB5.pH = coalesce(RB5.pH.x, RB5.pH.y)) %>%  # Fill missing values
  select(-RB5.pH.x, -RB5.pH.y)

all_data <- all_data %>%
  left_join(DOdata, by = "YearMonth") %>% 
  mutate(
    RB5.DO = coalesce(RB5.DO.x, RB5.DO.y)) %>%  # Fill missing values
  select(-RB5.DO.x, -RB5.DO.y)


View(all_data)

#clean data set
all_data<-all_data %>%  distinct(YearMonth, .keep_all = TRUE) %>%   # Keep only one row per date 
  select(YearMonth,year.y,month.y, Cryto.Biovolume, Cyano.Biovolume,
                              Diatoms.Biovolume, Greens.Biovolume,
                              waterlevel.masl,Daphnia,starts_with("RB5")) %>% 
  rename(Year=year.y,Month=month.y) %>% 
  mutate(
    Season = case_when(
      Month %in% c(12) ~ "Winter",    # December is Winter
      Month %in% c(1, 2) ~ "Winter",  # January & February are also Winter
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Autumn"
    ))%>%
  mutate(Total.Biovolume=Cryto.Biovolume+ Cyano.Biovolume+
                          Diatoms.Biovolume+ Greens.Biovolume) %>% 
  relocate(Season, .after = Month) %>% relocate(Total.Biovolume, .after = Greens.Biovolume)


View(all_data)

write.csv(all_data,"data/csv/alldata-2004-2016.csv")

write.csv(all_data,"data/csv/alldata-2004-2016v2.csv")
