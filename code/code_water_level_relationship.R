#27/2/25
#plotting water level data

library(tidyverse)

#load data
waterlevelp1<-read.csv("data/raw/csv/waterlevels_1980-1992.csv")
waterlevelp2<-read.csv("data/raw/csv/waterlevels_1993-2007.csv")

#changing + matching names 
waterlevelp1<-waterlevelp1 %>% rename(waterlevel.masl=Loch.Level.m.asl) %>% 
  mutate(Date=dmy(Date)) %>% select(Date,waterlevel.masl)

waterlevelp2<-waterlevelp2 %>% rename(waterlevel.masl=Harbour.masl) %>% 
  mutate(Date=dmy(Date)) %>% select(Date,waterlevel.masl)

#checking data
View(waterlevelp1)
View(waterlevelp2)

fullwaterlevel<-bind_rows(waterlevelp1,waterlevelp2)  %>% 
  mutate(year = year(Date),  # Extract year
  month = month(Date))

View(fullwaterlevel)

#averaging across months 
WL_monthly_average<- fullwaterlevel %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup()%>%
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-")))  # Create a proper date format

View(WL_monthly_average)

#plot water level time series
plot_WL<-ggplot(WL_monthly_average, aes(x = YearMonth, y = waterlevel.masl)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Water Level Time Series",
       x = "Year-Month",
       y = "Water Level (Average)") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
  theme_classic()  # Clean theme

plot_WL

ggsave("output/timeseries/waterlevel.png",plot_WL,width=16,height=8,dpi=450)

#plot relationship between chlorophyll and water level
RB5_monthly_average<-read.csv("data/raw/csv/RB5_monitoring_data_monthly_average_1980-2023.csv")

#checking data 
View(RB5_monthly_average)

WL_ChlA_data<- full_join(WL_monthly_average, RB5_monthly_average, by = "YearMonth")

View(WL_ChlA_data)

plot_WLChlA<-ggplot(WL_ChlA_data, aes(x = waterlevel.masl, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Water Level vs Chlorophyll A",
       x = "WL (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_WLChlA

ggsave("output/monitoringdatarelationship/waterlevelChlA.png",plot_WLChlA,width=16,height=8,dpi=450)
