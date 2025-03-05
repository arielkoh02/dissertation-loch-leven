#4/3/25
#plotting seasonal data 

library(tidyverse)

#load data
RB5_monthly_average<-read.csv("data/csv/RB5_monitoring_data_monthly_average_1980-2023.csv")

View(RB5_monthly_average)

# Add a season column

RB5_seasonal_average <- RB5_monthly_average %>%
  mutate(
    season = case_when(
      month %in% c(12) ~ "Winter",    # December is Winter
      month %in% c(1, 2) ~ "Winter",  # January & February are also Winter
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    ),
    season_year = if_else(month == 12, year + 1, year)  # Shift December’s year forward
  ) %>%
  group_by(season_year, season) %>%
  summarise(across(starts_with("RB5"), mean, na.rm = TRUE), .groups = "drop")  # Averages all RB5.* columns

# Assign numeric months for correct date alignment
RB5_seasonal_average <- RB5_seasonal_average %>%
  mutate(
    month_for_plot = case_when(
      season == "Winter" ~ 1,   # Assign January as representative of Winter
      season == "Spring" ~ 4,   # April for Spring
      season == "Summer" ~ 7,   # July for Summer
      season == "Autumn" ~ 10   # October for Autumn
    ),
    date_for_plot = as.Date(paste(season_year, month_for_plot, "01", sep = "-")) # Create valid Date
    ) #%>%
      #filter(date_for_plot >= as.Date("2004-01-01") & date_for_plot <= as.Date("2017-01-01"))  # Correct filtering
View(RB5_seasonal_average)

# Plot seasonal ChlA over time grouped by season
plot_seasonalchla<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.ChlA`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Chlorophyll-a Trends",
       x = "Year",
       y = "Chlorophyll-a (RB5 ChlA)",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")
plot_seasonalchla

ggsave("output/seasonaltimeseries/ChlA.png",plot_seasonalchla,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries2004-2016/ChlA.png",plot_seasonalchla,width=16,height=8,dpi=450)

# Plot seasonal DO over time grouped by season
plot_seasonalDO<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.DO`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Disolved Oxygen Trends",
       x = "Year",
       y = "Dissolved Oxygen",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")
plot_seasonalDO

ggsave("output/seasonaltimeseries/DO.png",plot_seasonalDO,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries2004-2016/DO.png",plot_seasonalDO,width=16,height=8,dpi=450)

# Plot seasonal DO over time grouped by season
plot_seasonalNO3<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.NO3`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Nitrate Trends",
       x = "Year",
       y = "Nitrate",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

plot_seasonalNO3

ggsave("output/seasonaltimeseries/NO3.png",plot_seasonalNO3,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries2004-2016/NO3.png",plot_seasonalNO3,width=16,height=8,dpi=450)

# Plot seasonal SD over time grouped by season
plot_seasonalSD<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.SD`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Secchi Depth Trends",
       x = "Year",
       y = "Secchi Depth",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

plot_seasonalSD

ggsave("output/seasonaltimeseries/SD.png",plot_seasonalSD,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries/SD.png",plot_seasonalSD,width=16,height=8,dpi=450)

# Plot seasonal SRP over time grouped by season
plot_seasonalSRP<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.SRP`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Soluble Reactive Phosphorus Trends",
       x = "Year",
       y = "Soluble Reactive Phosphorus",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

plot_seasonalSRP

ggsave("output/seasonaltimeseries/SRP.png",plot_seasonalSRP,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries2004-2016/SRP.png",plot_seasonalSRP,width=16,height=8,dpi=450)

# Plot seasonal SRSi over time grouped by season
plot_seasonalSRSi<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.SRSi`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Soluble Reactive Silica Trends",
       x = "Year",
       y = "Soluble Reactive Silica",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

plot_seasonalSRSi

ggsave("output/seasonaltimeseries/SRSi.png",plot_seasonalSRSi,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries2004-2016/SRSi.png",plot_seasonalSRSi,width=16,height=8,dpi=450)

# Plot seasonal temp over time grouped by season
plot_seasonaltemp<-ggplot(RB5_seasonal_average, aes(x = date_for_plot, y = `RB5.Temp`)) +
  geom_line(size = 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Temperature Trends",
       x = "Year",
       y = "Temperature",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

plot_seasonaltemp

ggsave("output/seasonaltimeseries/temp.png",plot_seasonaltemp,width=16,height=8,dpi=450)
#ggsave("output/seasonaltimeseries2004-2016/temp.png",plot_seasonaltemp,width=16,height=8,dpi=450)
