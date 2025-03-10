#10/3/25
#cleaning predator dataset - plot against biomass

library(tidyverse)

predator_data<-read.csv("data/csv/crustaceanzooplankton_1980-2023.csv")

predator_data<-predator_data %>% select(Date,Daphnia.hyalina.ind.L) %>% 
  mutate(Date=dmy(Date),
         year = year(Date),  # Extract year
         month = month(Date)) %>% 
  filter(!is.na(Date)) %>%
  mutate(Daphnia= as.numeric(Daphnia.hyalina.ind.L)) %>% 
  select(-Daphnia.hyalina.ind.L)

View(predator_data)

#monthly average
average_predator <- predator_data %>% 
  group_by(year, month) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Average numeric cols
  ungroup() %>%  
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-"))  # Create a proper date format
  ) %>%
  filter(YearMonth>= as.Date("2004-01-01") & YearMonth <= as.Date("2016-12-01"))

View(average_predator)

#seasonal average 
seasonal_predator<-average_predator %>% 
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
  summarise(across(Daphnia, mean, na.rm = TRUE), .groups = "drop") 

seasonal_predator<- seasonal_predator %>% 
  mutate(
    month_for_plot = case_when(
      season == "Winter" ~ 1,   # Assign January as representative of Winter
      season == "Spring" ~ 4,   # April for Spring
      season == "Summer" ~ 7,   # July for Summer
      season == "Autumn" ~ 10   # October for Autumn
    ),
    date_for_plot = as.Date(paste(season_year, month_for_plot, "01", sep = "-")) # Create valid Date
  ) 

View(seasonal_predator)

#plotting seasonal trend
plot_seasonalpredator<-ggplot(seasonal_predator, aes(x = date_for_plot, y = `Daphnia`)) +
  geom_line(linewidth= 0.5, colour='grey') +  # Line plot for trends
  geom_point(size = 2, aes(colour=season)) +   # Points for individual data
  labs(title = "Seasonal Daphnia Trends",
       x = "Year",
       y = "Daphnia",
       color = "Season") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")
plot_seasonalpredator

ggsave("output/seasonaltimeseries/predator.png",plot_seasonalchla,width=16,height=8,dpi=450)

