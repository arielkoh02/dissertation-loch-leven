#3/3/25
#plotting phytoplankton biovolumes

rm(list=ls())

#library 
library(tidyverse)

#loading data
cryto_data<-read.csv("data/csv/cryto_phyto_2004-2016.csv")
cyano_data<-read.csv("data/csv/cyano_phyto_2004-2016.csv")
diatoms_data<-read.csv("data/csv/diatoms_phyto_2004-2016.csv")
greens_data<-read.csv("data/csv/greens_phyto_2004-2016.csv")

View(cryto_data)

#clean up the date format in data set
cryto_data<- cryto_data %>% 
  mutate(Date=dmy(Date),
         year = year(Date),  # Extract year
         month = month(Date),
         YearMonth = as.Date(paste(year, month, "01", sep = "-")))

cyano_data<- cyano_data %>% 
  mutate(Date=dmy(Date),
         year = year(Date),  # Extract year
         month = month(Date),
         YearMonth = as.Date(paste(year, month, "01", sep = "-")))

diatoms_data<- diatoms_data %>% 
  mutate(Date=dmy(Date),
         year = year(Date),  # Extract year
         month = month(Date),
         YearMonth = as.Date(paste(year, month, "01", sep = "-")))

greens_data<- greens_data %>% 
  mutate(Date=dmy(Date),
         year = year(Date),  # Extract year
         month = month(Date),
         YearMonth = as.Date(paste(year, month, "01", sep = "-")))

#combine into one dataset
biovol_data1 <- full_join(
  select(cryto_data, YearMonth, year, month, Cryto.Biovolume),  
  select(cyano_data, YearMonth, Cyano.Biovolume),  
  by = "YearMonth"
)
View(biovol_data1)

biovol_data2 <- full_join(
  select(diatoms_data, YearMonth, year, month, Diatoms.Biovolume),  
  select(greens_data, YearMonth, Greens.Biovolume),  
  by = "YearMonth"
)
View(biovol_data2)

biovol_fulldata <- full_join(
  select(biovol_data1, YearMonth, year, month, Cryto.Biovolume, Cyano.Biovolume),  
  select(biovol_data2, YearMonth, Diatoms.Biovolume, Greens.Biovolume),  
  by = "YearMonth")
View(biovol_fulldata)

#convert to long form 
biovol_long <- biovol_fulldata %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume")  # Create new column for values
View(biovol_long)

#plotting data biovolume over time grouped by phytoplankton group
plot_biovol<-ggplot(biovol_long, aes(x = YearMonth, y = Biovolume, color = Phytoplankton_Group)) +
  geom_line(linewidth=0.5) +  # Line plot
  geom_point(size=1) +  # Add points for better visibility
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Format x-axis
  labs(title = "Phytoplankton Biovolume Over Time",
       x = "Year",
       y = "Biovolume",
       color = "Phytoplankton Group") +
  theme_classic() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

plot_biovol

ggsave("output/timeseries/phyto.png",plot_biovol,width=16,height=8,dpi=450)

#data with summed 
biovol_sumdata<- biovol_fulldata %>% 
  mutate(Total.Biovolume = rowSums(select(., ends_with("Biovolume")), na.rm = TRUE))

RB5_monthly_average<-read.csv("data/raw/csv/RB5_monitoring_data_monthly_average_1980-2023.csv")

#RB5_monthly_average<-RB5_monthly_average %>% 
#filter(YearMonth>= as.Date("2004-01-01") & YearMonth <= as.Date("2016-12-01"))

biovol_chla <- full_join(
  select(biovol_sumdata, YearMonth, year, month, Total.Biovolume),  
  select(RB5_monthly_average, YearMonth, RB5.ChlA),  
  by = "YearMonth")

View(biovol_chla)

plot_biovolchla<-ggplot(biovol_chla, aes(x = Total.Biovolume, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Total Biovolume of Phytoplankton vs Chlorophyll A",
       x = "Total Bio (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_biovolchla

ggsave("output/chlaproxy/biovolvschla.png",plot_biovolchla,width=16,height=8,dpi=450)
#ggsave("output/chlaproxy/biovolvschla2004-2016.png",plot_biovolchla,width=16,height=8,dpi=450)

phyto_seasonal_average <- biovol_fulldata %>%
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
  summarise(across(ends_with("Biovolume"), mean, na.rm = TRUE), .groups = "drop")  # Averages all RB5.* columns

# Assign numeric months for correct date alignment
phyto_seasonal_average <- phyto_seasonal_average %>%
  mutate(
    month_for_plot = case_when(
      season == "Winter" ~ 1,   # Assign January as representative of Winter
      season == "Spring" ~ 4,   # April for Spring
      season == "Summer" ~ 7,   # July for Summer
      season == "Autumn" ~ 10   # October for Autumn
    ),
    date_for_plot = as.Date(paste(season_year, month_for_plot, "01", sep = "-")) # Create valid Date
  )%>%
  drop_na(season)

View(phyto_seasonal_average)

#plot seasonal cryto biovol
plot_seasonalcryto <- ggplot(phyto_seasonal_average, aes(x = date_for_plot, y = Cryto.Biovolume)) +
  geom_line(size = 0.5) +  # Line color for each Phytoplankton Group
  geom_point(aes(colour = season), size = 2) +   # Point color based on Season
  labs(title = "Seasonal Cryto Biovolume Trends",
       x = "Year",
       y = "Biovolume") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  guides(color = guide_legend(title = "Season"))

plot_seasonalcryto

#plot seasonal cyano biovol
plot_seasonalcyano <- ggplot(phyto_seasonal_average, aes(x = date_for_plot, y = Cyano.Biovolume)) +
  geom_line(size = 0.5) +  # Line color for each Phytoplankton Group
  geom_point(aes(colour = season), size = 2) +   # Point color based on Season
  labs(title = "Seasonal Cyano Biovolume Trends",
       x = "Year",
       y = "Biovolume") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  guides(color = guide_legend(title = "Season"))

plot_seasonalcyano

#plot seasonal diatoms biovol
plot_seasonaldiatoms <- ggplot(phyto_seasonal_average, aes(x = date_for_plot, y = Diatoms.Biovolume)) +
  geom_line(size = 0.5) +  # Line color for each Phytoplankton Group
  geom_point(aes(colour = season), size = 2) +   # Point color based on Season
  labs(title = "Seasonal Diatoms Biovolume Trends",
       x = "Year",
       y = "Biovolume") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  guides(color = guide_legend(title = "Season"))


plot_seasonaldiatoms

#plot seasonal greens biovol
plot_seasonalgreens <- ggplot(phyto_seasonal_average, aes(x = date_for_plot, y = Greens.Biovolume)) +
  geom_line(size = 0.5) +  # Line color for each Phytoplankton Group
  geom_point(aes(colour = season), size = 2) +   # Point color based on Season
  labs(title = "Seasonal Greens Biovolume Trends",
       x = "Year",
       y = "Biovolume") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  guides(color = guide_legend(title = "Season"))


plot_seasonalgreens

install.packages("ggpubr")
library(ggpubr)

# Arrange in a 2x2 grid
plot_seasonalbiovol<-ggarrange(plot_seasonalcryto, plot_seasonalcyano, plot_seasonaldiatoms, plot_seasonalgreens, nrow = 2, ncol = 2, common.legend = TRUE, legend="bottom")
plot_seasonalbiovol
ggsave("output/seasonaltimeseries/biovol.png",plot_seasonalbiovol,width=16,height=8,dpi=450)
ggsave("output/seasonaltimeseries2004-2016/biovol.png",plot_seasonalbiovol,width=16,height=8,dpi=450)

phyto_long <- phyto_seasonal_average %>%
  select(season, Cryto.Biovolume, Cyano.Biovolume, Diatoms.Biovolume, Greens.Biovolume) %>%
  pivot_longer(cols = -season, names_to = "Phytoplankton_Group", values_to = "Biovolume")

plot_violin <- ggplot(phyto_long, aes(x = season, y = Biovolume, fill = season)) +
  geom_violin() +
  facet_wrap(~Phytoplankton_Group, scales = "free_y") +  # Separate plots per group
  labs(title = "Seasonal Phytoplankton Biovolume Distributions",
       x = "Season",
       y = "Biovolume") +
  theme_classic() +
  scale_fill_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                               "Summer" = "maroon", "Autumn" = "orange")) +
  theme(legend.position = "none") 

plot_violin
ggsave("output/seasonaltimeseries2004-2016/violinbiovol.png",plot_violin,width=16,height=8,dpi=450)



plot_boxplot <- ggplot(phyto_long, aes(x = season, y = Biovolume, fill = season)) +
  geom_boxplot() +
  facet_wrap(~Phytoplankton_Group, scales = "free_y") +  # Separate plots per group
  labs(title = "Seasonal Phytoplankton Biovolume Distributions",
       x = "Season",
       y = "Biovolume") +
  theme_classic() +
  scale_fill_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                               "Summer" = "maroon", "Autumn" = "orange")) +
  theme(legend.position = "none") 

plot_boxplot
ggsave("output/seasonaltimeseries2004-2016/boxplotbiovol.png",plot_violin,width=16,height=8,dpi=450)

