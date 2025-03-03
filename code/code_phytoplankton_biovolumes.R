#3/3/25
#plotting phytoplankton biovolumes

#library 
library(tidyverse)

#loading data
cryto_data<-read.csv("data/raw/csv/cryto_phyto_2004-2016.csv")
cyano_data<-read.csv("data/raw/csv/cyano_phyto_2004-2016.csv")
diatoms_data<-read.csv("data/raw/csv/diatoms_phyto_2004-2016.csv")
greens_data<-read.csv("data/raw/csv/greens_phyto_2004-2016.csv")

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
  by = "YearMonth"
)
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
