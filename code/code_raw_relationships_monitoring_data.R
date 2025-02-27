#26/2/25
#plotting relationships using monitoring data

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
  ungroup()%>%
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-")))  # Create a proper date format

#checking data 
View(RB5_monthly_average)

#plot SRP vs ChlA
plot_SRPChlA<-ggplot(RB5_monthly_average, aes(x = RB5.SRP, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Soluble Reactive Phosphorus vs Chlorophyll A",
       x = "SRP (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_SRPChlA

ggsave("output/monitoringdatarelationship/SRPChlA.png",plot_SRPChlA,width=16,height=8,dpi=450)

#plot NO3 vs ChlA
plot_NO3ChlA<-ggplot(RB5_monthly_average, aes(x = RB5.NO3, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Nitrate vs Chlorophyll A",
       x = "NO3 (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_NO3ChlA

ggsave("output/monitoringdatarelationship/NO3ChlA.png",plot_NO3ChlA,width=16,height=8,dpi=450)

#plot temp vs ChlA
plot_tempChlA<-ggplot(RB5_monthly_average, aes(x = RB5.Temp, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Temperature vs Chlorophyll A",
       x = "Temperature (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_tempChlA

ggsave("output/monitoringdatarelationship/tempChlA.png",plot_tempChlA,width=16,height=8,dpi=450)

#plot secchi depth vs ChlA
plot_SDChlA<-ggplot(RB5_monthly_average, aes(x = RB5.SD, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Secchi Depth vs Chlorophyll A",
       x = "Secchi Depth (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_SDChlA

ggsave("output/monitoringdatarelationship/SDChlA.png",plot_SDChlA,width=16,height=8,dpi=450)

#plot SRSi vs ChlA
plot_SRSiChlA<-ggplot(RB5_monthly_average, aes(x = RB5.SRSi, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Soluble Silica vs Chlorophyll A",
       x = "Soluble Silica (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_SRSiChlA

ggsave("output/monitoringdatarelationship/SRSiChlA.png",plot_SRSiChlA,width=16,height=8,dpi=450)

#plot DO vs ChlA
plot_DOChlA<-ggplot(RB5_monthly_average, aes(x = RB5.DO, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Dissolved Oxygen vs Chlorophyll A",
       x = "Dissolved Oxygen (Average)",
       y = "ChlA (Average)") +
  theme_classic()  # Clean theme

plot_DOChlA

ggsave("output/monitoringdatarelationship/DOChlA.png",plot_DOChlA,width=16,height=8,dpi=450)
