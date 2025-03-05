#7/2/25
#plotting raw time series data 

#library 
library(tidyverse)
#install.packages("tidyverse")

#loading data
monitoring_data<-read.csv("data/csv/monitoring_data_1980-2023.csv")

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
  mutate(YearMonth = as.Date(paste(year, month, "01", sep = "-"))  # Create a proper date format
  ) #%>% 
  #filter(YearMonth>= as.Date("2004-01-01") & YearMonth <= as.Date("2016-12-01"))

#checking data 
View(RB5_monthly_average)

#plot ChlA
plot_ChlA<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.ChlA)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Time Series of RB5_ChlA",
       x = "Year-Month",
       y = "Average Chlorophyll A Content") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +  # Format x-axis
  theme_minimal()  # Clean theme

plot_ChlA

ggsave("output/timeseries/chlA.png",plot_ChlA,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/chlA.png",plot_ChlA,width=16,height=8,dpi=450)

#plot SRP
plot_SRP<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.SRP)) +
   geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
   geom_point(colour="grey",size = 1) +  # Add points
   geom_smooth(linetype="dashed",se=F,colour="black")+
   labs(title = "Time Series of Soluble Reactive Phosphorus (1986-2023)",
        x = "Year-Month",
        y = "Average SRP") +
   scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
   theme_classic()  # Clean theme

plot_SRP

ggsave("output/timeseries/SRP.png",plot_SRP,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/SRP.png",plot_SRP,width=16,height=8,dpi=450)

#plot NO3
plot_NO3<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.NO3)) +
   geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
   geom_point(colour="grey",size = 1) +  # Add points
   geom_smooth(linetype="dashed",se=F,colour="black")+
   labs(title = "Time Series of Nitrate Content (1986-2023)",
        x = "Year-Month",
        y = "Average Nitrate Content") +
   scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
   theme_classic()  # Clean theme

 plot_NO3
 
ggsave("output/timeseries/NO3.png",plot_NO3,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/NO3.png",plot_NO3,width=16,height=8,dpi=450)
 
#plot temp
plot_temp<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.Temp)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Time Series of Temperature (1986-2023)",
       x = "Year-Month",
       y = "Average Temperature") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
  theme_classic()  # Clean theme

plot_temp

ggsave("output/timeseries/temp.png",plot_NO3,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/temp.png",plot_NO3,width=16,height=8,dpi=450)

#plot secchi depth
plot_SD<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.SD)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Time Series of Secchi Depth (1986-2023)",
       x = "Year-Month",
       y = "Average Secchi Depth") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
  theme_classic()  # Clean theme

plot_SD

ggsave("output/timeseries/SD.png",plot_NO3,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/SD.png",plot_NO3,width=16,height=8,dpi=450)

#plot soluble reactive silica
plot_SRSi<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.SRSi)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Time Series of Soluble Reactive Silica (1986-2023)",
       x = "Year-Month",
       y = "Average Soluble Reactive Silica") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
  theme_classic()  # Clean theme

plot_SRSi

ggsave("output/timeseries/SRSi.png",plot_SRSi,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/SRSi.png",plot_SRSi,width=16,height=8,dpi=450)

#plot dissolved oxygen
plot_DO<-ggplot(RB5_monthly_average, aes(x = YearMonth, y = RB5.DO)) +
  geom_line(colour="lightblue",linewidth = 0.5) +  # Line plot
  geom_point(colour="grey",size = 1) +  # Add points
  geom_smooth(linetype="dashed",se=F,colour="black")+
  labs(title = "Time Series of Dissolved Oxygen (1986-2023)",
       x = "Year-Month",
       y = "Average Dissolved Oxygen") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +  # Format x-axis
  theme_classic()  # Clean theme

plot_DO

ggsave("output/timeseries/DO.png",plot_DO,width=16,height=8,dpi=450)
#ggsave("output/timeseries2004-2016/DO.png",plot_DO,width=16,height=8,dpi=450)
