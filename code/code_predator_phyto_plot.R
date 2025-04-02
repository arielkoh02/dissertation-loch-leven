# 18/3
# daphnia + phytoplankton biovolume plot 

#install.packages("RColorBrewer")
library(RColorBrewer)

predator.phyto.data <- full_join(
  select(phyto_seasonal_average,
         season_year,
         season,
         Cryto.Biovolume,
         Cyano.Biovolume,
         Diatoms.Biovolume,
         Greens.Biovolume,
         month_for_plot,
         date_for_plot),  
  select(seasonal_predator,
         Daphnia,
         date_for_plot),  
  by = "date_for_plot")

View(predator.phyto.data)

min_value <- 0.001
max_value <- max(c(cryto_long$Value, cyano_long$Value, diatoms_long$Value, greens_long$Value), na.rm = TRUE)

min_value
max_value
# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_cryto = Daphnia / max(Daphnia, na.rm = TRUE) * max(Cryto.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_cryto <- max(predator.phyto.data$Cryto.Biovolume, na.rm = TRUE)
scale_factor_cryto <- max_cryto / max_daphnia

cryto_daphnia_data<-predator.phyto.data %>% 
  select(date_for_plot,Cryto.Biovolume, Daphnia_scaled_cryto) %>% 
  rename(Crytomonads="Cryto.Biovolume",Daphnia="Daphnia_scaled_cryto")  
View(cryto_daphnia_data)

cryto_long<-cryto_daphnia_data %>% 
  pivot_longer(cols = c("Crytomonads","Daphnia"),  # Select biovolume columns
                            names_to = "Type",  # Create new column for names
                            values_to = "Value")
  

View(cryto_long)

# Plot for Cryptophytes 
plot1<-ggplot(data=cryto_long, aes(x = date_for_plot, y=Value)) +
  geom_line(aes(linetype=Type,colour=Type),linewidth=0.75) +  # Primary Y-axis: Phytoplankton
  scale_y_continuous(
    name = bquote("Biovolume ("~mu*m^3~ml^-1*")"),
    sec.axis = sec_axis(~ ./scale_factor_cryto)  # No transformation, keeps original values
  ) +
  labs(x = "Year", title="Crytomonads") +
  theme_classic(base_size=17)+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5) )+
  scale_colour_manual(values = c("Crytomonads" = "#377EB8",  # Green from Set1
                                 "Daphnia" = "#999999")) 

plot1

# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_cyano = Daphnia / max(Daphnia, na.rm = TRUE) * max(Cyano.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_cyano <- max(predator.phyto.data$Cyano.Biovolume, na.rm = TRUE)
scale_factor_cyano <- max_cyano / max_daphnia

cyano_daphnia_data<-predator.phyto.data %>% 
  select(date_for_plot,Cyano.Biovolume, Daphnia_scaled_cyano) %>% 
  rename(Cyanobacteria="Cyano.Biovolume",Daphnia="Daphnia_scaled_cyano")  
View(cyano_daphnia_data)

cyano_long<-cyano_daphnia_data %>% 
  pivot_longer(cols = c("Cyanobacteria","Daphnia"),  # Select biovolume columns
               names_to = "Type",  # Create new column for names
               values_to = "Value")

scientific_10 <- function(x) {
  labels <- scales::scientific_format()(x)       # Convert numbers to scientific notation
  labels <- gsub("0e\\+?0", "0", labels)         # Replace "0e0" or "0e+0" with "0"
  labels <- gsub("e\\+?", " %*% 10^", labels)    # Format other numbers correctly
  parse(text = labels)
}

# Plot for Cyanobacteria
plot2<-ggplot(data=cyano_long, aes(x = date_for_plot, y=Value)) +
  geom_line(aes(linetype=Type,colour=Type),linewidth=0.75) +  # Primary Y-axis: Phytoplankton
  scale_y_continuous(label=scientific_10,
    sec.axis = sec_axis(~ ./scale_factor_cyano, name = bquote("Daphnia Density ("~individuals~L^-1*")"))  # No transformation, keeps original values
  ) +
  labs(x = "Year", title="Cyanobacteria") +
  theme_classic(base_size=17)+
  theme(legend.position="bottom", axis.title.y.left = element_blank(), plot.title = element_text(hjust = 0.5) )+
  scale_colour_manual(values = c("Cyanobacteria" = "#984EA3",  # Green from Set1
                                "Daphnia" = "#999999")) 

plot2

# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_diatoms = Daphnia / max(Daphnia, na.rm = TRUE) * max(Diatoms.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_diatoms <- max(predator.phyto.data$Diatoms.Biovolume, na.rm = TRUE)
scale_factor_diatoms <- max_diatoms / max_daphnia

diatoms_daphnia_data<-predator.phyto.data %>% 
  select(date_for_plot,Diatoms.Biovolume, Daphnia_scaled_diatoms) %>% 
  rename(Diatoms="Diatoms.Biovolume",Daphnia="Daphnia_scaled_diatoms")  
View(diatoms_daphnia_data)

diatoms_long<-diatoms_daphnia_data %>% 
  pivot_longer(cols = c("Diatoms","Daphnia"),  # Select biovolume columns
               names_to = "Type",  # Create new column for names
               values_to = "Value") %>% 
  mutate(Type = factor(Type, levels = c("Diatoms", "Daphnia")))

# Plot for Cyanobacteria
plot3<-ggplot(data=diatoms_long, aes(x = date_for_plot, y=Value)) +
  geom_line(aes(linetype=Type,colour=Type),linewidth=0.75) +  # Primary Y-axis: Phytoplankton
  scale_y_continuous(
    name = bquote("Biovolume ("*~mu*m^3~ml^-1*")"),label=scientific_10,
    sec.axis = sec_axis(~ ./scale_factor_diatoms)  # No transformation, keeps original values
  ) +
  labs(x = "Year", title="Diatoms") +
  theme_classic(base_size=17)+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5) )+
  scale_colour_manual(values = c("Diatoms" = "#E41A1C",  # Green from Set1
                                 "Daphnia" = "#999999"))

plot3
# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_greens = Daphnia / max(Daphnia, na.rm = TRUE) * max(Greens.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_greens <- max(predator.phyto.data$Greens.Biovolume, na.rm = TRUE)
scale_factor_greens <- max_greens / max_daphnia

greens_daphnia_data<-predator.phyto.data %>% 
  select(date_for_plot,Greens.Biovolume, Daphnia_scaled_greens) %>% 
  rename(Greens="Greens.Biovolume",Daphnia="Daphnia_scaled_greens")  
View(greens_daphnia_data)

greens_long<-greens_daphnia_data %>% 
  pivot_longer(cols = c("Greens","Daphnia"),  # Select biovolume columns
               names_to = "Type",  # Create new column for names
               values_to = "Value") %>% 
  mutate(Type = factor(Type, levels = c("Greens", "Daphnia")))

# Plot for Cyanobacteria
plot4<-ggplot(data=greens_long, aes(x = date_for_plot, y=Value)) +
  geom_line(aes(linetype=Type,colour=Type),linewidth=0.75) +  # Primary Y-axis: Phytoplankton
  scale_y_continuous(
    sec.axis = sec_axis(~ ./scale_factor_greens, name = bquote("Daphnia Density ("~individuals~L^-1*")"))# No transformation, keeps original values
  ) +
  labs(x = "Year", title= "Green Algae") +
  theme_classic(base_size=17)+
  theme(legend.position="bottom", axis.title.y.left = element_blank(), plot.title = element_text(hjust = 0.5) )+
  scale_colour_manual(values = c("Greens" = "#4DAF4A",  # Green from Set1
                                 "Daphnia" = "#999999"))

plot4

library(ggpubr)

# Arrange in a 2x2 grid
plot_seasonalbiovoldaphnia <- ggarrange(
  plot1, NULL, plot2,
  plot3, NULL, plot4,
  nrow = 2, ncol = 3,
  widths = c(1, 0.05, 1),  # Adds space between columns
  common.legend = FALSE,
  legend = "bottom"
)
plot_seasonalbiovoldaphnia
ggsave("output/seasonaltimeseries2004-2016/biovoldaphnia.png",plot_seasonalbiovoldaphnia,width=15,height=10,dpi=650, bg = "white")



# 1:1 plot 

sumdata<-predator.phyto.data %>% 
  select(Daphnia,Cryto.Biovolume,Cyano.Biovolume,Greens.Biovolume) %>% 
  mutate(Sum.Biovolume=Cryto.Biovolume+Cyano.Biovolume+Greens.Biovolume
  )

View(sumdata)

# Compute min and max values
x_min <- min(sumdata$Daphnia, na.rm = TRUE)
x_max <- max(sumdata$Daphnia, na.rm = TRUE)
y_min <- min(sumdata$Sum.Biovolume, na.rm = TRUE)
y_max <- max(sumdata$Sum.Biovolume, na.rm = TRUE)

# Calculate a scaled slope based on data ranges
scaled_slope <- (y_max - y_min) / (x_max - x_min)

plot5 <- ggplot(data = sumdata, aes(y = Sum.Biovolume, x = Daphnia)) +
  geom_point(size = 3) +
  
  # Scaled 1:1 line
  geom_abline(slope = scaled_slope, intercept = y_min - scaled_slope * x_min, 
              color = "red", linetype = "dashed", linewidth = 1) +
  theme_classic(base_size = 22) +
  
  labs(
    x = bquote("Daphnia (individuals "L^-1*")"),
    y = bquote("Total Non-Diatoms Biovolume ("*~
                 ~mu*m^3~ml^-1*")")
  ) +
  scale_y_continuous(labels = scientific_10)

plot5


ggsave("output/seasonaltimeseries2004-2016/1to1plot.png",plot5,width=16,height=10,dpi=450)
