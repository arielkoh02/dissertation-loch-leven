# 18/3
# daphnia + phytoplankton biovolume plot 

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


# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_cryto = Daphnia / max(Daphnia, na.rm = TRUE) * max(Cryto.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_cryto <- max(predator.phyto.data$Cryto.Biovolume, na.rm = TRUE)
scale_factor_cryto <- max_cryto / max_daphnia

# Plot for Cryptophytes 
plot1<-ggplot(data=predator.phyto.data, aes(x = date_for_plot)) +
  geom_line(aes(y = Cryto.Biovolume), color = "black",linewidth=1) +  # Primary Y-axis: Phytoplankton
  geom_point(aes(y = Cryto.Biovolume, color = season,size=1.25)) +
  geom_line(aes(y = Daphnia_scaled_cryto), color = "grey",linewidth=1,linetype="dashed") + # Secondary Y-axis: Daphnia
  geom_point(aes(y = Daphnia_scaled_cryto, color = season,size=1.25),shape=10)+
  scale_y_continuous(
    name = "Cryto Biovolume",
    sec.axis = sec_axis(~ ./scale_factor, name = "Daphnia Density")  # No transformation, keeps original values
  ) +
  labs(x = "Year", title = "Seasonal Cryto Biovolume Trends with Daphnia") +
  theme_classic() +
  
  # Custom legend settings
  scale_color_manual(
    name = "Season",
    values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
               "Summer" = "maroon", "Autumn" = "orange"))
plot1

# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_cyano = Daphnia / max(Daphnia, na.rm = TRUE) * max(Cyano.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_cyano <- max(predator.phyto.data$Cyano.Biovolume, na.rm = TRUE)
scale_factor_cyano <- max_cyano / max_daphnia

# Plot for Cyanobacteria
plot2<-ggplot(data=predator.phyto.data, aes(x = date_for_plot)) +
  geom_line(aes(y = Cyano.Biovolume), color = "black",linewidth=1) +  # Primary Y-axis: Phytoplankton
  geom_point(aes(y = Cyano.Biovolume, color = season,size=1.25)) +
  geom_line(aes(y = Daphnia_scaled_cyano), color = "grey", linetype = "dashed",linewidth=1) + # Secondary Y-axis: Daphnia
  geom_point(aes(y = Daphnia_scaled_cyano, color = season,size=1.25),shape=10)+
  scale_y_continuous(
    name = "Cyano Biovolume",
    sec.axis = sec_axis(~ ./scale_factor_cyano, name = "Daphnia Density")  # No transformation, keeps original values
  ) +
  labs(x = "Year", title = "Seasonal Cyano Biovolume Trends with Daphnia") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))
plot2

# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_diatoms = Daphnia / max(Daphnia, na.rm = TRUE) * max(Diatoms.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_diatoms <- max(predator.phyto.data$Diatoms.Biovolume, na.rm = TRUE)
scale_factor_diatoms <- max_diatoms / max_daphnia

# Plot for Diatoms
plot3<-ggplot(data=predator.phyto.data, aes(x = date_for_plot)) +
  geom_line(aes(y = Diatoms.Biovolume), color = "black",linewidth=1) +  # Primary Y-axis: Phytoplankton
  geom_point(aes(y = Diatoms.Biovolume, color = season,size=1.25)) +
  geom_line(aes(y = Daphnia_scaled_diatoms), color = "grey", linetype = "dashed",linewidth=1) + # Secondary Y-axis: Daphnia
  geom_point(aes(y = Daphnia_scaled_diatoms, color = season,size=1.25),shape=10)+
  scale_y_continuous(
    name = "Diatom Biovolume",
    sec.axis = sec_axis(~ ./scale_factor_diatoms, name = "Daphnia Density")  # No transformation, keeps original values
  ) +
  labs(x = "Year", title = "Seasonal Diatoms Biovolume Trends with Daphnia") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))
plot3

# Normalize Daphnia so it fits on the same scale as biovolume
predator.phyto.data <- predator.phyto.data %>%
  mutate(Daphnia_scaled_greens = Daphnia / max(Daphnia, na.rm = TRUE) * max(Greens.Biovolume, na.rm = TRUE))

max_daphnia <- max(predator.phyto.data$Daphnia, na.rm = TRUE)
max_greens <- max(predator.phyto.data$Greens.Biovolume, na.rm = TRUE)
scale_factor_greens <- max_greens / max_daphnia

# Plot for Diatoms
plot4<-ggplot(data=predator.phyto.data, aes(x = date_for_plot)) +
  geom_line(aes(y = Greens.Biovolume), color = "black",linewidth=1) +  # Primary Y-axis: Phytoplankton
  geom_point(aes(y = Greens.Biovolume, color = season,size=1.25)) +
  geom_line(aes(y = Daphnia_scaled_greens), color = "grey", linetype = "dashed",linewidth=1) + # Secondary Y-axis: Daphnia
  geom_point(aes(y = Daphnia_scaled_greens, color = season,size=1.25),shape=10)+
  scale_y_continuous(
    name = "Greens Biovolume",
    sec.axis = sec_axis(~ ./scale_factor_greens, name = "Daphnia Density")  # No transformation, keeps original values
  ) +
  labs(x = "Year", title = "Seasonal Greens Biovolume Trends with Daphnia") +
  theme_classic() +
  scale_color_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                                "Summer" = "maroon", "Autumn" = "orange"))
plot4

library(ggpubr)

# Arrange in a 2x2 grid
plot_seasonalbiovoldaphnia<-ggarrange(plot1,plot2,plot3,plot4, nrow = 2, ncol = 2, common.legend = TRUE, legend="bottom")
plot_seasonalbiovoldaphnia
ggsave("output/seasonaltimeseries2004-2016/biovoldaphnia.png",plot_seasonalbiovoldaphnia,width=16,height=8,dpi=450)
