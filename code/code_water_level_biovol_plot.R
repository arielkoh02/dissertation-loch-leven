#2/5/25
#plotting waterlevel against total biovolume

rm(list=ls())

library(tidyverse)

alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")
view(alldata)

data<-alldata %>% select(Total.Biovolume,waterlevel.masl) %>% drop_na() %>% 
  mutate(WLproxy=(max(waterlevel.masl, na.rm = TRUE) - waterlevel.masl))

view(data)


# Create a dummy data frame for the expected line
expected_line <- data.frame(
  WLproxy = c(min(data$WLproxy), max(data$WLproxy))
)
expected_line$Total.Biovolume <- 6e7 + (-4e7 * expected_line$WLproxy)


scientific_10 <- function(x) {
  labels <- scales::scientific_format()(x)                 # Step 1: Convert to scientific notation
  labels <- gsub("0e\\+?0", "0", labels)                   # Step 2: Handle zero values
  labels <- gsub("e\\+?", " %*% 10^", labels)              # Step 3: Convert to R's math notation
  parse(text = labels)                                     # Step 4: Convert to expression for ggplot
}

# Plot with both lines and legend
plot1 <- ggplot(data, aes(x = WLproxy, y = Total.Biovolume)) +
  geom_point() +
  scale_y_continuous(
    labels = scientific_10,
    breaks = c(0, 2e7, 4e7, 6e7, 8e7)  # you can customize these values
  ) +
  theme_classic(base_size=24) +
  labs( x="Actual Water Level Proxy (cm)", y= bquote("Total Phytoplankton Biovolume ("~mu*m^3~ml^-1*")")) +
  geom_smooth(aes(color = "Observed"), method = lm, se = FALSE, size = 1) +
  geom_line(data = expected_line, aes(x = WLproxy, y = Total.Biovolume, color = "Expected"),
            linetype = "dotted", size = 1) +
  scale_color_manual(name="Relationship",
    values = c("Observed" = "blue", "Expected" = "red")) +
  theme(legend.position = "bottom")

plot1

ggsave("output/conceptualplots/waterlevel.png",plot1,width=16,height=10,dpi=450)
