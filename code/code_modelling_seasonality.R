#modelling seasonality and phytoplankton groups 

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(DHARMa)
#install.packages("emmeans")  # If not already installed
library(emmeans)
library(scales)
library(RColorBrewer)


alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")

alldata<-alldata %>% mutate(Total.Biovolume=log(Total.Biovolume),
                            Cryto.Biovolume=log(Cryto.Biovolume),
                            Cyano.Biovolume=log(Cyano.Biovolume),
                            Diatoms.Biovolume=log(Diatoms.Biovolume),
                            Greens.Biovolume=log(Greens.Biovolume),
                            Daphnia=log(Daphnia)) %>% 
  mutate(YearMonth=dmy(YearMonth))

alldatalong <- alldata %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume") %>% 
  #mutate(Biovolume= log(Biovolume)) %>% 
  filter(!(Phytoplankton_Group=='Total.Biovolume')) %>% 
  mutate( Season= factor(Season, levels = c("Winter", "Spring","Summer", "Autumn")))

View(alldatalong)

#running model on phytoplankon groups
alldata$Season <- relevel(factor(alldata$Season), ref = "Winter")

#cryto
modcryto <- lmer(Cryto.Biovolume ~ Season +(1|Year), data=alldata)
summary(modcryto)

modcrytoplot<-simulateResiduals(fittedModel = modcryto, plot = F)
plot(modcrytoplot)

emmcryto <- emmeans(modcryto, ~ Season)
pairs(emmcryto, adjust = "tukey")

#cyano
modcyano <- lmer(Cyano.Biovolume ~ Season +(1|Year), data=alldata)
summary(modcyano)

modcyanoplot<-simulateResiduals(fittedModel = modcyano, plot = F)
plot(modcyanoplot)

emmcyano <- emmeans(modcyano, ~ Season)
pairs(emmcyano, adjust = "tukey")

#diatoms
moddiatoms <- lmer(Diatoms.Biovolume ~ Season +(1|Year), data=alldata)
summary(moddiatoms)

moddiatomsplot<-simulateResiduals(fittedModel = moddiatoms, plot = F)
plot(moddiatomsplot)

emmdiatoms <- emmeans(moddiatoms, ~ Season)
pairs(emmdiatoms, adjust = "tukey")

#greens
modgreens <- lmer(Greens.Biovolume ~ Season +(1|Year), data=alldata)
summary(modgreens)

modgreensplot<-simulateResiduals(fittedModel = modgreens, plot = F)
plot(modgreensplot)

emmgreens <- emmeans(modgreens, ~ Season)
pairs(emmgreens, adjust = "tukey")



#plotting 
plot_violin <- ggplot(alldatalong, aes(x = Season, y = Biovolume, fill = Season)) +
  geom_violin(alpha = 0.5) +
  stat_summary(fun = median, geom = "point", color = "black", size = 2) +  # Black dot for median
  stat_summary(fun.data = function(y) {
    data.frame(y = median(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75))
  }, geom = "errorbar", width = 0.1, linetype = "dashed", color = "black") +
  facet_wrap(~Phytoplankton_Group, scales = "fixed", 
             labeller = labeller(Phytoplankton_Group = c(
               "Cryto.Biovolume" = "Crytomonads",
               "Cyano.Biovolume"="Cyanobacteria",
               "Diatoms.Biovolume" = "Diatoms",
               "Greens.Biovolume"="Green Algae"
             ))) + # Rename facet titles
  labs(x = "Season",
       y = bquote("Biovolume ("*mu*m^3~ml^-1*")")) +
  theme_classic(base_size = 30) +
  scale_fill_manual(values = c("Winter" = "#7570B3", "Spring" = "#66A61E", 
                               "Summer" = "#E6AB02", "Autumn" = "#E7298A")) +
  theme(legend.position = "bottom", strip.background=element_blank()) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))

plot_violin
ggsave("output/seasonaltimeseries2004-2016/violinbiovolwithmedian.png",plot_violin,width=17,height=12,dpi=650)

