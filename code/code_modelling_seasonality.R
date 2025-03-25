#modelling seasonality and phytoplankton groups 

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(DHARMa)

alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")

alldata<-alldata %>% mutate(Total.Biovolume=log(Total.Biovolume),
                            #Cryto.Biovolume=log(Cryto.Biovolume),
                            #Cyano.Biovolume=log(Cyano.Biovolume),
                            #Diatoms.Biovolume=log(Diatoms.Biovolume),
                            #Greens.Biovolume=log(Greens.Biovolume),
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

modcryto <- lmer(Cryto.Biovolume ~ Season +(1|Year), data=alldata)
summary(modcryto)

modcrytoplot<-simulateResiduals(fittedModel = modcryto, plot = F)
plot(modcrytoplot)

modcyano <- lmer(Cyano.Biovolume ~ Season +(1|Year), data=alldata)
summary(modcyano)

modcyanoplot<-simulateResiduals(fittedModel = modcyano, plot = F)
plot(modcyanoplot)

moddiatoms <- lmer(Diatoms.Biovolume ~ Season +(1|Year), data=alldata)
summary(moddiatoms)

moddiatomsplot<-simulateResiduals(fittedModel = moddiatoms, plot = F)
plot(moddiatomsplot)

modgreens <- lmer(Greens.Biovolume ~ Season +(1|Year), data=alldata)
summary(modgreens)

modgreensplot<-simulateResiduals(fittedModel = modgreens, plot = F)
plot(modgreensplot)



#plotting 
plot_violin <- ggplot(alldatalong, aes(x = Season, y = Biovolume, fill = Season)) +
  geom_violin(alpha = 0.6) +
  stat_summary(fun = median, geom = "point", color = "black", size = 2) +  # Black dot for median
  stat_summary(fun.data = function(y) {
    data.frame(y = median(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75))
  }, geom = "errorbar", width = 0.1, linetype = "dashed", color = "black") +
  facet_wrap(~Phytoplankton_Group, scales = "fixed", 
             labeller = labeller(Phytoplankton_Group = c(
               "Cryto.Biovolume" = "Crytomonads",
               "Cyano.Biovolume"="Cyanobacteria",
               "Diatoms.Biovolume" = "Diatoms",
               "Greens.Biovolume"="Greens"
             ))) + # Rename facet titles
  labs(x = "Season",
       y = "Biovolume (µm^3 per ml)") +
  theme_classic(base_size = 22) +
  scale_fill_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                               "Summer" = "violet", "Autumn" = "orange")) +
  theme(legend.position = "bottom") +
  scale_y_log10() 

plot_violin
ggsave("output/seasonaltimeseries2004-2016/violinbiovolwithmedian.png",plot_violin,width=16,height=8,dpi=450)

