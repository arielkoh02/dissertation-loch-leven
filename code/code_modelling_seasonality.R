#modelling seasonality and phytoplankton groups 

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(DHARMa)

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
  mutate(Biovolume= log(Biovolume)) %>% 
  filter(!(Phytoplankton_Group=='Total.Biovolume'))

View(alldatalong)

#running model on phytoplankon groups

modcryto <- lmer(Cryto.Biovolume ~ Season +(1|Year), data=alldata)
anova(modcryto)
summary(modcryto)

modcrytoplot<-simulateResiduals(fittedModel = modcryto, plot = F)
plot(modcrytoplot)

modcyano <- lmer(Cyano.Biovolume ~ Season +(1|Year), data=alldata)
anova(modcyano)
summary(modcyano)

modcyanoplot<-simulateResiduals(fittedModel = modcyano, plot = F)
plot(modcyanoplot)

moddiatoms <- lmer(Diatoms.Biovolume ~ Season +(1|Year), data=alldata)
anova(moddiatoms)
summary(moddiatoms)

moddiatomsplot<-simulateResiduals(fittedModel = moddiatoms, plot = F)
plot(moddiatomsplot)

modgreens <- lmer(Greens.Biovolume ~ Season +(1|Year), data=alldata)
anova(modgreens)
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
  facet_wrap(~Phytoplankton_Group, scales = "free_y") +  # Separate plots per group
  labs(title = "Seasonal Log-Transformed Phytoplankton Biovolume Distributions",
       x = "Season",
       y = "Log-Transformed Biovolume") +
  theme_classic() +
  scale_fill_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                               "Summer" = "violet", "Autumn" = "orange")) +
  theme(legend.position = "bottom") 

plot_violin
ggsave("output/seasonaltimeseries2004-2016/violinlogbiovolwithmedian.png",plot_violin,width=16,height=8,dpi=450)

#running model on daphnia

moddaphnia <- lmer(Daphnia ~ Season +(1|Year), data=alldata)
anova(moddaphnia)
summary(moddaphnia)

moddaphniaplot<-simulateResiduals(fittedModel = moddaphnia, plot = F)
plot(moddaphniaplot)

modnonp<-kruskal.test(Daphnia ~ Season, data = alldata)
modnonp

#install.packages("FSA")
library(FSA)
dunnTest(Daphnia ~ Season, data = alldata, method = "bh")  # Benjamini-Hochberg correction

seasonaldaphniaplot <- ggplot(alldata, aes(x = Season, y = Daphnia, fill = Season)) +
  geom_violin(alpha = 0.6) +
  stat_summary(fun = median, geom = "point", color = "black", size = 2) +  # Black dot for median
  stat_summary(fun.data = function(y) {
    data.frame(y = median(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75))
  }, geom = "errorbar", width = 0.1, linetype = "dashed", color = "black") +
  labs(title = "Seasonal Log-Transformed Daphnia Density",
       x = "Season",
       y = "Log-transformed Daphnia Density") +
  theme_classic() +
  scale_fill_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen", 
                               "Summer" = "violet", "Autumn" = "orange")) +
  theme(legend.position = "right") 

seasonaldaphniaplot
ggsave("output/seasonaltimeseries2004-2016/violindaphnia.png",seasonaldaphniaplot,width=16,height=8,dpi=450)
