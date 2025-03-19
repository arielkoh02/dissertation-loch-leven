#13/3/25
#trying to do multiple drivers now 

rm=list(ls)

library(tidyverse)
#install.packages("lme4")
#library(lme4)
#install.packages("lmerTest")
library(lmerTest)
#install.packages("DHARMa")
library(DHARMa)

alldata<-read.csv("data/csv/alldata-2004-2016.csv")

View(alldata)

alldata<-alldata %>% mutate(Total.Biovolume=log(Total.Biovolume))

alldatalong <- alldata %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume") %>% 
  mutate(Biovolume= log(Biovolume)) %>% 
  filter(!(Phytoplankton_Group=='Total.Biovolume'))

#NO3+SRP
mod1 <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP+RB5.NO3+(1|Year), data=alldata)
summary(mod1)

mod1plot<-simulateResiduals(fittedModel = mod1, plot = F)
plot(mod1plot)

mod2 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Phytoplankton_Group + Season + (1 | Year), 
                          data = alldatalong)
summary(mod2)

mod2plot<-simulateResiduals(fittedModel = mod2, plot = F)
plot(mod2plot)

AIC(mod1,mod2)

#NO3+SRP+Predator
mod3 <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP+RB5.NO3+Daphnia+(1|Year), data=alldata)
summary(mod3)

mod3plot<-simulateResiduals(fittedModel = mod3, plot = F)
plot(mod3plot)

mod4 <- lmerTest::lmer(Biovolume ~ RB5.SRP+RB5.NO3+Daphnia+Phytoplankton_Group + Season +(1|Year), data=alldatalong)
summary(mod4)

mod4plot<-simulateResiduals(fittedModel = mod4, plot = F)
plot(mod4plot)

AIC(mod1,mod2,mod3)


