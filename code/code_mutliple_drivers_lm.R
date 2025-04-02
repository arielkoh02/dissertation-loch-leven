#13/3/25
#trying to do multiple drivers now 

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(lme4)
library(DHARMa)
#install.packages("outliers")  # Install the package if not already installed
library(outliers)
#install.packages("MuMIn")
library(MuMIn)
#install.packages("car")
library(car)

alldata<-read.csv("data/csv/alldata-2004-2016v2.csv")

alldata<-alldata %>% mutate(Total.Biovolume=log(Total.Biovolume)) %>% 
  mutate(YearMonth=dmy(YearMonth))

alldatalong <- alldata %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume") %>% 
  mutate(Biovolume= log(Biovolume)) %>% 
  mutate(RB5.pH = scale(RB5.pH))%>% 
  filter(!(Phytoplankton_Group=='Total.Biovolume'))

View(alldata)
View(alldatalong)

alldata$Season <- relevel(factor(alldata$Season), ref = "Winter")
alldatalong$Season <- relevel(factor(alldatalong$Season), ref = "Winter")

#SRP
mod1 <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP+(1|Year), data=alldata)
summary(mod1)
r.squaredGLMM(mod1)

mod1plot<-simulateResiduals(fittedModel = mod1, plot = F)
plot(mod1plot)

mod2 <- lmerTest::lmer(Biovolume ~ RB5.SRP + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod2)
r.squaredGLMM(mod2)

mod2plot<-simulateResiduals(fittedModel = mod2, plot = F)
plot(mod2plot)

#NO3+SRP
mod3 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod3)
r.squaredGLMM(mod3)

mod3plot<-simulateResiduals(fittedModel = mod3, plot = F)
plot(mod3plot)

#NO3+SRP+Daphnia
mod4 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod4)
r.squaredGLMM(mod4)

mod4plot<-simulateResiduals(fittedModel = mod4, plot = F)
plot(mod4plot)

#NO3+SRP+Daphnia+temp
mod5 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.Temp + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod5)
r.squaredGLMM(mod5)

mod5plot<-simulateResiduals(fittedModel = mod5, plot = F)
plot(mod5plot)


#NO3+SRP+Daphnia+temp+water depth
mod6 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.Temp + waterlevel.masl + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod6)
r.squaredGLMM(mod6)

mod6plot<-simulateResiduals(fittedModel = mod6, plot = F)
plot(mod6plot)


#NO3+SRP+Daphnia+temp+water depth+DO
mod7 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.Temp + waterlevel.masl + RB5.DO + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod7)
r.squaredGLMM(mod7)

mod7plot<-simulateResiduals(fittedModel = mod7, plot = F)
plot(mod7plot)

vif(mod7)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
S