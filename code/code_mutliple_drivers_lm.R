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

#SRP
mod1 <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP+(1|Year), data=alldata)
summary(mod1)
r.squaredGLMM(mod1)

mod1plot<-simulateResiduals(fittedModel = mod1, plot = F)
plot(mod1plot)

mod3 <- lmerTest::lmer(Biovolume ~ RB5.SRP + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod3)
r.squaredGLMM(mod3)

mod3plot<-simulateResiduals(fittedModel = mod3, plot = F)
plot(mod3plot)

#NO3+SRP
mod4 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod4)
r.squaredGLMM(mod4)

mod4plot<-simulateResiduals(fittedModel = mod4, plot = F)
plot(mod4plot)

#NO3+SRP+Daphnia
mod5 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod5)
r.squaredGLMM(mod5)

mod5plot<-simulateResiduals(fittedModel = mod5, plot = F)
plot(mod5plot)

#SRP+Daphnia
mod5b <- lmerTest::lmer(Biovolume ~ RB5.SRP + Daphnia + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod5b)
r.squaredGLMM(mod5b)

mod5bplot<-simulateResiduals(fittedModel = mod5b, plot = F)
plot(mod5bplot)

#NO3+SRP+Daphnia+pH

mod6 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod6)
r.squaredGLMM(mod6)

mod6plot<-simulateResiduals(fittedModel = mod6, plot = F)
plot(mod6plot)

#NO3+SRP+Daphnia+pH+temp
mod7 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH+ RB5.Temp + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod7)
r.squaredGLMM(mod7)


mod7plot<-simulateResiduals(fittedModel = mod7, plot = F)
plot(mod7plot)

#NO3+SRP+Daphnia+temp
mod7a <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.Temp + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod7a)
r.squaredGLMM(mod7a)


mod7aplot<-simulateResiduals(fittedModel = mod7a, plot = F)
plot(mod7aplot)

#NO3+SRP+Daphnia+temp
mod7b <- lmerTest::lmer(Biovolume ~ RB5.SRP + Daphnia + RB5.Temp + Phytoplankton_Group*Season + (1 | Year), 
                        data = alldatalong)
summary(mod7b)
r.squaredGLMM(mod7b)


mod7bplot<-simulateResiduals(fittedModel = mod7b, plot = F)
plot(mod7bplot)


#NO3+SRP+Daphnia+pH+temp+water depth
mod8 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + RB5.Temp + waterlevel.masl + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod8)
r.squaredGLMM(mod8)

mod8plot<-simulateResiduals(fittedModel = mod8, plot = F)
plot(mod8plot)

#NO3+SRP+Daphnia+temp+water depth
mod8a <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.Temp + waterlevel.masl + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod8a)
r.squaredGLMM(mod8a)

mod8aplot<-simulateResiduals(fittedModel = mod8a, plot = F)
plot(mod8aplot)

#SRP+Daphnia+temp+water depth
mod8b <- lmerTest::lmer(Biovolume ~ RB5.SRP + Daphnia + RB5.Temp + waterlevel.masl + Phytoplankton_Group*Season + (1 | Year), 
                        data = alldatalong)
summary(mod8b)
r.squaredGLMM(mod8b)

mod8bplot<-simulateResiduals(fittedModel = mod8b, plot = F)
plot(mod8bplot)

vif(mod8b)

#NO3+SRP+Daphnia+pH+temp+water depth+DO
mod9 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + RB5.Temp + waterlevel.masl + RB5.DO + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod9)
r.squaredGLMM(mod9)

mod9plot<-simulateResiduals(fittedModel = mod9, plot = F)
plot(mod9plot)

#NO3+SRP+Daphnia+temp+water depth+DO
mod9a <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.Temp + waterlevel.masl + RB5.DO + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod9a)
r.squaredGLMM(mod9a)

mod9aplot<-simulateResiduals(fittedModel = mod9a, plot = F)
plot(mod9aplot)

vif(mod9a)

#SRP+Daphnia+temp+water depth+DO
mod9b <- lmerTest::lmer(Biovolume ~ RB5.SRP + Daphnia + RB5.Temp + waterlevel.masl + RB5.DO + Phytoplankton_Group*Season + (1 | Year), 
                        data = alldatalong)
summary(mod9b)
r.squaredGLMM(mod9b)
vif(mod9b)

mod9bplot<-simulateResiduals(fittedModel = mod9b, plot = F)
plot(mod9bplot)


AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)
AIC(mod7a,mod8a,mod9a)
AIC(mod5b,mod7b,mod8b,mod9b)
