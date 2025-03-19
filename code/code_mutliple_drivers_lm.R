#13/3/25
#trying to do multiple drivers now 

rm=list(ls)

library(tidyverse)
library(lmerTest)
library(DHARMa)

alldata<-read.csv("data/csv/alldata-2004-2016.csv")

alldata<-alldata %>% mutate(Total.Biovolume=log(Total.Biovolume))

alldatalong <- alldata %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume") %>% 
  mutate(Biovolume= log(Biovolume)) %>% 
  filter(!(Phytoplankton_Group=='Total.Biovolume'))

View(alldata)
View(alldatalong)

#NO3+SRP
mod1 <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP+(1|Year), data=alldata)
summary(mod1)

mod1plot<-simulateResiduals(fittedModel = mod1, plot = F)
plot(mod1plot)

mod2 <- lmerTest::lmer(Biovolume ~ RB5.SRP + Phytoplankton_Group + Season + (1 | Year), 
                          data = alldatalong)
summary(mod2)

mod2plot<-simulateResiduals(fittedModel = mod2, plot = F)
plot(mod2plot)

mod3 <- lmerTest::lmer(Biovolume ~ RB5.SRP + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod3)

mod3plot<-simulateResiduals(fittedModel = mod3, plot = F)
plot(mod3plot)

AIC(mod1,mod2,mod3)

#NO3+SRP
mod4 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod4)

mod4plot<-simulateResiduals(fittedModel = mod4, plot = F)
plot(mod4plot)

AIC(mod1,mod2,mod3,mod4)

#NO3+SRP+Daphnia
mod5 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod5)

mod5plot<-simulateResiduals(fittedModel = mod5, plot = F)
plot(mod5plot)

AIC(mod1,mod2,mod3,mod4,mod5)

#NO3+SRP+Daphnia+pH
mod6 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod6)

mod6plot<-simulateResiduals(fittedModel = mod6, plot = F)
plot(mod6plot)

#NO3+SRP+Daphnia+pH+temp
mod7 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + RB5.Temp + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod7)

mod7plot<-simulateResiduals(fittedModel = mod7, plot = F)
plot(mod7plot)

#NO3+SRP+Daphnia+pH+temp+water depth
mod8 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + RB5.Temp + waterlevel.masl + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod8)

mod8plot<-simulateResiduals(fittedModel = mod8, plot = F)
plot(mod8plot)

#NO3+SRP+Daphnia+pH+temp+water depth+DO
mod9 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + RB5.Temp + waterlevel.masl + RB5.DO + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod9)

mod9plot<-simulateResiduals(fittedModel = mod9, plot = F)
plot(mod9plot)

#NO3+SRP+Daphnia+pH+temp+water depth+DO+SD
mod10 <- lmerTest::lmer(Biovolume ~ RB5.SRP + RB5.NO3 + Daphnia + RB5.pH + RB5.Temp + waterlevel.masl + RB5.DO + RB5.SD + Phytoplankton_Group*Season + (1 | Year), 
                       data = alldatalong)
summary(mod10)

mod10plot<-simulateResiduals(fittedModel = mod10, plot = F)
plot(mod10plot)


AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)
