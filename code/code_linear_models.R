#12/3/25
#linear modelling 

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

### chemical predictors
## SRP 
#linear model with just phosphorus as a fixed effect
mod1SRP <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP+(1|Year), data=alldata)
summary(mod1SRP)

mod1SRPplot<-simulateResiduals(fittedModel = mod1SRP, plot = F)
plot(mod1SRPplot)

#convert to long form 
alldatalong <- all_data %>%
  pivot_longer(cols = ends_with("Biovolume"),  # Select biovolume columns
               names_to = "Phytoplankton_Group",  # Create new column for names
               values_to = "Biovolume") 

View(alldatalong)

#linear model taking into account srp & phytoplankton group & season 
mod2SRP <- lmerTest::lmer(Biovolume ~ RB5.SRP + Phytoplankton_Group + Season + (1 | Year), 
               data = all_data_long)
summary(mod2SRP)

mod2SRPlot<-simulateResiduals(fittedModel = mod2SRP, plot = F)
plot(mod2SRPlot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intSRP <- lmerTest::lmer(Biovolume ~ RB5.SRP*Phytoplankton_Group*Season + (1 | Year), 
                         data = all_data_long)
summary(mod2intSRP)

mod2intSRPplot<-simulateResiduals(fittedModel = mod2intSRP, plot = F)
plot(mod2intSRPplot)

#linear model taking into account srp & phytoplankton group 
mod3SRP <- lmerTest::lmer(Biovolume ~ RB5.SRP + Phytoplankton_Group + (1 | Year), 
                          data = all_data_long)
summary(mod3SRP)

mod3SRPlot<-simulateResiduals(fittedModel = mod3SRP, plot = F)
plot(mod3SRPlot)

#linear model taking into account srp & phytoplankton group interation
mod3SRPint <- lmerTest::lmer(Biovolume ~ RB5.SRP*Phytoplankton_Group + (1 | Year), 
                          data = all_data_long)
summary(mod3SRPint)

mod3intSRPlot<-simulateResiduals(fittedModel = mod3SRPint, plot = F)
plot(mod3intSRPlot)

#linear model taking into account srp & season
mod4SRP <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP + Season + (1 | Year), 
                          data = alldata)
summary(mod4SRP)

mod4SRPlot<-simulateResiduals(fittedModel = mod4SRP, plot = F)
plot(mod4SRPlot)

#linear model taking into account srp & phytoplankton group interation
mod4SRPint <- lmerTest::lmer(Total.Biovolume ~ RB5.SRP*Season + (1 | Year), 
                             data = alldata)
summary(mod4SRPint)

mod4intSRPlot<-simulateResiduals(fittedModel = mod4SRPint, plot = F)
plot(mod4intSRPlot)

#AIC 
AIC(mod1SRP,mod2SRP,mod2intSRP,mod3SRP,mod3SRPint,mod4SRP,mod4SRPint)

#NO3
#linear model with just phosphorus as a fixed effect
mod1NO3 <- lmerTest::lmer(Total.Biovolume ~ RB5.NO3+(1|Year), data=alldata)
summary(mod1NO3)

mod1NO3plot<-simulateResiduals(fittedModel = mod1NO3, plot = F)
plot(mod1NO3plot)

#linear model taking into account srp & phytoplankton group & season 
mod2NO3 <- lmerTest::lmer(Biovolume ~ RB5.NO3 + Phytoplankton_Group + Season + (1 | Year), 
                          data = all_data_long)
summary(mod2NO3)

mod2NO3plot<-simulateResiduals(fittedModel = mod2NO3, plot = F)
plot(mod2NO3plot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intNO3 <- lmerTest::lmer(Biovolume ~ RB5.NO3*Phytoplankton_Group*Season + (1 | Year), 
                             data = all_data_long)
summary(mod2intNO3)

mod2intNO3plot<-simulateResiduals(fittedModel = mod2intNO3, plot = F)
plot(mod2intNO3plot)

#linear model taking into account srp & phytoplankton group 
mod3NO3 <- lmerTest::lmer(Biovolume ~ RB5.NO3 + Phytoplankton_Group + (1 | Year), 
                          data = all_data_long)
summary(mod3NO3)

mod3NO3Plot<-simulateResiduals(fittedModel = mod3NO3, plot = F)
plot(mod3NO3Plot)

#linear model taking into account srp & phytoplankton group interation
mod3NO3int <- lmerTest::lmer(Biovolume ~ RB5.NO3*Phytoplankton_Group + (1 | Year), 
                             data = all_data_long)
summary(mod3NO3int)

mod3intNO3lot<-simulateResiduals(fittedModel = mod3NO3int, plot = F)
plot(mod3intNO3lot)

#linear model taking into account srp & season
mod4NO3 <- lmerTest::lmer(Total.Biovolume ~ RB5.NO3 + Season + (1 | Year), 
                          data = alldata)
summary(mod4NO3)

mod4NO3plot<-simulateResiduals(fittedModel = mod4NO3, plot = F)
plot(mod4NO3plot)

#linear model taking into account srp & phytoplankton group interation
mod4NO3int <- lmerTest::lmer(Total.Biovolume ~ RB5.NO3*Season + (1 | Year), 
                             data = alldata)
summary(mod4NO3int)

mod4intNO3Plot<-simulateResiduals(fittedModel = mod4NO3int, plot = F)
plot(mod4intNO3Plot)

#AIC 
AIC(mod1NO3,mod2NO3,mod2intNO3,mod3NO3,mod3NO3int,mod4NO3,mod4NO3int)

#SRSi
#linear model with just phosphorus as a fixed effect
mod1SRSi <- lmerTest::lmer(Total.Biovolume ~ RB5.SRSi+(1|Year), data=alldata)
summary(mod1SRSi)

mod1SRSiplot<-simulateResiduals(fittedModel = mod1SRSi, plot = F)
plot(mod1SRSiplot)

#linear model taking into account srp & phytoplankton group & season 
mod2SRSi <- lmerTest::lmer(Biovolume ~ RB5.SRSi + Phytoplankton_Group + Season + (1 | Year), 
                          data = all_data_long)
summary(mod2SRSi)

mod2SRSiplot<-simulateResiduals(fittedModel = mod2SRSi, plot = F)
plot(mod2SRSiplot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intSRSi <- lmerTest::lmer(Biovolume ~ RB5.SRSi*Phytoplankton_Group*Season + (1 | Year), 
                             data = all_data_long)
summary(mod2intSRSi)

mod2intSRSiplot<-simulateResiduals(fittedModel = mod2intSRSi, plot = F)
plot(mod2intSRSiplot)

#linear model taking into account srp & phytoplankton group 
mod3SRSi <- lmerTest::lmer(Biovolume ~ RB5.SRSi + Phytoplankton_Group + (1 | Year), 
                          data = all_data_long)
summary(mod3SRSi)

mod3SRSiPlot<-simulateResiduals(fittedModel = mod3SRSi, plot = F)
plot(mod3SRSiPlot)

#linear model taking into account srp & phytoplankton group interation
mod3SRSiint <- lmerTest::lmer(Biovolume ~ RB5.SRSi*Phytoplankton_Group + (1 | Year), 
                             data = all_data_long)
summary(mod3SRSiint)

mod3intSRSiplot<-simulateResiduals(fittedModel = mod3SRSiint, plot = F)
plot(mod3intSRSiplot)

#linear model taking into account srp & season
mod4SRSi <- lmerTest::lmer(Total.Biovolume ~ RB5.SRSi + Season + (1 | Year), 
                          data = alldata)
summary(mod4SRSi)

mod4SRSiplot<-simulateResiduals(fittedModel = mod4SRSi, plot = F)
plot(mod4SRSiplot)

#linear model taking into account srp & phytoplankton group interation
mod4SRSiint <- lmerTest::lmer(Total.Biovolume ~ RB5.SRSi*Season + (1 | Year), 
                             data = alldata)
summary(mod4SRSiint)

mod4intSRSiplot<-simulateResiduals(fittedModel = mod4SRSiint, plot = F)
plot(mod4intSRSiplot)

#AIC 
AIC(mod1SRSi,mod2SRSi,mod2intSRSi,mod3SRSi,mod3SRSiint,mod4SRSi,mod4SRSiint)
