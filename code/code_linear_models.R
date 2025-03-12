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
plot(mod2SRPplot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intSRP <- lmerTest::lmer(Biovolume ~ RB5.SRP*Phytoplankton_Group*Season + (1 | Year), 
                         data = all_data_long)
summary(mod2intSRP)

mod2intSRPplot<-simulateResiduals(fittedModel = mod2intSRP, plot = F)
plot(mod2intSRPplot)

#AIC 
AIC(mod1SRP,mod2SRP,mod2intSRP)

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

AIC(mod1NO3,mod2NO3, mod2intNO3)
