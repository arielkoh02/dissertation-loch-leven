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

#linear model taking into account srp & phytoplankton group interaction
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

#linear model taking into account srp & phytoplankton group interaction
mod4SRSiint <- lmerTest::lmer(Total.Biovolume ~ RB5.SRSi*Season + (1 | Year), 
                             data = alldata)
summary(mod4SRSiint)

mod4intSRSiplot<-simulateResiduals(fittedModel = mod4SRSiint, plot = F)
plot(mod4intSRSiplot)

#AIC 
AIC(mod1SRSi,mod2SRSi,mod2intSRSi,mod3SRSi,mod3SRSiint,mod4SRSi,mod4SRSiint)

### physical predictors

## pH
#linear model with just phosphorus as a fixed effect
mod1pH <- lmerTest::lmer(Total.Biovolume ~ RB5.pH +(1|Year), data=alldata)
summary(mod1pH)

mod1pHplot<-simulateResiduals(fittedModel = mod1pH, plot = F)
plot(mod1pHplot)

#linear model taking into account srp & phytoplankton group & season 
mod2pH <- lmerTest::lmer(Biovolume ~ RB5.pH + Phytoplankton_Group + Season + (1 | Year), 
                           data = all_data_long)
summary(mod2pH)

mod2pHplot<-simulateResiduals(fittedModel = mod2pH, plot = F)
plot(mod2pHplot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intpH <- lmerTest::lmer(Biovolume ~ RB5.pH*Phytoplankton_Group*Season + (1 | Year), 
                              data = all_data_long)
summary(mod2intpH)

mod2intpHplot<-simulateResiduals(fittedModel = mod2intpH, plot = F)
plot(mod2intpHplot)

#linear model taking into account srp & phytoplankton group 
mod3pH <- lmerTest::lmer(Biovolume ~ RB5.pH + Phytoplankton_Group + (1 | Year), 
                           data = all_data_long)
summary(mod3pH)

mod3pHPlot<-simulateResiduals(fittedModel = mod3pH, plot = F)
plot(mod3pHPlot)

#linear model taking into account srp & phytoplankton group interaction
mod3pHint <- lmerTest::lmer(Biovolume ~ RB5.pH*Phytoplankton_Group + (1 | Year), 
                              data = all_data_long)
summary(mod3pHint)

mod3intpHplot<-simulateResiduals(fittedModel = mod3pHint, plot = F)
plot(mod3intpHplot)

#linear model taking into account srp & season
mod4pH<- lmerTest::lmer(Total.Biovolume ~ RB5.pH + Season + (1 | Year), 
                           data = alldata)
summary(mod4pH)

mod4pHplot<-simulateResiduals(fittedModel = mod4pH, plot = F)
plot(mod4pHplot)

#linear model taking into account srp & phytoplankton group interaction
mod4pHint <- lmerTest::lmer(Total.Biovolume ~ RB5.pH*Season + (1 | Year), 
                              data = alldata)
summary(mod4pHint)

mod4intpHplot<-simulateResiduals(fittedModel = mod4pHint, plot = F)
plot(mod4intpHplot)

#AIC 
AIC(mod1pH,mod2pH,mod2intpH,mod3pH,mod3pHint,mod4pH,mod4pHint)

## SD
#linear model with just phosphorus as a fixed effect
mod1SD <- lmerTest::lmer(Total.Biovolume ~ RB5.SD +(1|Year), data=alldata)
summary(mod1SD)

mod1SDplot<-simulateResiduals(fittedModel = mod1SD, plot = F)
plot(mod1SDplot)

#linear model taking into account srp & phytoplankton group & season 
mod2SD <- lmerTest::lmer(Biovolume ~ RB5.SD + Phytoplankton_Group + Season + (1 | Year), 
                         data = all_data_long)
summary(mod2SD)

mod2SDplot<-simulateResiduals(fittedModel = mod2SD, plot = F)
plot(mod2SDplot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intSD <- lmerTest::lmer(Biovolume ~ RB5.SD*Phytoplankton_Group*Season + (1 | Year), 
                            data = all_data_long)
summary(mod2intSD)

mod2intSDplot<-simulateResiduals(fittedModel = mod2intSD, plot = F)
plot(mod2intSDplot)

#linear model taking into account srp & phytoplankton group 
mod3SD <- lmerTest::lmer(Biovolume ~ RB5.SD + Phytoplankton_Group + (1 | Year), 
                         data = all_data_long)
summary(mod3SD)

mod3SDPlot<-simulateResiduals(fittedModel = mod3SD, plot = F)
plot(mod3SDPlot)

#linear model taking into account srp & phytoplankton group interaction
mod3SDint <- lmerTest::lmer(Biovolume ~ RB5.SD*Phytoplankton_Group + (1 | Year), 
                            data = all_data_long)
summary(mod3SDint)

mod3intSDplot<-simulateResiduals(fittedModel = mod3SDint, plot = F)
plot(mod3intSDplot)

#linear model taking into account srp & season
mod4SD<- lmerTest::lmer(Total.Biovolume ~ RB5.SD + Season + (1 | Year), 
                        data = alldata)
summary(mod4SD)

mod4SDplot<-simulateResiduals(fittedModel = mod4SD, plot = F)
plot(mod4SDplot)

#linear model taking into account srp & phytoplankton group interaction
mod4SDint <- lmerTest::lmer(Total.Biovolume ~ RB5.SD*Season + (1 | Year), 
                            data = alldata)
summary(mod4SDint)

mod4intSDplot<-simulateResiduals(fittedModel = mod4SDint, plot = F)
plot(mod4intSDplot)

#AIC 
AIC(mod1SD,mod2SD,mod2intSD,mod3SD,mod3SDint,mod4SD,mod4SDint)

## Temperature 
#linear model with just phosphorus as a fixed effect
mod1Temp <- lmerTest::lmer(Total.Biovolume ~ RB5.Temp +(1|Year), data=alldata)
summary(mod1Temp)

mod1Tempplot<-simulateResiduals(fittedModel = mod1Temp, plot = F)
plot(mod1Tempplot)

#linear model taking into account srp & phytoplankton group & season 
mod2Temp <- lmerTest::lmer(Biovolume ~ RB5.Temp + Phytoplankton_Group + Season + (1 | Year), 
                         data = all_data_long)
summary(mod2Temp)

mod2Tempplot<-simulateResiduals(fittedModel = mod2Temp, plot = F)
plot(mod2Tempplot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intTemp <- lmerTest::lmer(Biovolume ~ RB5.Temp*Phytoplankton_Group*Season + (1 | Year), 
                            data = all_data_long)
summary(mod2intTemp)

mod2intTempplot<-simulateResiduals(fittedModel = mod2intTemp, plot = F)
plot(mod2intTempplot)

#linear model taking into account srp & phytoplankton group 
mod3Temp <- lmerTest::lmer(Biovolume ~ RB5.Temp + Phytoplankton_Group + (1 | Year), 
                         data = all_data_long)
summary(mod3Temp)

mod3TempPlot<-simulateResiduals(fittedModel = mod3Temp, plot = F)
plot(mod3TempPlot)

#linear model taking into account srp & phytoplankton group interaction
mod3Tempint <- lmerTest::lmer(Biovolume ~ RB5.Temp*Phytoplankton_Group + (1 | Year), 
                            data = all_data_long)
summary(mod3Tempint)

mod3intTempplot<-simulateResiduals(fittedModel = mod3Tempint, plot = F)
plot(mod3intTempplot)

#linear model taking into account srp & season
mod4Temp<- lmerTest::lmer(Total.Biovolume ~ RB5.Temp + Season + (1 | Year), 
                        data = alldata)
summary(mod4Temp)

mod4Tempplot<-simulateResiduals(fittedModel = mod4Temp, plot = F)
plot(mod4Tempplot)

#linear model taking into account srp & phytoplankton group interaction
mod4Tempint <- lmerTest::lmer(Total.Biovolume ~ RB5.Temp*Season + (1 | Year), 
                            data = alldata)
summary(mod4Tempint)

mod4intTempplot<-simulateResiduals(fittedModel = mod4Tempint, plot = F)
plot(mod4intTempplot)

#AIC 
AIC(mod1Temp,mod2Temp,mod2intTemp,mod3Temp,mod3Tempint,mod4Temp,mod4Tempint)


## Predator 
#linear model with just phosphorus as a fixed effect
mod1Daphnia <- lmerTest::lmer(Total.Biovolume ~ Daphnia +(1|Year), data=alldata)
summary(mod1Daphnia)

mod1Daphniaplot<-simulateResiduals(fittedModel = mod1Daphnia, plot = F)
plot(mod1Daphniaplot)

#linear model taking into account srp & phytoplankton group & season 
mod2Daphnia <- lmerTest::lmer(Biovolume ~ Daphnia + Phytoplankton_Group + Season + (1 | Year), 
                           data = all_data_long)
summary(mod2Daphnia)

mod2Daphniaplot<-simulateResiduals(fittedModel = mod2Daphnia, plot = F)
plot(mod2Daphniaplot)

#linear model taking into account srp & phytoplankton group & season interaction
mod2intDaphnia <- lmerTest::lmer(Biovolume ~ Daphnia*Phytoplankton_Group*Season + (1 | Year), 
                              data = all_data_long)
summary(mod2intDaphnia)

mod2intDaphniaplot<-simulateResiduals(fittedModel = mod2intDaphnia, plot = F)
plot(mod2intDaphniaplot)

#linear model taking into account srp & phytoplankton group 
mod3Daphnia <- lmerTest::lmer(Biovolume ~ Daphnia + Phytoplankton_Group + (1 | Year), 
                           data = all_data_long)
summary(mod3Daphnia)

mod3DaphniaPlot<-simulateResiduals(fittedModel = mod3Daphnia, plot = F)
plot(mod3DaphniaPlot)

#linear model taking into account srp & phytoplankton group interaction
mod3Daphniaint <- lmerTest::lmer(Biovolume ~ Daphnia*Phytoplankton_Group + (1 | Year), 
                              data = all_data_long)
summary(mod3Daphniaint)

mod3intDaphniaplot<-simulateResiduals(fittedModel = mod3Daphniaint, plot = F)
plot(mod3intDaphniaplot)

#linear model taking into account srp & season
mod4Daphnia<- lmerTest::lmer(Total.Biovolume ~ Daphnia + Season + (1 | Year), 
                          data = alldata)
summary(mod4Daphnia)

mod4Daphniaplot<-simulateResiduals(fittedModel = mod4Daphnia, plot = F)
plot(mod4Daphniaplot)

#linear model taking into account srp & phytoplankton group interaction
mod4Daphniaint <- lmerTest::lmer(Total.Biovolume ~ Daphnia*Season + (1 | Year), 
                              data = alldata)
summary(mod4Daphniaint)

mod4intDaphniaplot<-simulateResiduals(fittedModel = mod4Daphniaint, plot = F)
plot(mod4intDaphniaplot)

#AIC 
AIC(mod1Daphnia,mod2Daphnia,mod2intDaphnia,mod3Daphnia,mod3Daphniaint,mod4Daphnia,mod4Daphniaint)
