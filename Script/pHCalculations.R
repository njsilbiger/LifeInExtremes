## process pH for Megan's Maunalua data
library(tidyverse)
library(seacarb)
library(broom)

## bring in pH calibration files and raw data files
pHcalib<-read.csv('Data/pHCalibration.csv')
pHData<-read.csv("Data/pHduringSampling.csv")

## take the mV calibration files by each date and use them to calculate pH
pHData<-pHcalib %>%
  group_by(Date)%>%
  do(fitpH = lm(mVTris~TTris, data = .))%>% # linear regression of mV and temp of the tris
  tidy(fitpH) %>% # make the output tidy
  select(Date, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
  left_join(.,pHData) %>% # join with the pH sample data
  mutate(mVTris = Temp*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
  mutate(pH = pH(Ex=mV,Etris=mVTris,S=35,T=Temp))%>%
  select(PoolID, Date, Time, pH, Temp, Notes) ## need to calculate pH insi then it is done

write.csv(x = pHData, file = 'Output/pHData.csv')

