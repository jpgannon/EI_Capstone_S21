library(tidyverse)
library(lubridate)
library(readr)
library(shiny)
#Import Soil Data

SoilResp <- read_csv("Litter_and_Respiration/SoilResp.csv")


#Convert date to "month, day, year" form

SoilResp_Filtered <- SoilResp %>% select(date, stand, flux, temperature, treatment) %>%
  mutate(date = mdy(date))

#Import Leaf litter
Litterfall <- read_csv("Litter_and_Respiration/Litterfall.csv")

Litterfall_Filtered <- Litterfall %>% 
  mutate(Treatment = paste(Treatment))

#Make plot
ggplot(SoilResp_Filtered, aes(x = date, y = value))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(title = "Soil Respiration Flux", 
       x = "Date", 
       y = "CO2 efflux per unit area (μg CO2/m2/s)")+
  geom_smooth(method = "lm")
  

summary(SoilResp_Filtered$stand)
unique(SoilResp_Filtered$stand)
unique(SoilResp_Filtered$treatment)
