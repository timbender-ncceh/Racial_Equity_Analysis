# Pull County US Census Data

library(dplyr)
library(tidycensus)
library(tigris)
library(lubridate)
library(ggplot2)


rm(list=ls());cat('\f');gc()


# smartsheet location----

# https://app.smartsheet.com/sheets/Q3HRjppwWFqwFC4PrWMvj2C9967RX3cHhQpXPQ91?view=grid
# nested heirarchy: BoS Developing CoC System / Racial Equity / Coc Policy and Governance / Data Focused Projects (~ row 1474)

