# Pull County US Census Data

library(dplyr)
library(tidycensus)
library(tigris)
library(lubridate)
library(ggplot2)
library(readr)
library(sf)

rm(list=ls());cat('\f');gc()

# SetWD----
prime.wd    <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis"
hudtools.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/hud_tools"
data.wd     <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/data"
r_code.wd   <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/R"


setwd(prime.wd)

# Vars----
census.year <- 2021

# smartsheet----
# https://app.smartsheet.com/sheets/Q3HRjppwWFqwFC4PrWMvj2C9967RX3cHhQpXPQ91?view=grid
# nested heirarchy: BoS Developing CoC System / Racial Equity / Coc Policy and Governance / Data Focused Projects (~ row 1474)

# crosswalks----
all.nc.counties      <- tigris::counties(state = "NC", cb = T, year = census.year) %>%
  sf::st_drop_geometry() %>%
  .[,c(5,6,8,9)]
co_coc.cw            <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/crosswalks/county_district_region_crosswalk.csv") 
co_coc.cw$coc        <- unlist(lapply(X = strsplit(co_coc.cw$`Coc/Region`, " Region "), FUN = first))
co_coc.cw$region.coc <- as.numeric(unlist(lapply(X = strsplit(co_coc.cw$`Coc/Region`, " Region "), FUN = last)))
co_coc.cw <- co_coc.cw[,c(2,5,6)]

nc.counties <- full_join(all.nc.counties, 
          co_coc.cw,
           by = c("NAME" = "County")) %>% as_tibble()

# cleanup
rm(co_coc.cw, all.nc.counties)
