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

acs5_2019.vars <- tidycensus::load_variables(2019, "acs5")

# acs5_2019.vars[acs5_2019.vars$name == "B01001_001",]
# acs5_2019.vars[grepl("\\(hispanic or latino\\)", acs5_2019.vars$concept, ignore.case = T),]$concept %>% unique()
# acs5_2019.vars[grepl("white alone", acs5_2019.vars$concept, ignore.case = T),]$concept %>% unique()
# 
# acs5_2019.vars[grepl("white alone", acs5_2019.vars$label, ignore.case = T),]$concept %>% unique()
# acs5_2019.vars[grepl("^Estimate!!Total:", acs5_2019.vars$label, ignore.case = T),]$concept %>%
#   grep("^poverty", ., ignore.case = T, value = T) %>%
#   unique()

variables_list_2019 <- c(acs5_2019.vars[grepl("POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE", #"POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE \\(WHITE ALONE\\)",
                                            acs5_2019.vars$concept, ignore.case = T) & 
                                        grepl("^Estimate!!Total:$|^EStimate!!Total:!!Income in the past 12 months below poverty level:$", 
                                              acs5_2019.vars$label, ignore.case = T),]$name, 
                         acs5_2019.vars[grepl(paste(c("B01003_", "B02001_", "B01001_", "B03002_"), 
                                                    sep = "|", collapse = "|"), 
                                              acs5_2019.vars$name, 
                                              ignore.case = T),]$name) 
variables_list_2020 <- NA


# download data----
data.2019_NC <- tidycensus::get_acs(geography = "state", 
                    variables  = variables_list_2019, 
                    year   = 2019, 
                    state  = "NC", 
                    survey = "acs5") 

data.2019_NC <- full_join(data.2019_NC, 
                          NA)

data.2020_NC <- tidycensus::get_acs(geography = "state", 
                                    variables  = variables_list_2020, 
                                    year   = 2020, 
                                    state  = "NC", 
                                    survey = "acs5")

# Tidy data----
# all people, all races
data.2019_NC[data.2019_NC$var_def == "total population",]$estimate %>% scales::comma()
# all people in poverty, all races
data.2019_NC[data.2019_NC$var_def == "in poverty",]$estimate %>% scales::comma()
