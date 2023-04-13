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

acs5_vars.cw <- rbind(data.frame(year = 2020, 
                              variable = c("B01003_001E", # total population
                                           "B06012_001E", # in poverty
                                           "B02001_001E", # race - total 
                                           "B02001_002E", # race - white alone 
                                           "B02001_003E", # race - black or african american alone 
                                           "B02001_004E", # race - American Indian and Alaska Native alone
                                           "B02001_005E", # race - American Indian and Alaska Native alone 
                                           "B02001_006E", # race - Native Hawaiian and Other Pacific Islander alone 
                                           "B02001_007E", # race - some other race alone 
                                           "B02001_008E", # race - two or more races
                                           "B02001_009E", # race - two or more races, including some other race
                                           "B02001_010E", # race - two or more races, excluding some other race, and three or more races 
                                           "B01001_001E", # total - sex by age 
                                           "B01001_002E", # Male - total 
                                           "B01001_003E", # Male - 0-4   years old
                                           "B01001_004E", # Male - 5-9   years old
                                           "B01001_005E", # Male - 10-14 years old
                                           "B01001_006E", # Male - 15-17 years old
                                           "B01001_007E", # Male - 18-19 years old
                                           "B01001_008E", # Male - 20    years old
                                           "B01001_009E", # Male - 21    years old
                                           "B01001_010E", # Male - 22-24  years old
                                           "B01001_026E", # Female - total 
                                           "B01001_027E", # Female - 0-4   years old
                                           "B01001_028E", # Female - 5-9   years old
                                           "B01001_029E", # Female - 10-14 years old
                                           "B01001_030E", # Female - 15-17 years old
                                           "B01001_031E", # Female - 18-19 years old
                                           "B01001_032E", # Female - 20    years old
                                           "B01001_033E", # Female - 21    years old
                                           "B01001_034E"  # Female - 22-24  years old
                              ), 
                              var_def  = c("B01003_001E", # "total population",
                                           "B06012_001E", # "in poverty",
                                           "B02001_001E", # "race - total",
                                           "B02001_002E", # "race - white alone",
                                           "B02001_003E", # "race - black or african american alone",
                                           "B02001_004E", # "race - American Indian and Alaska Native alone",
                                           "B02001_005E", # "race - American Indian and Alaska Native alone", 
                                           "B02001_006E", # "race - Native Hawaiian and Other Pacific Islander alone", 
                                           "B02001_007E", # "race - some other race alone", 
                                           "B02001_008E", # "race - two or more races",
                                           "B02001_009E", # "race - two or more races, including some other race",
                                           "B02001_010E", # "race - two or more races, excluding some other race, and three or more races", 
                                           "B01001_001E", # "total - sex by age", 
                                           "B01001_002E", # "Male - total", 
                                           "B01001_003E", # "Male - 0-4   years old",
                                           "B01001_004E", # "Male - 5-9   years old",
                                           "B01001_005E", # "Male - 10-14 years old",
                                           "B01001_006E", # "Male - 15-17 years old",
                                           "B01001_007E", # "Male - 18-19 years old",
                                           "B01001_008E", # "Male - 20    years old",
                                           "B01001_009E", # "Male - 21    years old",
                                           "B01001_010E", # "Male - 22-24  years old",
                                           "B01001_026E", # "Female - total", 
                                           "B01001_027E", # "Female - 0-4   years old",
                                           "B01001_028E", # "Female - 5-9   years old",
                                           "B01001_029E", # "Female - 10-14 years old",
                                           "B01001_030E", # "Female - 15-17 years old",
                                           "B01001_031E", # "Female - 18-19 years old",
                                           "B01001_032E", # "Female - 20    years old",
                                           "B01001_033E", # "Female - 21    years old",
                                           "B01001_034E"  # "Female - 22-24  years old"
                              )), 
                   data.frame(year = 2019, 
                              variable = c(NA), 
                              var_def  = c(NA))) %>%
  as_tibble()

acs5_vars.cw

tidycensus::get_acs(geography = "county", 
                    variables  = c("B01003_001E", # total population
                                   "B06012_001E", # in poverty
                                   "B02001_001E", # race - total 
                                   "B02001_002E", # race - white alone 
                                   "B02001_003E", # race - black or african american alone 
                                   "B02001_004E", # race - American Indian and Alaska Native alone
                                   "B02001_005E", # race - American Indian and Alaska Native alone 
                                   "B02001_006E", # race - Native Hawaiian and Other Pacific Islander alone 
                                   "B02001_007E", # race - some other race alone 
                                   "B02001_008E", # race - two or more races
                                   "B02001_009E", # race - two or more races, including some other race
                                   "B02001_010E", # race - two or more races, excluding some other race, and three or more races 
                                   "B01001_001E", # total - sex by age 
                                   "B01001_002E", # Male - total 
                                   "B01001_003E", # Male - 0-4   years old
                                   "B01001_004E", # Male - 5-9   years old
                                   "B01001_005E", # Male - 10-14 years old
                                   "B01001_006E", # Male - 15-17 years old
                                   "B01001_007E", # Male - 18-19 years old
                                   "B01001_008E", # Male - 20    years old
                                   "B01001_009E", # Male - 21    years old
                                   "B01001_010E", # Male - 22-24  years old
                                   "B01001_026E", # Female - total 
                                   "B01001_027E", # Female - 0-4   years old
                                   "B01001_028E", # Female - 5-9   years old
                                   "B01001_029E", # Female - 10-14 years old
                                   "B01001_030E", # Female - 15-17 years old
                                   "B01001_031E", # Female - 18-19 years old
                                   "B01001_032E", # Female - 20    years old
                                   "B01001_033E", # Female - 21    years old
                                   "B01001_034E"  # Female - 22-24  years old
                                   ), 
                    year   = 2020, 
                    state  = "NC", 
                    survey = "acs5")


# # acs1p_vars <- tidycensus::load_variables(2019, "acs1/profile")
# # acs1_vars  <- tidycensus::load_variables(2019, "acs1")
# 
# search.ptrns <- c("total population") %>% 
#   paste(., sep = "|", collapse = "|")
# 
# grep(search.ptrns, x = acs5p_vars$concept, ignore.case = T, value = T) %>% unique
# grep(search.ptrns, x = acs5_vars$concept,  ignore.case = T, value = T) %>% unique
# 
# grep(search.ptrns, x = acs5p_vars$label,   ignore.case = T, value = T) %>% unique
# grep(search.ptrns, x = acs5_vars$label,    ignore.case = T, value = T) %>% unique
# 
# acs5_vars[grepl("^TOTAL POPULATION$", acs5_vars$concept),]
# acs5_vars[grepl("UNWEIGHTED TOTAL POPULATION SAMPLE", acs5_vars$concept),]
# 
# acs5_vars$concept %>% unique
