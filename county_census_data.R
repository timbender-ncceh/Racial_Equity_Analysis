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

acs5_2019.vars[acs5_2019.vars$name == "B01001_001",]
acs5_2019.vars[grepl("\\(hispanic or latino\\)", acs5_2019.vars$concept, ignore.case = T),]$concept %>% unique()
acs5_2019.vars[grepl("white alone", acs5_2019.vars$concept, ignore.case = T),]$concept %>% unique()

acs5_2019.vars[grepl("white alone", acs5_2019.vars$label, ignore.case = T),]$concept %>% unique()
acs5_2019.vars[grepl("^Estimate!!Total:", acs5_2019.vars$label, ignore.case = T),]$concept %>%
  grep("^poverty", ., ignore.case = T, value = T) %>%
  unique()

acs5_2019.vars[grepl("POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE \\(WHITE ALONE\\)",
                     acs5_2019.vars$concept, ignore.case = T),] %>% .$name 



acs5_vars.cw <- rbind(data.frame(year = 2020, 
                                 variable = c("B01003_001E", # total population
                                              NA, # in poverty
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
                                              "B01001_034E",  # Female - 22-24  years old
                                              "B03002_001E",
                                              "B03002_002E",
                                              "B03002_003E",
                                              "B03002_004E",
                                              "B03002_005E",
                                              "B03002_006E",
                                              "B03002_007E",
                                              "B03002_008E",
                                              "B03002_009E",
                                              "B03002_010E",
                                              "B03002_011E",
                                              "B03002_012E",
                                              "B03002_013E",
                                              "B03002_014E",
                                              "B03002_015E",
                                              "B03002_016E",
                                              "B03002_017E",
                                              "B03002_018E",
                                              "B03002_019E",
                                              "B03002_020E",
                                              "B03002_021E"
                                              
                                 ), 
                                 var_def  = c("total population",
                                              "in poverty",
                                              "race - total",
                                              "race - white alone",
                                              "race - black or african american alone",
                                              "race - American Indian and Alaska Native alone",
                                              "race - American Indian and Alaska Native alone", 
                                              "race - Native Hawaiian and Other Pacific Islander alone", 
                                              "race - some other race alone", 
                                              "race - two or more races",
                                              "race - two or more races, including some other race",
                                              "race - two or more races, excluding some other race, and three or more races", 
                                              "total - sex by age", 
                                              "Male - total", 
                                              "Male - 0-4   years old",
                                              "Male - 5-9   years old",
                                              "Male - 10-14 years old",
                                              "Male - 15-17 years old",
                                              "Male - 18-19 years old",
                                              "Male - 20    years old",
                                              "Male - 21    years old",
                                              "Male - 22-24  years old",
                                              "Female - total", 
                                              "Female - 0-4   years old",
                                              "Female - 5-9   years old",
                                              "Female - 10-14 years old",
                                              "Female - 15-17 years old",
                                              "Female - 18-19 years old",
                                              "Female - 20    years old",
                                              "Female - 21    years old",
                                              "Female - 22-24  years old",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: white alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: black or african american alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: american indian or alaska native alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: asian alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: native hawaiian and other pacific islander alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: some other race alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: two or more races",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: two races including some other race", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: two races excluding some other race, and three or more races", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: white alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: black or african american alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: american indian and alaska native alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: asian alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: native hawaiian and other pacific islander alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: some other race alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: two or more races", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: two or more races including some other race", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: two or more races excluding some other race, and three or more races"
                                 )), 
                      data.frame(year = 2019, 
                                 variable = c("B01003_001E", # total population
                                              NA, # in poverty
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
                                              "B01001_034E",  # Female - 22-24  years old
                                              "B03002_001E",
                                              "B03002_002E",
                                              "B03002_003E",
                                              "B03002_004E",
                                              "B03002_005E",
                                              "B03002_006E",
                                              "B03002_007E",
                                              "B03002_008E",
                                              "B03002_009E",
                                              "B03002_010E",
                                              "B03002_011E",
                                              "B03002_012E",
                                              "B03002_013E",
                                              "B03002_014E",
                                              "B03002_015E",
                                              "B03002_016E",
                                              "B03002_017E",
                                              "B03002_018E",
                                              "B03002_019E",
                                              "B03002_020E",
                                              "B03002_021E"
                                 ), 
                                 var_def  = c("total population",
                                              "in poverty",
                                              "race - total",
                                              "race - white alone",
                                              "race - black or african american alone",
                                              "race - American Indian and Alaska Native alone",
                                              "race - American Indian and Alaska Native alone", 
                                              "race - Native Hawaiian and Other Pacific Islander alone", 
                                              "race - some other race alone", 
                                              "race - two or more races",
                                              "race - two or more races, including some other race",
                                              "race - two or more races, excluding some other race, and three or more races", 
                                              "total - sex by age", 
                                              "Male - total", 
                                              "Male - 0-4   years old",
                                              "Male - 5-9   years old",
                                              "Male - 10-14 years old",
                                              "Male - 15-17 years old",
                                              "Male - 18-19 years old",
                                              "Male - 20    years old",
                                              "Male - 21    years old",
                                              "Male - 22-24  years old",
                                              "Female - total", 
                                              "Female - 0-4   years old",
                                              "Female - 5-9   years old",
                                              "Female - 10-14 years old",
                                              "Female - 15-17 years old",
                                              "Female - 18-19 years old",
                                              "Female - 20    years old",
                                              "Female - 21    years old",
                                              "Female - 22-24  years old",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: white alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: black or african american alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: american indian or alaska native alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: asian alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: native hawaiian and other pacific islander alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: some other race alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: two or more races",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: two races including some other race", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - not hispanic or latino: two races excluding some other race, and three or more races", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: white alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: black or african american alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: american indian and alaska native alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: asian alone", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: native hawaiian and other pacific islander alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: some other race alone",
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: two or more races", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: two or more races including some other race", 
                                              "HISPANIC OR LATINO ORIGIN BY RACE - total - hispanic or latino: two or more races excluding some other race, and three or more races"
                                 ))) %>%
  as_tibble() 

acs5_vars.cw$var_def <- acs5_vars.cw$var_def %>%
  gsub(pattern = " {2,}", " ", .) %>%
  trimws() %>% tolower()

acs5_vars.cw$var_prefix <- acs5_vars.cw$variable %>% strsplit(., "_") %>% lapply(., first) %>% unlist
acs5_vars.cw$var_suffix <- acs5_vars.cw$variable %>% strsplit(., "_") %>% lapply(., last) %>% unlist

# download data----
data.2019_NC <- tidycensus::get_acs(geography = "state", 
                    variables  = acs5_vars.cw$variable[acs5_vars.cw$year == 2019], 
                    year   = 2019, 
                    state  = "NC", 
                    survey = "acs5") 

data.2019_NC <- full_join(data.2019_NC, 
                          mutate(acs5_vars.cw[acs5_vars.cw$year == 2019,], 
                                 variable = gsub("E$", "", variable)))

data.2020_NC <- tidycensus::get_acs(geography = "state", 
                                    variables  = acs5_vars.cw$variable[acs5_vars.cw$year == 2020], 
                                    year   = 2020, 
                                    state  = "NC", 
                                    survey = "acs5")

# Tidy data----
# all people, all races
data.2019_NC[data.2019_NC$var_def == "total population",]$estimate %>% scales::comma()
# all people in poverty, all races
data.2019_NC[data.2019_NC$var_def == "in poverty",]$estimate %>% scales::comma()
