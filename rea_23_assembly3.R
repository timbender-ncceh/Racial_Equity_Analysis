# Bos REA
# copy pasta 2022 charts

# https://www.ncceh.org/files/12525/

library(dplyr)
library(tidycensus)
library(lubridate)
library(ggplot2)
library(readr)
library(openxlsx)

rm(list=ls()[!ls() %in% 
               c("vars.21")])
cat('\f');gc()

# SetWD----
prime.wd    <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis"
hudtools.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/hud_tools"
data.wd     <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/data"
r_code.wd   <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/R"

setwd(prime.wd)

# Funs----
gen_coname <- function(NAME1){
  return(gsub(" County, .*$", "", NAME1))
}

# Vars----
var.ct <- F # cache tables? 
var.year <- 2021

gh.census.data <- "https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/raw_county_data.csv"
gh.census.ref  <- "https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_source_info.csv"
gh.cw_coc_co   <- "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/crosswalks/coc_county_crosswalk.csv"
gh.cw_bosco_reg <- "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/crosswalks/regionscrosswalk.csv"
# Setup----
if(!"vars.21" %in% ls()){
  vars.21 <- tidycensus::load_variables(year = var.year, 
                                        dataset = "acs5")  
}

# Import----
ghd   <- read_csv(gh.census.data)
ghref <- read_csv(gh.census.ref)
cw.coc_co <- read_csv(gh.cw_coc_co)
cw.co_reg <- read_csv(gh.cw_bosco_reg)

# Tidy----
ghd$county <- gsub(" County, .*$", "", ghd$NAME)
ghd <- left_join(ghd, 
          vars.21[,c("name", "label", "concept")], 
          by = c("variable" = "name"))


# REPRODUCE CHARTS----

# race / ethnicity----

# total pop
vars.21[grepl(pattern = "^SEX BY AGE$", x = vars.21$concept) & 
          grepl(pattern = "^Estimate!!Total:$", x = vars.21$label),]


B01001_001.nc <- get_acs(geography = "state", 
                         variables = "B01001_001", 
                         table     = NULL, 
                         cache_table = var.ct, 
                         year        = var.year, 
                         state       = "NC", 
                         survey      = "acs5") %>%
  mutate(., 
         geo = "state",
         geo_name = gen_coname(NAME), 
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name"))


B01001_001.co <- get_acs(geography = "county", 
                         variables = "B01001_001", 
                         table     = NULL, 
                         cache_table = var.ct, 
                         year        = var.year, 
                         state       = "NC", 
                         survey      = "acs5") %>%
  mutate(., 
         geo = "county",
         geo_name = gen_coname(NAME), 
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name"))

# pop by race
vars.21[grepl(pattern = "^SEX BY AGE \\(.*\\)$", x = vars.21$concept) & 
  grepl(pattern = "^Estimate!!Total:$", x = vars.21$label) & 
    !grepl(pattern = "HISPANIC", x = vars.21$concept),]

B01001R_001.nc <- get_acs(geography = "state", 
                          variables = c("B01001A_001", 
                                        "B01001B_001", 
                                        "B01001C_001", 
                                        "B01001D_001", 
                                        "B01001E_001", 
                                        "B01001F_001", 
                                        "B01001G_001"), 
                          table     = NULL, 
                          cache_table = var.ct, 
                          year        = var.year, 
                          state       = "NC", 
                          survey      = "acs5") %>%
  mutate(., 
         geo = "state",
         geo_name = gen_coname(NAME), 
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name"))

B01001R_001.co <- get_acs(geography = "county", 
                          variables = c("B01001A_001", 
                                        "B01001B_001", 
                                        "B01001C_001", 
                                        "B01001D_001", 
                                        "B01001E_001", 
                                        "B01001F_001", 
                                        "B01001G_001"), 
                          table     = NULL, 
                          cache_table = var.ct, 
                          year        = var.year, 
                          state       = "NC", 
                          survey      = "acs5") %>%
  mutate(., 
         geo = "county",
         geo_name = gen_coname(NAME), 
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name"))


# pop by eth
vars.21[grepl(pattern = "^SEX BY AGE \\(.*\\)$", x = vars.21$concept) & 
          grepl(pattern = "^Estimate!!Total:$", x = vars.21$label) & 
          grepl(pattern = "HISPANIC", x = vars.21$concept),]

B01001E_001.nc <- get_acs(geography = "state", 
                          variables = c("B01001H_001", 
                                        "B01001I_001"), 
                          table     = NULL, 
                          cache_table = var.ct, 
                          year        = var.year, 
                          state       = "NC", 
                          survey      = "acs5") %>%
  mutate(., 
         geo = "state",
         geo_name = gen_coname(NAME), 
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name"))

B01001E_001.co <- get_acs(geography = "county", 
                          variables = c("B01001H_001", 
                                        "B01001I_001"), 
                          table     = NULL, 
                          cache_table = var.ct, 
                          year        = var.year, 
                          state       = "NC", 
                          survey      = "acs5") %>%
  mutate(., 
         geo = "county",
         geo_name = gen_coname(NAME), 
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name"))



# poverty----

# race
ghref %>%
  .[.$population == "All_People",] %>%
  .[!.$subpopulation %in% c("Hispanic", "Not Hispanic", "TOTAL"),] %>%
  group_by(population, 
            subpopulation,
           ALL.PPL_total,
           ALL.PPL_in.fwc,
           IN.POV_total, 
           IN.POV_in.fwc,
           EXP.HL_total,
           EXP.HL_in.fwc) %>%
  summarise()

# ethnicity
ghref %>%
  .[.$population == "All_People",] %>%
  .[.$subpopulation %in% c("Hispanic", "Not Hispanic", "TOTAL"),] %>%
  group_by(population, 
           subpopulation,
           ALL.PPL_total,
           ALL.PPL_in.fwc,
           IN.POV_total, 
           IN.POV_in.fwc,
           EXP.HL_total,
           EXP.HL_in.fwc) %>%
  summarise()

# youth----

# by race

# by ethnicity


# veterans----

# by race

# by ethnicity


# Race over time----


# Ethnicity over Time----

# Race and Ethnicity of Heads of Households and Adults----

# Average Length of Stay by Race, FY22----

# Average Length of Stay by Race and Ethnicity, FY22----

# more stellaP data VVV (see report)
