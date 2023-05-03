library(tidycensus)
library(tigris)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)

rm(list=ls()[!ls() %in% c("vars.19", "vars.20")]);cat('\f');gc()

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/R")


# Funs----

# Vars----
youth.max.age <- 24

# Data----
master.data  <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_data.csv")
master.table <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_source_info.csv")
#raw.data     <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/raw_county_data.csv")

# get vars----
if(! "vars.19" %in% ls()){
  vars.19 <- tidycensus::load_variables(2019, "acs5") %>%
    .[!colnames(.) %in% c("geography")]
}
if(! "vars.20" %in% ls()){
  vars.20 <- tidycensus::load_variables(2020, "acs5") %>%
    .[!colnames(.) %in% c("geography")]
}


df.hud <- expand.grid(pop_hud      = c("All_People", "Youth", "Veteran"), 
                      race_eth_hud = c("Asian/Pacific Islander", #
                                       "Black", #
                                       "Native American/Alaskan", #
                                       "White", #
                                       "Other/Multi-Racial", #
                                       "Hispanic", 
                                       "Not Hispanic"),
                      data_hud     = c("TOTAL", 
                                       "TOTAL.fwc", 
                                       "in_POV", 
                                       "in_POV.fwc"),
                      stringsAsFactors = F) %>%
  as_tibble()

# race_eth regex
grep(" ALONE\\)$| RACES\\)$| LATINO\\)$", vars.19$concept, value = T) %>%
  unique()

census.raceeth <- data.frame(race_census  = c("WHITE ALONE", 
                                              "BLACK OR AFRICAN AMERICAN ALONE", 
                                              "AMERICAN INDIAN AND ALASKA NATIVE ALONE", 
                                              "ASIAN ALONE", 
                                              "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", 
                                              "SOME OTHER RACE ALONE", 
                                              "TWO OR MORE RACES", 
                                              "WHITE ALONE, NOT HISPANIC OR LATINO", 
                                              "HISPANIC OR LATINO"), 
                             race_hud     = c("White", 
                                              "Black", 
                                              "Native American/Alaskan", 
                                              "Asian/Pacific Islander",
                                              "Asian/Pacific Islander", 
                                              "Other/Multi-Racial",
                                              "Other/Multi-Racial",
                                              "Not Hispanic", 
                                              "Hispanic")) %>%
  as_tibble()



# census.veteran <- data.frame(vet_census   = unique(grep("^SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER", 
#                                                         vars.19$concept, value = T)), 
#                              vet_hud      = c("Veteran"), 
#                              race_census  = gsub("^.*\\(|\\)$|SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER", "", 
#                                                  unique(grep("^SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER", 
#                                                              vars.19$concept, value = T))),
#                              census_loc   = "concept") %>% as_tibble()

veteran <- c("VETERAN STATUS", "!!Veteran:", "!!Nonveteran:", "!!Veteran", "!!Nonveteran")
youth   <- c("!!18 and 19 years", 
             "!!25 to 39 years" ,
             "!!5 to 17 years", 
             "!!18 to 64 years", 
             "!!18 years and over", 
             "!!65 to 74 years", 
             "!!55 to 64 years", 
             "!!45 to 54 years", 
             "!!35 to 44 years", 
             "!!25 to 34 years", 
             "!!18 to 24 years", 
             "!!16 and 17 years", 
             "!!12 to 14 years", 
             "!!6 to 11 years", 
             "!!75 years and over", 
             "!!18 to 34 years", 
             "!!35 to 54 years", 
             "!!65 years and over", 
             "!!62 to 64 years", 
             "!!60 and 61 years", 
             "!!55 to 59 years", 
             "!!Under 15 years", 
             "!!15 to 17 years", 
             "!!75 to 84 years", 
             "!!85 years and over", 
             "!!19 to 34 years", 
             "!!Under 19 years", 
             "!!26 to 64 years", 
             "!!Under 18 years", 
             "!!16 to 64 years", 
             "!!19 to 64 years", 
             "!!35 to 64 years", 
             "!!18 to 29 years", 
             "!!30 to 44 years", 
             "!!45 to 64 years",
             "!!Under 5 years", "!!5 to 9 years", "!!10 to 14 years", "!!22 to 24 years", "!!25 to 29 years", "!!30 to 34 years", 
             "!!35 to 39 years", "!!40 to 44 years", "!!45 to 49 years", "!!50 to 54 years", "!!65 and 66 years", "!!67 to 69 years", 
             "!!70 to 74 years", "!!75 to 79 years", "!!80 to 84 years", "!!20 to 24 years", "!!Under 6 years:", "!!Under 6 years", 
             "!!6 to 17 years:", "!!6 to 17 years", "!!15 to 19 years", "!!60 to 64 years", "!!65 to 69 years", "!!Under 10 years", 
             "!!10 to 19 years", "!!20 to 29 years", "!!30 to 39 years", "!!40 to 49 years", "!!50 to 59 years", "!!60 to 69 years",
             "!!70 years and over", "!!1 to 4 years", "!!16 to 19 years", "!!25 to 44 years", "!!Under 3 years", "!!3 and 4 years", 
             "!!6 to 8 years", "!!9 to 11 years", "!!12 to 17 years", "!!30 to 59 years", "!!60 years and over", "!!Under 6 years only", 
             "!!Under 6 years and 6 to 17 years", "!!6 to 17 years only", "!!15 to 19 years old", "!!20 to 34 years old", 
             "!!35 to 50 years old", "!!20 to 24 years old", "!!25 to 29 years old", "!!30 to 34 years old", "!!35 to 39 years old", 
             "!!40 to 44 years old", "!!45 to 50 years old", "!!35 years and over", "!!40 to 64 years:", "!!40 to 64 years", 
             "!!Under 5 years only", "!!Under 5 years and 5 to 17 years", "!!18 to 59 years", "!!60 to 74 years", "!!12 to 17 years:", 
             "!!Under 5 years:", "!!16 to 19 years:", "!!20 and 21 years:", "!!20 and 21 years", "!!22 to 24 years:", 
             "!!25 to 29 years:", "!!30 to 34 years:", "!!65 to 69 years:", "!!70 to 74 years:", "!!Under 6 years only:", 
             "!!Under 6 years and 6 to 17 years:", "!!6 to 17 years only:", "!!20 to 24 years:", "!!25 to 44 years:", 
             "!!70 years and over:", "!!6 to 18 years:", "!!6 to 18 years", "!!19 to 25 years:", "!!19 to 25 years", "!!26 to 34 years:", 
             "!!26 to 34 years") %>%
  tolower() %>%
  gsub("years old|years only", "years", .) %>% 
  gsub("^!!|:$", "", .) %>%
  unique()

poverty <- c("POVERTY STATUS IN THE PAST 12 MONTHS BY AGE", 
             "POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN")
fwc     <- c("FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS", 
             "FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN UNDER 18 YEARS", 
             "POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN")



vars.19[vars.19$label == "Estimate!!Total:" ,] %>%
  .[grepl(paste(fwc, sep = "|", collapse = "|"), .$concept, ignore.case = T),]


# grep(pattern = "children of the householder under 18 years", 
#      x = vars.19$label, 
#      ignore.case = T, 
#      value = T) %>% unique() 
# 
# vars.19[grepl("children of the householder under 18 years", vars.19$label, ignore.case = T),]$concept %>% 
#   unique() %>%
#   .[!grepl("household", ., ignore.case = T)]
# 
# grep(pattern = "!!with related children|!!with own children", 
#      x = vars.19$label, 
#      ignore.case = T, 
#      value = T) %>% unique()
# 
# vars.19$label[grepl(paste(poverty, sep = "|", collapse = "|"), 
#                     x = vars.19$concept)] %>%
#   unique()
# 
# grep(pattern = "^poverty status in the past 12 months of individuals|^poverty status in the past 12 months of families by family type by presence" , 
#      x = vars.19$concept, 
#      ignore.case = T, 
#      value = T) %>% unique()
# 
# vars.19[grepl(pattern = "family type.*own children|family type.*related children", x = vars.19$concept, ignore.case = T) & 
#           grepl(pattern = "", x = vars.19$label),]$concept %>% unique()

youth.df <- data.frame(census_term = youth,
                       min_age.inc = NA,
                       max_age.inc = NA) %>%
  as_tibble()

youth.df[grepl("^\\d{1,2} to", youth.df$census_term),]$min_age.inc <- youth.df[grepl("^\\d{1,2} to", youth.df$census_term),]$census_term %>%
  strsplit(., " to | years") %>%
  lapply(., as.numeric) %>%
  lapply(., min) %>%
  unlist()
youth.df[grepl("^\\d{1,2} to", youth.df$census_term),]$max_age.inc <- youth.df[grepl("^\\d{1,2} to", youth.df$census_term),]$census_term %>%
  strsplit(., " to | years") %>%
  lapply(., as.numeric) %>%
  lapply(., max) %>%
  unlist()

youth.df[grepl("^\\d{1,2} and", youth.df$census_term),]$min_age.inc <- youth.df[grepl("^\\d{1,2} and", youth.df$census_term),]$census_term %>%
  strsplit(., " and | years") %>%
  lapply(., as.numeric) %>%
  lapply(., min) %>%
  unlist()
youth.df[grepl("^\\d{1,2} and", youth.df$census_term),]$max_age.inc <- youth.df[grepl("^\\d{1,2} and", youth.df$census_term),]$census_term %>%
  strsplit(., " and | years") %>%
  lapply(., as.numeric) %>%
  lapply(., max) %>%
  unlist()

youth.df[grepl("^under \\d{1,2} years and \\d{1,2} to \\d{1,2} years$",
               youth.df$census_term),]$min_age.inc <- 0
youth.df[grepl("^under \\d{1,2} years and \\d{1,2} to \\d{1,2} years$",
               youth.df$census_term),]$max_age.inc <- youth.df[grepl("^under \\d{1,2} years and \\d{1,2} to \\d{1,2} years$",
                                                                     youth.df$census_term),]$census_term %>%
  strsplit(split = "\\D|\\W", x=.) %>%
  lapply(., as.numeric) %>%
  lapply(., max, na.rm = T) %>%
  unlist()

youth.df[grepl("^under \\d{1,2} years$",
               youth.df$census_term),]$min_age.inc <- 0
youth.df[grepl("^under \\d{1,2} years$",
               youth.df$census_term),]$max_age.inc <- as.numeric(gsub("under|years| ", "", youth.df[grepl("^under \\d{1,2} years$",
                                                      youth.df$census_term),]$census_term)) - 1

youth.df[grepl("years and over$",
               youth.df$census_term),]$max_age.inc <- 99
youth.df[grepl("years and over$",
               youth.df$census_term),]$min_age.inc <- youth.df[grepl("years and over$",
               youth.df$census_term),]$census_term %>%
  gsub(pattern = " years and over$", replacement = "", x = .) %>%
  as.numeric()

youth.df <- youth.df %>%
  mutate(.,
         def_youth = min_age.inc <= youth.max.age &
           max_age.inc <= youth.max.age) #%>%.[.$def_youth,]


youth.df <- youth.df %>%
  .[order(.$min_age.inc, .$max_age.inc),]

# # ggplot() + 
# #   geom_segment(data = youth.df, 
# #                aes(x = min_age.inc, 
# #                    xend = max_age.inc, 
# #                    y = census_term, 
# #                    yend = census_term, 
# #                    color = def_youth))+
# #   geom_vline(aes(xintercept = youth.max.age))
# # 
# # census.youth   <- data.frame(youth_census = c(NA), 
# #                              youth_hud    = c(NA)) %>% as_tibble()
# # 
# # census.poverty <- data.frame(pov_census   = c(NA), 
# #                              pov_hud      = c(NA)) %>% as_tibble()
# # 
# # census.fwc     <- data.frame(fwc_census   = c(NA), 
# #                              fwc_hud      = c(NA)) %>% as_tibble()
# # 
# # 
# # grep(pattern = "\\Wby age\\W|\\Wage by\\W", 
# #      x = vars.19$concept, 
# #      ignore.case = T, 
# #      value = T) %>% unique()
# # 
# # grep(pattern = "!!\\d{1,2} to \\d{1,2} years|!!\\d{1,2} and \\d{1,2} years|!!\\d{1,2} years and over|!!under \\d{1,2} years", 
# #      x = vars.19$label, 
# #      ignore.case = T, 
# #      value = T) %>% unique() %>%
# #   .[!grepl(paste(youth, sep = "|", collapse = "|"), x = .)] # %>% 
# #   
# # strsplit(x = ., 
#   #          split = ":!!|!!") %>%
#   # unlist() %>% unique() %>%
#   # paste("!!", ., sep = "") %>%
#   # grep(pattern = "!!\\d{1,2} to \\d{1,2} years|!!\\d{1,2} and \\d{1,2} years|!!\\d{1,2} years and over|!!under \\d{1,2} years", 
#   #      x = ., 
#   #      ignore.case = T, 
#   #      value = T) %>% 
#   # unique() %>% 
#   # paste(., 
#   #       sep = "\", \"",
#   #       collapse = "\", \"") %>% 
#   # cat()
#   
# 
# 


# Tidy----

# Analysis----

# Visualizations----

# Outputs-----