# Pull County US Census Data
library(data.table)
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

# Master Table Design----

expand.grid(race = c(letters[1:5], LETTERS[6:7]),
            FWC = c("in_fam_w_children","not_in_fam_w_children"),
            PS = c("in_poverty", "not_in_poverty"),
            #count_by = c("ALL_Pop", "In_fam_w_children"),
            VET = c("is_vet", "not_vet"),
            AGE = c("is_youth", "not_youth"),
            n = 0, 
            stringsAsFactors = F) %>%
  as_tibble() %>%
  .[ !(.$VET == "is_vet" &  .$AGE == "is_youth"),] %>%
  .[ !(.$FWC == "in_fam_w_children" & .$AGE == "is_youth"),] %>%
  as.data.table() %>%
  dcast(., 
        VET + AGE + race ~ FWC + PS)


master.table <- expand.grid(population = c("All_People", "Youth", "Veteran"), 
                            year = c(2020),#,2019),
                            concept = factor(c("ALL.PPL", "IN.POV", "EXP.HL"),
                                          levels = c("ALL.PPL", "IN.POV", "EXP.HL")),
                            var3 = factor(c("total", "in.fwc"), 
                                          levels = c("total", "in.fwc")),
                            # measure = factor(c("#", "%"), 
                            #                  levels = c("#", "%")),
                            subpopulation = factor(c("TOTAL", 
                                          "Asian/Pacific Islander",
                                          "Black",
                                          "Native American/Alaskan",
                                          "White",
                                          "Other/Multi-Racial", 
                                          "Hispanic", 
                                          "Not Hispanic"), 
                                          levels = c("TOTAL", 
                                                     "Asian/Pacific Islander",
                                                     "Black",
                                                     "Native American/Alaskan",
                                                     "White",
                                                     "Other/Multi-Racial", 
                                                     "Hispanic", 
                                                     "Not Hispanic")),
                            n = 0,
                            table_name = NA,
                            stringsAsFactors = F) %>%
  as.data.table() %>%
  dcast(., 
        year + population + subpopulation ~ concept + var3) %>%
  as.data.frame() %>%
  as_tibble()


master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"] <- "B01001_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"] <- "B01001D_001 + B01001E_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Black"] <- "B01001B_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- "B01001C_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "White"] <- "B01001A_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"] <- "B01001F_001 + B01001G_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"] <- "B01001I_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"] <- "B01001_001 - B01001I_001"

master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "TOTAL"]                   <- "B21001_002"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "White"]                   <- "C21001A_011 + C21001A_014 + C21001A_004 + C21001A_007"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "C21001D_011 + C21001D_014 + C21001D_004 + C21001D_007 + C21001E_011 + C21001E_014 + C21001E_004 + C21001E_007"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "Black"]                   <- "C21001B_011 + C21001B_014 + C21001B_004 + C21001B_007"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "Native American/Alaskan"] <- "C21001C_011 + C21001C_014 + C21001C_004 + C21001C_007"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "Other/Multi-Racial"]      <- "C21001F_011 + C21001F_014 + C21001F_004 + C21001F_007 + C21001G_011 + C21001G_014 + C21001G_004 + C21001G_007"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "Hispanic"]                <- "C21001I_011 + C21001I_014 + C21001I_004 + C21001I_007"
master.table$ALL.PPL_total[master.table$population == "Veteran" & master.table$subpopulation == "Not Hispanic"]            <- "B21001_002 - (C21001I_011 + C21001I_014 + C21001I_004 + C21001I_007)"

master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- "B17010_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- "B17010A_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "B17010D_001 + B17010E_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- "B17010B_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- "B17010C_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- "B17010F_001 + B17010G_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- "B17010I_001"
master.table$ALL.PPL_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- "B17010_001 - B17010I_001"

master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "TOTAL"]                   <- "B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_007 + B01001_008 + B01001_009 + B01001_010 + B01001_027 + B01001_028 + B01001_029 + B01001_030 + B01001_031 + B01001_032 + B01001_033 + B01001_034"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "White"]                   <- "B01001A_003 + B01001A_004 + B01001A_005 + B01001A_006 + B01001A_007 + B01001A_008 + B01001A_018 + B01001A_019 + B01001A_020 + B01001A_021 + B01001A_022 + B01001A_023"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "B01001D_003 + B01001D_004 + B01001D_005 + B01001D_006 + B01001D_007 + B01001D_008 + B01001D_018 + B01001D_019 + B01001D_020 + B01001D_021 + B01001D_022 + B01001D_023 + B01001E_003 + B01001E_004 + B01001E_005 + B01001E_006 + B01001E_007 + B01001E_008 + B01001E_018 + B01001E_019 + B01001E_020 + B01001E_021 + B01001E_022 + B01001E_023"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "Black"]                   <- "B01001B_003 + B01001B_004 + B01001B_005 + B01001B_006 + B01001B_007 + B01001B_008 + B01001B_018 + B01001B_019 + B01001B_020 + B01001B_021 + B01001B_022 + B01001B_023"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "Native American/Alaskan"] <- "B01001C_003 + B01001C_004 + B01001C_005 + B01001C_006 + B01001C_007 + B01001C_008 + B01001C_018 + B01001C_019 + B01001C_020 + B01001C_021 + B01001C_022 + B01001C_023"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "Other/Multi-Racial"]      <- "B01001F_003 + B01001F_004 + B01001F_005 + B01001F_006 + B01001F_007 + B01001F_008 + B01001F_018 + B01001F_019 + B01001F_020 + B01001F_021 + B01001F_022 + B01001F_023 + B01001G_003 + B01001G_004 + B01001G_005 + B01001G_006 + B01001G_007 + B01001G_008 + B01001G_018 + B01001G_019 + B01001G_020 + B01001G_021 + B01001G_022 + B01001G_023"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "Hispanic"]                <- "B01001I_003 + B01001I_004 + B01001I_005 + B01001I_006 + B01001I_007 + B01001I_008 + B01001I_018 + B01001I_019 + B01001I_020 + B01001I_021 + B01001I_022 + B01001I_023"
master.table$ALL.PPL_total[master.table$population == "Youth" & master.table$subpopulation == "Not Hispanic"]            <- "(B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_007 + B01001_008 + B01001_009 + B01001_010 + B01001_027 + B01001_028 + B01001_029 + B01001_030 + B01001_031 + B01001_032 + B01001_033 + B01001_034) - (B01001I_003 + B01001I_004 + B01001I_005 + B01001I_006 + B01001I_007 + B01001I_008 + B01001I_018 + B01001I_019 + B01001I_020 + B01001I_021 + B01001I_022 + B01001I_023)"

master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- ""
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- ""
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- " + "
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- ""
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- ""
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- " + "
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- ""
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- " - "

master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- ""
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- ""
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- " + "
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- ""
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- ""
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- " + "
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- ""
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- " - "

master.table %>% as_tibble()

write_csv(master.table, file = "census_table_source_info.csv")


# acs5.2019.vars <- tidycensus::load_variables(year = 2019, 
#                                              dataset = "acs5") %>%
#   .[,c("name", "label", "concept")] %>%
#   .[!duplicated(.),] %>%
#   .[complete.cases(.),]

acs5.2020.vars <- tidycensus::load_variables(year = 2020, 
                                             dataset = "acs5") %>%
  .[,c("name", "label", "concept")] %>%
  .[!duplicated(.),] %>%
  .[complete.cases(.),]

# search terms vars----
youth.pattern.label <- c("!!Under 5 years", 
                         "!!5 to 9 years",
                         "!!10 to 14 years", 
                         "!!15 to 17 years", 
                         "!!18 and 19 years", 
                         "!!20 years", 
                         "!!21 years", 
                         "!!22 to 24 years", 
                         "!!20 to 24 years")


veteran.pattern.concepts <- NA


get_tables2 <- function(regex.concept, 
                        regex.label,
                        year1 = 2020,
                        dataset1 = "acs5"){
  require(dplyr)
  require(tidycensus)
  # pull master dataset
  df <- load_variables(year = year1, 
                       dataset = dataset1) %>%
    .[,c("name", "label", "concept")] %>%
    .[!duplicated(.),] %>%
    .[complete.cases(.),]
  
}

get_tables <- function(filter_race = c("literal hud race", FALSE, NA, NULL), 
                       filter_age  = c(T,F), 
                       filter_vet  = c(T,F)){
  require(dplyr) 
  require(glue)
  
  # RACE
  if(any(c(is.na(filter_race), 
           is.null(filter_race), 
           isFALSE(filter_race), 
           !tolower(filter_race) %in% tolower(c("White", 
                                                "Black", 
                                                "Native American/Alaskan", 
                                                "Asian/Pacific Islander", 
                                                "Other/Multi-Racial", 
                                                "Hispanic"))))){
    # do this 
    print("skip race")
    PATTERN.race <- NA
    
  }else{
    # do the that
    print("don't skip race")
    race.pattern.concepts <- data.frame(hud_term    = tolower(c("White", 
                                                        "Black", 
                                                        "Native American/Alaskan", 
                                                        "Asian/Pacific Islander", 
                                                        "Asian/Pacific Islander", 
                                                        "Other/Multi-Racial", 
                                                        "Other/Multi-Racial", 
                                                        "Hispanic")), 
                                        census_term = c("WHITE ALONE", 
                                                        "BLACK OR AFRICAN AMERICAN ALONE", 
                                                        "AMERICAN INDIAN AND ALASKA NATIVE ALONE", 
                                                        "ASIAN ALONE", 
                                                        "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", 
                                                        "SOME OTHER RACE ALONE",
                                                        "TWO OR MORE RACES", 
                                                        "HISPANIC OR LATINO"))
    filter_race.hud <- tolower(filter_race)
    PATTERN.race <-  paste("SEX BY AGE (", race.pattern.concepts$census_term[race.pattern.concepts$hud_term %in% filter_race.hud,], ")", sep = "")
    
    
  }
  
  # YOUTH
  if(filter_age){
    youth.pattern.label <- c("!!Under 5 years", 
                             "!!5 to 9 years",
                             "!!10 to 14 years", 
                             "!!15 to 17 years", 
                             "!!18 and 19 years", 
                             "!!20 years", 
                             "!!21 years", 
                             "!!22 to 24 years", 
                             "!!20 to 24 years")
    PATTERN.youth <- paste(youth.pattern.label, sep = "|", collapse = "|")
  }else{
    PATTERN.youth <- NA
  }
  
  # VETERAN
  if(filter_vet){
    print('do this')
    PATTERN.vet <- "???"
  }else{
    PATTERN.vet <- NA
  }
  
  # in family with children
  
  
  # poverty
  
}


race.pattern.concepts <- c("hispanic or latino")




acs5.2020.vars[grepl(pattern = paste(youth.pattern.label, sep = "|", collapse = "|"), 
                     x = acs5.2020.vars$label) & 
                 acs5.2020.vars$concept %in%
                 paste("SEX BY AGE (", toupper(race.pattern.concepts), ")", sep = ""),]$name %>% 
  paste(., sep = " + ", collapse = " + ")

acs5.2020.vars[grepl(pattern = "!!Under 5 years", x = acs5.2020.vars$label) & 
                 acs5.2020.vars$concept == "SEX BY AGE",]

acs5.2020.vars
"^SEX BY AGE$|^SEX BY AGE \\(.*\\)$"

acs5.2020.vars[grepl("^SEX BY AGE$|^SEX BY AGE \\(.*\\)$",acs5.2020.vars$concept),] %>%
  .[!grepl("years", .$label),] %>%
  .[!grepl("Male|Female", .$label),]

acs5.2020.vars[grepl("^SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER",acs5.2020.vars$concept),] %>%
  .[!grepl("years", .$label),] %>%
  .[!grepl("Male|Female", .$label),]

acs5.2020.vars[grepl("(WHITE ALONE)",acs5.2020.vars$concept) & 
                 grepl("!!Veteran", acs5.2020.vars$label),]

acs5.2020.vars[grepl("^C21001", acs5.2020.vars$name) & 
                 grepl("!!Veteran", acs5.2020.vars$label),]$concept %>% unique

acs5.2020.vars[grepl("^C21001", acs5.2020.vars$name) & 
                 grepl("!!Veteran", acs5.2020.vars$label) & 
                 grepl("\\(HISPANIC OR LATINO\\)", acs5.2020.vars$concept),] %>%
  .[order(.$label),] %>%
  .$name %>%
  paste(., sep = " + ", collapse = " + ")

acs5.2020.vars[grepl("FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS", acs5.2020.vars$concept) & 
                 grepl("^Estimate!!Total:!!Income in the past 12 months below poverty level:$", acs5.2020.vars$label),] %>% as_tibble() %>%
  .[,c("name", "concept"),]

acs5.2020.vars[grepl("FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS", acs5.2020.vars$concept),]$label %>% unique()


# search for data terms----
search.terms.df <- data.frame(data_source     = c("load_variables()"), 
                             concept_pattern = c("^SEX BY AGE$|^SEX BY AGE \\(.*\\)$", 
                                                 "^POVERTY STATUS IN THE PAST 12 MONTHS BY AGE$|^POVERTY STATUS IN THE PAST 12 MONTHS BY AGE.*\\(.*\\)$", 
                                                 "^SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER", 
                                                 "^POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN$|^POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN \\(.*\\)$"), 
                             label_pattern   = c("^Estimate!!Total:$"),
                             returns         = c("race, age", 
                                                 "poverty status, race, age", 
                                                 "veteran status, race, age", 
                                                 "family type, presence of children, race")) %>%
  as_tibble()




# Vars----
census.year <- 2021

acs5.2019.vars <- tidycensus::load_variables(year = 2019, 
                                             dataset = "acs5") %>%
  .[,c("name", "label", "concept")] %>%
  .[!duplicated(.),] %>%
  .[complete.cases(.),]


acs5.2019.vars %>%
  .[grepl(unique(search.terms.df$label_pattern), x = .$label, ignore.case = F) & 
      grepl(search.terms.df$concept_pattern[1], x = .$concept, ignore.case = F),]

acs5.2019.vars[grepl("^B01001", acs5.2019.vars$name),] %>%
  group_by(concept) %>%
  summarise(n = n())

# acs5.2019.vars %>%
#   .[.$label %in% c("Estimate!!Total" ,
#                    "Estimate!!Total:"),] %>%
#   #.[grepl("Estimate!!Total|Estimate!!Total:", .$label,ignore.case = F),] %>%
#   .[grepl(pattern = "FAMILY|FAMILIES", x = .$concept, ignore.case = F) & 
#       grepl(pattern = "CHILDREN", x = .$concept, ignore.case = F) & 
#       grepl(pattern = "PRESENCE", x = .$concept, ignore.case = F),] %>%
#   .$concept %>% unique() %>%
#   .[order(.)]
# 
# acs5.2019.vars$concept %>% 
#   grep(pattern = "TOTAL POPULATION", x = ., 
#        ignore.case = F, 
#        value = T) %>% unique()
# 
# grep(pattern = "Male", 
#      x = acs5.2019.vars$label, 
#      ignore.case = F, 
#      value = T) %>% unique()


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
acs5_2020.vars <- tidycensus::load_variables(2020, "acs5")


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
                                    survey = "acs5") %>%
  full_join(., 
            acs5_2019.vars[,c("name", "label", "concept")], 
            by = c("variable" = "name"))  

data.2019_NC$var_prefix <- strsplit(data.2019_NC$variable, "_") %>%
  lapply(., first) %>%
  unlist()

data.2019_NC$var_suffix <- strsplit(data.2019_NC$variable, "_") %>%
  lapply(., last) %>%
  unlist()

data.2020_NC <- tidycensus::get_acs(geography = "state", 
                                    variables  = variables_list_2020, 
                                    year   = 2020, 
                                    state  = "NC", 
                                    survey = "acs5")
grep("^poverty status in the past 12 months by age$|^poverty status in the past 12 months by age.*\\)$", 
     data.2019_NC$concept, 
     ignore.case = T, value = T) 




# Tidy data----
# all people, all races
data.2019_NC[which(data.2019_NC$concept %in% 
                     grep("^total population$", 
                          data.2019_NC$concept, 
                          ignore.case = T, value = T)),]$estimate %>% scales::comma()
# all people in poverty, all races
data.2019_NC[which((data.2019_NC$concept %in% 
                      unique(grep("^poverty status in the past 12 months by age$|^poverty status in the past 12 months by age.*\\)$", 
                                  data.2019_NC$concept, 
                                  ignore.case = T, value = T))) ),]$estimate %>% scales::comma()

data.2019_NC[is.na(data.2019_NC$GEOID),]
