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
        year + population + subpopulation ~ concept + var3)


master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"] <- "B01001_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"] <- "B01001D_001 + B01001E_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Black"] <- "B01001B_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- "B01001C_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "White"] <- "B01001A_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"] <- "B01001F_001 + B01001G_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"] <- "B01001I_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"] <- "B01001_001 - B01001I_001"


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


acs5.2020.vars
"^SEX BY AGE$|^SEX BY AGE \\(.*\\)$"

acs5.2020.vars[grepl("^SEX BY AGE$|^SEX BY AGE \\(.*\\)$",acs5.2020.vars$concept),] %>%
  .[!grepl("years", .$label),] %>%
  .[!grepl("Male|Female", .$label),]





# Vars----
census.year <- 2021

acs5.2019.vars <- tidycensus::load_variables(year = 2019, 
                                             dataset = "acs5") %>%
  .[,c("name", "label", "concept")] %>%
  .[!duplicated(.),] %>%
  .[complete.cases(.),]

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
