# Bos REA
# copy pasta 2022 charts

# https://www.ncceh.org/files/12525/

library(dplyr)
library(tidycensus)
library(lubridate)
library(ggplot2)
library(readr)
library(openxlsx)
library(data.table)

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
gen_cen.re.name <- function(concept1){
  if(!grepl(paste(cw.raceeth_raceeth$census_re, sep = "|", collapse = "|"), 
            x = concept1)){
    # no race / eth input
    out <- NA
  }else{
    # race / eth input
    
    out <- cw.raceeth_raceeth$census_re[unname(mapply(FUN = grepl, 
                                                      pattern = cw.raceeth_raceeth$census_re, 
                                                      x = rep(concept1, length(cw.raceeth_raceeth$census_re))))]
    
  }
  return(out)
}

make_regnum <- function(x){
  if(nchar(x)==1){
    x <- paste("0",x,sep="")
  }else{
    x <- as.character(x)
  }
  
  x <- paste("R",x,sep="")
  return(x)
}
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
(cw.raceeth_raceeth <- data.frame(census_re = c("WHITE ALONE", 
                                               "BLACK OR AFRICAN AMERICAN ALONE",
                                               "AMERICAN INDIAN AND ALASKA NATIVE ALONE", 
                                               "ASIAN ALONE", 
                                               "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", 
                                               "SOME OTHER RACE ALONE", 
                                               "TWO OR MORE RACES", 
                                               "WHITE ALONE, NOT HISPANIC OR LATINO", 
                                               "^HISPANIC OR LATINO"), 
                                 hud_re    = factor(c("White", "Black", "Native American/Alaskan", 
                                                      "Asian/Pacific Islander", "Asian/Pacific Islander", 
                                                      "Other/Multi-Racial", "Other/Multi-Racial", 
                                                      "Hispanic", "Non-Hispanic"), 
                                                    levels = c("White", "Black", "Native American/Alaskan", 
                                                               "Asian/Pacific Islander", 
                                                               "Other/Multi-Racial", 
                                                               "Hispanic", "Non-Hispanic"))))



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
         geo_name = gen_coname(NAME), 
         geo = ifelse(geo_name == "North Carolina", "state", "county"),
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("geo_name" = "county")) %>%
  left_join(., cw.co_reg, 
            by = c("geo_name" = "County"))


B01001_001.co <- get_acs(geography = "county", 
                         variables = "B01001_001", 
                         table     = NULL, 
                         cache_table = var.ct, 
                         year        = var.year, 
                         state       = "NC", 
                         survey      = "acs5") %>%
  mutate(., 
         geo_name = gen_coname(NAME), 
         geo = ifelse(geo_name == "North Carolina", "state", "county"),
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("geo_name" = "county")) %>%
  left_join(., cw.co_reg, 
            by = c("geo_name" = "County"))




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
         geo_name = gen_coname(NAME), 
         geo = ifelse(geo_name == "North Carolina", "state", "county"),
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("geo_name" = "county")) %>%
  left_join(., cw.co_reg, 
            by = c("geo_name" = "County")) 
B01001R_001.nc$census_re <- unlist(lapply(X = B01001R_001.nc$concept, 
                            FUN = gen_cen.re.name))
B01001R_001.nc <- left_join(B01001R_001.nc, 
                            cw.raceeth_raceeth)


chart1_nc <- B01001R_001.nc %>%
  group_by(year, 
           geo = "NC", 
           geo_name,
           #concept, 
           label, 
           hud_re) %>%
  summarise(est_population = sum(estimate)) %>%
  ungroup() %>%
  group_by(geo_name) %>%
  mutate(., 
         pct_total = est_population/sum(est_population))

chart1_nc$geo_f <- factor(chart1_nc$geo, 
                           levels = c("NC", 
                                      "BoS", 
                                      "All Other CoCs"))

chart1_nc$est_population %>% as.character() %>% writeClipboard()


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
         geo_name = gen_coname(NAME), 
         geo = ifelse(geo_name == "North Carolina", "state", "county"),
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("geo_name" = "county")) %>%
  left_join(., cw.co_reg, 
            by = c("geo_name" = "County"))
B01001R_001.co$census_re <- unlist(lapply(X = B01001R_001.co$concept, 
                                   FUN = gen_cen.re.name))
B01001R_001.co <- left_join(B01001R_001.co, 
          cw.raceeth_raceeth)

chart1_coc <- B01001R_001.co %>%
  group_by(year, 
           geo = "All Other CoCs", 
           geo_name = coc_long, 
           label, hud_re) %>%
  summarise(est_population = sum(estimate)) %>%
  ungroup() %>%
  group_by(geo_name) %>%
  mutate(., 
         pct_total = est_population / sum(est_population))

chart1_coc[chart1_coc$geo_name == "NC-503",]$geo <- "BoS"

chart1_coc$geo_f <- factor(chart1_coc$geo, 
                           levels = c("NC", 
                                      "BoS", 
                                      "All Other CoCs"))

ggplot() + 
  geom_col(data = chart1_nc, 
             aes(x = pct_total, 
                 y = geo_name, 
                 fill = hud_re), 
           color = "white") +
  geom_col(data = chart1_coc, 
           aes(x = pct_total, 
               y = geo_name, 
               fill = hud_re), 
           color = "white")+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        strip.text.y = element_text(angle = 0)) +
  scale_fill_discrete(name = "Race")+
  scale_x_continuous(labels = scales::percent, 
                     name = "Percent of Total Population", 
                     breaks = seq(0,1,by=0.1))+
  scale_y_discrete(name = "Geography Name") +
  labs(title = "Baseline Race - 2022") + 
  facet_grid(geo_f~., scales = "free_y", space = "free_y", switch = "y") 



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
         geo_name = gen_coname(NAME), 
         geo = ifelse(geo_name == "North Carolina", "state", "county"),
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("geo_name" = "county")) %>%
  left_join(., cw.co_reg, 
            by = c("geo_name" = "County"))
B01001E_001.nc$re <- unlist(lapply(X = B01001E_001.nc$concept, 
                                   FUN = gen_cen.re.name)) %>% .[!is.na(.)]

B01001E_001.co <- get_acs(geography = "county", 
                          variables = c("B01001H_001", 
                                        "B01001I_001"), 
                          table     = NULL, 
                          cache_table = var.ct, 
                          year        = var.year, 
                          state       = "NC", 
                          survey      = "acs5") %>%
  mutate(., 
         geo_name = gen_coname(NAME), 
         geo = ifelse(geo_name == "North Carolina", "state", "county"),
         year = var.year) %>%
  left_join(., 
            vars.21[,c(1,2,3)], 
            by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("geo_name" = "county")) %>%
  left_join(., cw.co_reg, 
            by = c("geo_name" = "County"))
B01001E_001.co$re <- unlist(lapply(X = B01001E_001.co$concept, 
                                   FUN = gen_cen.re.name))%>% .[!is.na(.)]


ggplot() + 
  geom_col(data = chart1_nc, 
           aes(x = pct_total, 
               y = geo_name, 
               fill = hud_re), 
           color = "white") +
  geom_col(data = chart1_coc, 
           aes(x = pct_total, 
               y = geo_name, 
               fill = hud_re), 
           color = "white")+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        strip.text.y = element_text(angle = 0)) +
  scale_fill_discrete(name = "Race")+
  scale_x_continuous(labels = scales::percent, 
                     name = "Percent of Total Population", 
                     breaks = seq(0,1,by=0.1))+
  scale_y_discrete(name = "Geography Name") +
  labs(title = "Baseline Race - 2022") + 
  facet_grid(geo_f~., scales = "free_y", space = "free_y", switch = "y") 

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
