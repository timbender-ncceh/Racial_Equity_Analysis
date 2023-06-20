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
make_regnum <- function(x){
  if(is.na(x)){
    x <- NA
  }else{
    if(nchar(x)==1){
      x <- paste("0",x,sep="")
    }else{
      x <- as.character(x)
    }
    
    x <- paste("R",x,sep="")
  }
  
  return(x)
}
gen_cen.re.name <- function(concept1){
  # if(!grepl(paste(cw.raceeth_raceeth$census_re, sep = "|", collapse = "|"), 
  #           x = concept1)){
  #   # no race / eth input
  #   out <- NA
  # }else{
  #   # race / eth input
  #   
  #   out <- cw.raceeth_raceeth$census_re[unname(mapply(FUN = grepl, 
  #                                                     pattern = cw.raceeth_raceeth$census_re, 
  #                                                     x = rep(concept1, length(cw.raceeth_raceeth$census_re))))]
  #   
  # }
  # return(out)
  
  out <- gsub("^.*\\(|\\).*$", "", concept1) %>%
    gsub(" HOUSEHOLDER$", "", .)
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

gh.mast_cen <- "https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_data2021.csv"
gh.mast_cen1920 <- "https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_data.csv"

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
                                               "HISPANIC OR LATINO"), 
                                 hud_re    = factor(c("White", "Black", "Native American/Alaskan", 
                                                      "Asian/Pacific Islander", "Asian/Pacific Islander", 
                                                      "Other/Multi-Racial", "Other/Multi-Racial", 
                                                      "Hispanic", "Non-Hispanic"), 
                                                    levels = c("White", "Black", "Native American/Alaskan", 
                                                               "Asian/Pacific Islander", 
                                                               "Other/Multi-Racial", 
                                                               "Hispanic", "Non-Hispanic"))))


master <- read_csv(gh.mast_cen)
master1920 <- read_csv(gh.mast_cen1920)

# Tidy----
ghd$county <- gsub(" County, .*$", "", ghd$NAME)
ghd <- left_join(ghd, 
                 vars.21[,c("name", "label", "concept")], 
                 by = c("variable" = "name"))


# REPRODUCE CHARTS----

# race / ethnicity----

# # total pop
# vars.21[grepl(pattern = "^SEX BY AGE$", x = vars.21$concept) & 
#           grepl(pattern = "^Estimate!!Total:$", x = vars.21$label),]
# 
# 
# B01001_001.nc <- get_acs(geography = "state", 
#                          variables = "B01001_001", 
#                          table     = NULL, 
#                          cache_table = var.ct, 
#                          year        = var.year, 
#                          state       = "NC", 
#                          survey      = "acs5") %>%
#   mutate(., 
#          geo_name = gen_coname(NAME), 
#          geo = ifelse(geo_name == "North Carolina", "state", "county"),
#          year = var.year) %>%
#   left_join(., 
#             vars.21[,c(1,2,3)], 
#             by = c("variable" = "name")) %>%
#   left_join(., 
#             cw.coc_co, 
#             by = c("geo_name" = "county")) %>%
#   left_join(., cw.co_reg, 
#             by = c("geo_name" = "County"))
# 
# 
# B01001_001.co <- get_acs(geography = "county", 
#                          variables = "B01001_001", 
#                          table     = NULL, 
#                          cache_table = var.ct, 
#                          year        = var.year, 
#                          state       = "NC", 
#                          survey      = "acs5") %>%
#   mutate(., 
#          geo_name = gen_coname(NAME), 
#          geo = ifelse(geo_name == "North Carolina", "state", "county"),
#          year = var.year) %>%
#   left_join(., 
#             vars.21[,c(1,2,3)], 
#             by = c("variable" = "name")) %>%
#   left_join(., 
#             cw.coc_co, 
#             by = c("geo_name" = "county")) %>%
#   left_join(., cw.co_reg, 
#             by = c("geo_name" = "County"))




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


chart1_reg <- rbind(summarise(group_by(mutate(B01001R_001.co, 
                                              group_cat = ifelse(coc_short == 503, Region, "All Other CoCs")),
                                       year, group_cat, 
                                       hud_re), 
                              est_pop = sum(estimate)),
                    summarise(group_by(mutate(B01001R_001.nc, 
                                              group_cat = "NC"),
                                       year, group_cat, 
                                       hud_re), 
                              est_pop = sum(estimate))) %>%
  as.data.table() %>%
  dcast(., 
        year + group_cat ~ hud_re) 

chart1_reg$region <- unlist(lapply(ifelse(grepl("^Region \\d{1,2}$", 
             x = chart1_reg$group_cat), 
       yes = as.numeric(gsub("^Region ", "", chart1_reg$group_cat)), 
       no = as.numeric(chart1_reg$group_cat)), 
       make_regnum))

chart1_reg$group_cat[!is.na(chart1_reg$region)] <- chart1_reg$region[!is.na(chart1_reg$region)]

chart1_reg <- chart1_reg[order(chart1_reg$group_cat),]
chart1_reg <- chart1_reg[rev(c(2,1,3:15))]

chart1_reg$group_cat %>% as.character() %>% writeClipboard()
chart1_reg$White %>% as.character() %>% writeClipboard()
chart1_reg$Black %>% as.character() %>% writeClipboard()
chart1_reg$`Native American/Alaskan` %>% as.character() %>% writeClipboard()
chart1_reg$`Asian/Pacific Islander` %>% as.character() %>% writeClipboard()
chart1_reg$`Other/Multi-Racial` %>% as.character() %>% writeClipboard()

chart1_reg


chart1 <- rbind(summarise(group_by(mutate(B01001R_001.co, 
                                          group_cat = ifelse(coc_short == 503, "BoS", "All Other CoCs")),
                                   year, group_cat, 
                                   hud_re), 
                          est_pop = sum(estimate)),
                summarise(group_by(mutate(B01001R_001.nc, 
                                          group_cat = "NC"),
                                   year, group_cat, 
                                   hud_re), 
                          est_pop = sum(estimate))) %>%
  as.data.table() %>%
  dcast(., 
        year + group_cat ~ hud_re) 

chart1$year %>% as.character %>% writeClipboard()
chart1$group_cat %>% as.character %>% writeClipboard()
chart1$White %>% as.character %>% writeClipboard()
chart1$Black %>% as.character %>% writeClipboard()
chart1$`Native American/Alaskan` %>% as.character %>% writeClipboard()
chart1$`Asian/Pacific Islander` %>% as.character %>% writeClipboard()
chart1$`Other/Multi-Racial` %>% as.character %>% writeClipboard()



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


B01001E_001.nc$census_re <- gsub("^.*\\(|\\).*$", "", B01001E_001.nc$concept)
B01001E_001.nc <- left_join(B01001E_001.nc, 
                            cw.raceeth_raceeth)

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
B01001E_001.co$census_re <- gsub("^.*\\(|\\).*$", "", B01001E_001.co$concept)
B01001E_001.co <- left_join(B01001E_001.co, 
                            cw.raceeth_raceeth)


chart2 <- rbind(summarise(group_by(mutate(B01001E_001.co, 
                                          group_cat = ifelse(coc_short == 503, "BoS", "All Other CoCs")),
                                   year, group_cat, 
                                   hud_re), 
                          est_pop = sum(estimate)),
                summarise(group_by(mutate(B01001E_001.nc, 
                                          group_cat = "NC"),
                                   year, group_cat, 
                                   hud_re), 
                          est_pop = sum(estimate))) %>%
  as.data.table() %>%
  dcast(., 
        year + group_cat ~ hud_re) 

chart2$year %>% as.character %>% writeClipboard()
chart2$group_cat %>% as.character %>% writeClipboard()
chart2$Hispanic %>% as.character %>% writeClipboard()
chart2$`Non-Hispanic` %>% as.character %>% writeClipboard()


chart2reg <- rbind(summarise(group_by(mutate(B01001E_001.co, 
                                             group_cat = ifelse(coc_short == 503, Region, "All Other CoCs")),
                                      year, group_cat, 
                                      hud_re), 
                             est_pop = sum(estimate)),
                   summarise(group_by(mutate(B01001E_001.nc, 
                                             group_cat = "NC"),
                                      year, group_cat, 
                                      hud_re), 
                             est_pop = sum(estimate))) %>%
  as.data.table() %>%
  dcast(., 
        year + group_cat ~ hud_re) 

chart2reg$region <- unlist(lapply(ifelse(grepl("^Region \\d{1,2}$", 
                                                x = chart2reg$group_cat), 
                                          yes = as.numeric(gsub("^Region ", "", chart2reg$group_cat)), 
                                          no = as.numeric(chart2reg$group_cat)), 
                                   make_regnum))

chart2reg$group_cat[!is.na(chart2reg$region)] <- chart2reg$region[!is.na(chart2reg$region)]

chart2reg <- chart2reg[order(chart2reg$group_cat),]
chart2reg <- chart2reg[rev(c(2,1,3:15))]

chart2reg$year %>% as.character %>% writeClipboard()
chart2reg$group_cat %>% as.character %>% writeClipboard()
chart2reg$Hispanic %>% as.character %>% writeClipboard()
chart2reg$`Non-Hispanic` %>% as.character %>% writeClipboard()


# poverty----
pov21.bos <- master %>%
  # mutate(., 
  #        subpopulation = toupper(subpopulation)) %>%
  # mutate(., 
  #        geo_name = gen_coname(NAME), 
  #        geo = ifelse(geo_name == "North Carolina", "state", "county"),
  #        year = var.year) %>%
  # left_join(., 
  #           vars.21[,c(1,2,3)], 
  #           by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("county")) %>%
  left_join(., cw.co_reg, 
            by = c("county" = "County"))

pov21.bos$group_geo <- ifelse(pov21.bos$coc_short == 503, 
                              "BoS", "Other CoCs")

pov21.bos[,c(4:7)]

chart3 <- pov21.bos %>%
  .[.$year == 2021 & 
      .$population == "All_People" & 
      .$group_geo == "BoS",] %>%
  group_by(year, population, 
           hud_re = subpopulation,
           group_geo) %>%
  summarise(sum_popTot = sum(ALL.PPL_total), 
            sum_popFam = sum(ALL.PPL_in.fwc), 
            sum_pop.povTot = sum(IN.POV_total), 
            sum_pop.povFam = sum(IN.POV_in.fwc)) %>%
  as.data.table() %>%
  melt(., 
       id.vars = c("year", "population", "hud_re", "group_geo"))  %>%
  as.data.frame() %>% as_tibble()

chart3$hud_re[chart3$hud_re == "Asian/Pacific Islander"] <- "API"
chart3$hud_re[chart3$hud_re == "Native American/Alaskan"] <- "NAA"
chart3$hud_re[chart3$hud_re == "Other/Multi-Racial"] <- "OMR"
chart3$hud_re[chart3$hud_re == "Hispanic"] <- "Hisp"
chart3$hud_re[chart3$hud_re == "Not Hispanic"] <- "NHisp"

chart4 <- chart3 %>%
  as.data.table() %>%
  dcast(., 
        year + population + group_geo + variable ~ hud_re) %>%
  .[,c("year", "variable", "Hisp", "NHisp", "TOTAL")]  %>%
  as.data.frame() 

chart3 <- chart3 %>% 
  as.data.table() %>%
  dcast(., 
        year + population + group_geo + variable ~ hud_re) %>%
  .[,c("year", "variable", "White", "Black", "NAA", "API", "OMR", "TOTAL")]  %>%
  as.data.frame() 


chart3$year %>% as.character() %>% writeClipboard()
chart3$variable %>% as.character() %>% writeClipboard()
chart3$White %>% as.character() %>% writeClipboard()
chart3$Black %>% as.character() %>% writeClipboard()
chart3$NAA %>% as.character() %>% writeClipboard()
chart3$API %>% as.character() %>% writeClipboard()
chart3$OMR %>% as.character() %>% writeClipboard()
chart3$TOTAL  %>% as.character() %>% writeClipboard()

chart4$year %>% as.character() %>% writeClipboard()
chart4$variable %>% as.character() %>% writeClipboard()
chart4$Hisp %>% as.character() %>% writeClipboard()
chart4$NHisp %>% as.character() %>% writeClipboard()
chart4$TOTAL %>% as.character() %>% writeClipboard()

# youth----
youthRE.bos <- master %>%
  # mutate(., 
  #        subpopulation = toupper(subpopulation)) %>%
  # mutate(., 
  #        geo_name = gen_coname(NAME), 
  #        geo = ifelse(geo_name == "North Carolina", "state", "county"),
  #        year = var.year) %>%
  # left_join(., 
  #           vars.21[,c(1,2,3)], 
  #           by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("county")) %>%
  left_join(., cw.co_reg, 
            by = c("county" = "County"))

youthRE.bos$group_geo <- ifelse(youthRE.bos$coc_short == 503, 
                              "BoS", "Other CoCs")

chart5 <- youthRE.bos %>%
  .[.$year == 2021 & 
      .$population == "Youth" & 
      .$group_geo == "BoS",] %>%
  group_by(year, population, 
           hud_re = subpopulation,
           group_geo) %>%
  summarise(sum_popTot = sum(ALL.PPL_total)#, 
            #sum_popFam = sum(ALL.PPL_in.fwc), 
            #sum_pop.povTot = sum(IN.POV_total), 
            #sum_pop.povFam = sum(IN.POV_in.fwc)
            ) %>%
  as.data.table() %>%
  melt(., 
       id.vars = c("year", "population", "hud_re", "group_geo"))  %>%
  as.data.frame() %>% as_tibble()

chart5$hud_re[chart5$hud_re == "Asian/Pacific Islander"] <- "API"
chart5$hud_re[chart5$hud_re == "Native American/Alaskan"] <- "NAA"
chart5$hud_re[chart5$hud_re == "Other/Multi-Racial"] <- "OMR"
chart5$hud_re[chart5$hud_re == "Hispanic"] <- "Hisp"
chart5$hud_re[chart5$hud_re == "Not Hispanic"] <- "NHisp"

chart6 <- chart5 %>%
  as.data.table() %>%
  dcast(., 
        year + population + group_geo + variable ~ hud_re) %>%
  .[,c("year", "variable", "Hisp", "NHisp", "TOTAL")]  %>%
  as.data.frame() 

chart5 <- chart5 %>% 
  as.data.table() %>%
  dcast(., 
        year + population + group_geo + variable ~ hud_re) %>%
  .[,c("year", "variable", "White", "Black", "NAA", "API", "OMR", "TOTAL")]  %>%
  as.data.frame() 

chart5$White %>% as.character() %>% writeClipboard()
chart5$Black %>% as.character() %>% writeClipboard()
chart5$NAA %>% as.character() %>% writeClipboard()
chart5$API %>% as.character() %>% writeClipboard()
chart5$OMR %>% as.character() %>% writeClipboard()

# veterans----

vetRE.bos <- master %>%
  # mutate(., 
  #        subpopulation = toupper(subpopulation)) %>%
  # mutate(., 
  #        geo_name = gen_coname(NAME), 
  #        geo = ifelse(geo_name == "North Carolina", "state", "county"),
  #        year = var.year) %>%
  # left_join(., 
  #           vars.21[,c(1,2,3)], 
  #           by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("county")) %>%
  left_join(., cw.co_reg, 
            by = c("county" = "County"))

vetRE.bos$group_geo <- ifelse(vetRE.bos$coc_short == 503, 
                                "BoS", "Other CoCs")

chart7 <- vetRE.bos %>%
  .[.$year == 2021 & 
      .$population == "Veteran" & 
      .$group_geo == "BoS",] %>%
  group_by(year, population, 
           hud_re = subpopulation,
           group_geo) %>%
  summarise(sum_popTot = sum(ALL.PPL_total)#, 
            #sum_popFam = sum(ALL.PPL_in.fwc), 
            #sum_pop.povTot = sum(IN.POV_total), 
            #sum_pop.povFam = sum(IN.POV_in.fwc)
  ) %>%
  as.data.table() %>%
  melt(., 
       id.vars = c("year", "population", "hud_re", "group_geo"))  %>%
  as.data.frame() %>% as_tibble()

chart7$hud_re[chart7$hud_re == "Asian/Pacific Islander"] <- "API"
chart7$hud_re[chart7$hud_re == "Native American/Alaskan"] <- "NAA"
chart7$hud_re[chart7$hud_re == "Other/Multi-Racial"] <- "OMR"
chart7$hud_re[chart7$hud_re == "Hispanic"] <- "Hisp"
chart7$hud_re[chart7$hud_re == "Not Hispanic"] <- "NHisp"

chart8 <- chart7 %>%
  as.data.table() %>%
  dcast(., 
        year + population + group_geo + variable ~ hud_re) %>%
  .[,c("year", "variable", "Hisp", "NHisp", "TOTAL")]  %>%
  as.data.frame() 

chart7 <- chart7 %>% 
  as.data.table() %>%
  dcast(., 
        year + population + group_geo + variable ~ hud_re) %>%
  .[,c("year", "variable", "White", "Black", "NAA", "API", "OMR", "TOTAL")]  %>%
  as.data.frame() 

chart7$White %>% as.character() %>% writeClipboard()
chart7$Black %>% as.character() %>% writeClipboard()
chart7$NAA %>% as.character() %>% writeClipboard()
chart7$API %>% as.character() %>% writeClipboard()
chart7$OMR %>% as.character() %>% writeClipboard()

chart8



# Race over time----




chart9 <- rbind(master,master1920) %>%
  # mutate(., 
  #        subpopulation = toupper(subpopulation)) %>%
  # mutate(., 
  #        geo_name = gen_coname(NAME), 
  #        geo = ifelse(geo_name == "North Carolina", "state", "county"),
  #        year = var.year) %>%
  # left_join(., 
  #           vars.21[,c(1,2,3)], 
  #           by = c("variable" = "name")) %>%
  left_join(., 
            cw.coc_co, 
            by = c("county")) %>%
  left_join(., cw.co_reg, 
            by = c("county" = "County"))

chart9$group_geo <- ifelse(chart9$coc_short == 503, 
                              "BoS", "Other CoCs")




chart9 <- chart9 %>%
  .[#.$year == 2021 & 
      .$population == "All_People" & 
      .$group_geo == "BoS",] %>%
      #  ,] %>%
  group_by(year, population, 
           hud_re = subpopulation,
           group_geo) %>%
  #) %>%
  summarise(sum_popTot = sum(ALL.PPL_total)#, 
            #sum_popFam = sum(ALL.PPL_in.fwc), 
            #sum_pop.povTot = sum(IN.POV_total), 
            #sum_pop.povFam = sum(IN.POV_in.fwc)
  ) %>%
  as.data.table() %>%
  melt(., 
       id.vars = c("year", "population", "hud_re" , "group_geo"))  %>%
  as.data.frame() %>% as_tibble()

chart9$hud_re[chart9$hud_re == "Asian/Pacific Islander"] <- "API"
chart9$hud_re[chart9$hud_re == "Native American/Alaskan"] <- "NAA"
chart9$hud_re[chart9$hud_re == "Other/Multi-Racial"] <- "OMR"
chart9$hud_re[chart9$hud_re == "Hispanic"] <- "Hisp"
chart9$hud_re[chart9$hud_re == "Not Hispanic"] <- "NHisp"

chart10 <- chart9 %>%
  as.data.table() %>%
  dcast(., 
        year + population + 
          group_geo + 
          variable ~ hud_re) %>%
  .[,c("year", 
       "group_geo", 
       "variable", "Hisp", "NHisp", "TOTAL")]  %>%
  as.data.frame() 

chart9 <- chart9 %>% 
  as.data.table() %>%
  dcast(., 
        year + population + 
          group_geo + 
          variable ~ hud_re) %>%
  .[,c("year", 
       "group_geo", 
       "variable", "White", "Black", "NAA", "API", "OMR", "TOTAL")]  %>%
  as.data.frame() 


chart9$variable %>% as.character() %>% writeClipboard()
chart9$year %>% as.character() %>% writeClipboard()
chart9$White %>% as.character() %>% writeClipboard()
chart9$Black %>% as.character() %>% writeClipboard()
chart9$NAA %>% as.character() %>% writeClipboard()
chart9$API %>% as.character() %>% writeClipboard()
chart9$OMR %>% as.character() %>% writeClipboard()

chart10$year %>% as.character() %>% writeClipboard()
chart10$Hisp %>% as.character() %>% writeClipboard()
chart10$NHisp %>% as.character() %>% writeClipboard()
# Ethnicity over Time----

# Race and Ethnicity of Heads of Households and Adults----

# Average Length of Stay by Race, FY22----

# Average Length of Stay by Race and Ethnicity, FY22----

# more stellaP data VVV (see report)



# # Summary
# 
# sumpull <- expand.grid(geo_types = c("County"), 
#                        years     = c(2019:2023), 
#                        source    = c(NA), 
#                        count     = c("total_HH", "total_PPL", "PPL_in_pov", "FWC_in_pov", "Sheltered HL", "Unsheltered HL"), 
#                        by        = c("race", "ethnicity")) %>%
#   mutate(., 
#          dummyval = 1)
# 
# sumpull %>%
#   # .[!(.$year %in% c(2022:2023) &
#   #         .$source %in% c("Census")),] %>%
#   as.data.table() %>%
#   dcast(., 
#         geo_types + source + by + years ~ count, 
#         fun.aggregate = length) %>%
#   as.data.frame() %>%
#   as_tibble()
# 
# 
# expand.grid(Var1 = c("everyone", "homeless"), 
#             Var2 = c("HH/Fam", "PPL"), 
#             Var4 = c("Race", "Ethnicity")) %>%
#   left_join(., 
#             data.frame(Var1 = c("homeless", "homeless", "everyone"), 
#                        Var3 = c("sheltered", "unsheltered", "ALL"))) %>%
#   left_join(., 
#             data.frame(Var1 = c("everyone", "homeless", "homeless"), 
#                        source = c("Census", "PIT", "stellaP"))) %>%
#   left_join(., 
#             data.frame(source = c(rep("Census", 3), 
#                                   rep("PIT", 5), 
#                                   rep("stellaP", 5)), 
#                        year = c(2019:2021, 
#                                 2019:2023, 2019:2023)))


# Summary Try again----

# 1) Population

# 2) Poverty in fam/ppl

# 3) Homelessness in fam/ppl

# 4) Length of Stay

# 5) Exits to destinations

# 6) Returns to HL from destinations
