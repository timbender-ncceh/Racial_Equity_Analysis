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

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/R")

# functions----
get_NA_cell <- function(df){
  out <- F
  while(out == F){
    
    temp.row <- sample(1:nrow(df), size = 1)
    temp.col <- sample(1:ncol(df), size = 1)
    
    if(is.na(unname(unlist(df[temp.row,temp.col])))){
      out <- T
      outR <- c(row = temp.row, col = temp.col)
    }
    
    rm(temp.row, temp.col)
    
  }
  return(outR)
}

# SetWD----
prime.wd    <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis"
hudtools.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/hud_tools"
data.wd     <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/data"
r_code.wd   <- "C:/Users/TimBender/Documents/R/ncceh/projects/racial_equity_analysis/R"


setwd(prime.wd)

# vars----

gh.census.data <- "https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/raw_county_data.csv"

# Master Table Design----
master.table <- expand.grid(population = c("All_People", "Youth", "Veteran"), 
                            year = c(2020,2019),
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

# to do- double-check all tables related ot age to make sure they all cut-off at 24 and under not 34 and under-----

master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- "B01001_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "B01001D_001 + B01001E_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- "B01001B_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- "B01001C_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- "B01001A_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- "B01001F_001 + B01001G_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- "B01001I_001"
master.table$ALL.PPL_total[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- "B01001_001 - B01001I_001"

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

master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- "B17020_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- "B17020A_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "B17020D_002 + B17020E_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- "B17020B_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- "B17020C_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- "B17020F_002 + B17020G_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- "B17020I_002"
master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- "B17020_002 - B17020I_002"

master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- "B17010_004 + B17010_011 + B17010_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- "B17010A_004 + B17010A_011 + B17010A_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "B17010D_004 + B17010D_011 + B17010D_017 + B17010E_004 + B17010E_011 + B17010E_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- "B17010B_004 + B17010B_011 + B17010B_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- "B17010C_004 + B17010C_011 + B17010C_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- "B17010F_004 + B17010F_011 + B17010F_017 + B17010G_004 + B17010G_011 + B17010G_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- "B17010I_004 + B17010I_011 + B17010I_017"
master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- "(B17010_004 + B17010_011 + B17010_017) - (B17010I_004 + B17010I_011 + B17010I_017)"

master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "TOTAL"]                   <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "White"]                   <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Asian/Pacific Islander"]  <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Black"]                   <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Native American/Alaskan"] <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Other/Multi-Racial"]      <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Hispanic"]                <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Not Hispanic"]            <- NA

master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "TOTAL"]                   <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "White"]                   <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "Asian/Pacific Islander"]  <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "Black"]                   <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "Native American/Alaskan"] <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "Other/Multi-Racial"]      <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "Hispanic"]                <- NA
master.table$IN.POV_total[master.table$population == "Veteran" & master.table$subpopulation == "Not Hispanic"]            <- NA

master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "TOTAL"]                   <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "White"]                   <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Asian/Pacific Islander"]  <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Black"]                   <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Native American/Alaskan"] <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Other/Multi-Racial"]      <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Hispanic"]                <- NA
master.table$IN.POV_in.fwc[master.table$population == "Veteran" & master.table$subpopulation == "Not Hispanic"]            <- NA

master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "TOTAL"]                   <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "White"]                   <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Asian/Pacific Islander"]  <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Black"]                   <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Native American/Alaskan"] <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Other/Multi-Racial"]      <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Hispanic"]                <- NA
master.table$ALL.PPL_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Not Hispanic"]            <- NA

master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "TOTAL"]                   <- "B17001_004 + B17001_005 + B17001_006 + B17001_007 + B17001_008 + B17001_009 + B17001_010 + B17001_018 + B17001_019 + B17001_020 + B17001_021 + B17001_022 + B17001_023 + B17001_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "White"]                   <- "B17001A_004 + B17001A_005 + B17001A_006 + B17001A_007 + B17001A_008 + B17001A_009 + B17001A_010 + B17001A_018 + B17001A_019 + B17001A_020 + B17001A_021 + B17001A_022 + B17001A_023 + B17001A_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "Asian/Pacific Islander"]  <- "B17001D_004 + B17001D_005 + B17001D_006 + B17001D_007 + B17001D_008 + B17001D_009 + B17001D_010 + B17001D_018 + B17001D_019 + B17001D_020 + B17001D_021 + B17001D_022 + B17001D_023 + B17001D_024 + B17001E_004 + B17001E_005 + B17001E_006 + B17001E_007 + B17001E_008 + B17001E_009 + B17001E_010 + B17001E_018 + B17001E_019 + B17001E_020 + B17001E_021 + B17001E_022 + B17001E_023 + B17001E_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "Black"]                   <- "B17001B_004 + B17001B_005 + B17001B_006 + B17001B_007 + B17001B_008 + B17001B_009 + B17001B_010 + B17001B_018 + B17001B_019 + B17001B_020 + B17001B_021 + B17001B_022 + B17001B_023 + B17001B_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "Native American/Alaskan"] <- "B17001C_004 + B17001C_005 + B17001C_006 + B17001C_007 + B17001C_008 + B17001C_009 + B17001C_010 + B17001C_018 + B17001C_019 + B17001C_020 + B17001C_021 + B17001C_022 + B17001C_023 + B17001C_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "Other/Multi-Racial"]      <- "B17001F_004 + B17001F_005 + B17001F_006 + B17001F_007 + B17001F_008 + B17001F_009 + B17001F_010 + B17001F_018 + B17001F_019 + B17001F_020 + B17001F_021 + B17001F_022 + B17001F_023 + B17001F_024 + B17001G_004 + B17001G_005 + B17001G_006 + B17001G_007 + B17001G_008 + B17001G_009 + B17001G_010 + B17001G_018 + B17001G_019 + B17001G_020 + B17001G_021 + B17001G_022 + B17001G_023 + B17001G_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "Hispanic"]                <- "B17001I_004 + B17001I_005 + B17001I_006 + B17001I_007 + B17001I_008 + B17001I_009 + B17001I_010 + B17001I_018 + B17001I_019 + B17001I_020 + B17001I_021 + B17001I_022 + B17001I_023 + B17001I_024"
master.table$IN.POV_total[master.table$population == "Youth" & master.table$subpopulation == "Not Hispanic"]            <- "(B17001_004 + B17001_005 + B17001_006 + B17001_007 + B17001_008 + B17001_009 + B17001_010 + B17001_018 + B17001_019 + B17001_020 + B17001_021 + B17001_022 + B17001_023 + B17001_024) - (B17001I_004 + B17001I_005 + B17001I_006 + B17001I_007 + B17001I_008 + B17001I_009 + B17001I_010 + B17001I_018 + B17001I_019 + B17001I_020 + B17001I_021 + B17001I_022 + B17001I_023 + B17001I_024)"

master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "TOTAL"]                   <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "White"]                   <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Asian/Pacific Islander"]  <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Black"]                   <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Native American/Alaskan"] <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Other/Multi-Racial"]      <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Hispanic"]                <- NA
master.table$IN.POV_in.fwc[master.table$population == "Youth" & master.table$subpopulation == "Not Hispanic"]            <- NA

master.table <- master.table %>% 
  as_tibble() %>%
  left_join(., 
            data.frame(GEOID = rep(c("37001", "37003", "37005", "37007", "37009", 
                                     "37011", "37013", "37015", "37017", "37019", 
                                     "37021", "37023", "37025", "37027", "37029", 
                                     "37031", "37033", "37035", "37037", "37039", 
                                     "37041", "37043", "37045", "37047", "37049", 
                                     "37051", "37053", "37055", "37057", "37059", 
                                     "37061", "37063", "37065", "37067", "37069", 
                                     "37071", "37073", "37075", "37077", "37079", 
                                     "37081", "37083", "37085", "37087", "37089", 
                                     "37091", "37093", "37095", "37097", "37099", 
                                     "37101", "37103", "37105", "37107", "37109", 
                                     "37111", "37113", "37115", "37117", "37119", 
                                     "37121", "37123", "37125", "37127", "37129", 
                                     "37131", "37133", "37135", "37137", "37139", 
                                     "37141", "37143", "37145", "37147", "37149", 
                                     "37151", "37153", "37155", "37157", "37159", 
                                     "37161", "37163", "37165", "37167", "37169", 
                                     "37171", "37173", "37175", "37177", "37179", 
                                     "37181", "37183", "37185", "37187", "37189", 
                                     "37191", "37193", "37195", "37197", "37199"), each =2), 
                       year = c(2019, 2020)), 
            by = "year")


write_csv(master.table, file = "census_table_source_info.csv")
# get the data----


raw.data <- read_csv(gh.census.data)

master.data <- master.table %>%
  mutate(., 
         ALL.PPL_total  = NA_real_, 
         ALL.PPL_in.fwc = NA_real_, 
         IN.POV_total   = NA_real_, 
         IN.POV_in.fwc  = NA_real_, 
         EXP.HL_total   = NA_real_, 
         EXP.HL_in.fwc  = NA_real_)

master.data <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_data.csv")


for(ic in c("ALL.PPL_total", 
            "ALL.PPL_in.fwc", "IN.POV_total", "IN.POV_in.fwc")){
  for(ir in 1:nrow(master.table)){
    #print(sample(0:9, size = 1))
    temp.tblval <- unname(unlist(master.table[ir,ic]))
    if(!is.na(temp.tblval)){
      # do the logic
      
      # if there's just 1 table to pull
      if(!grepl("\\+", temp.tblval) & !grepl("\\-", temp.tblval)){
        master.data[ir,ic] <- raw.data[raw.data$variable == temp.tblval & 
                                         raw.data$GEOID == master.table$GEOID[ir] & 
                                         raw.data$yr == master.table$year[ir],]$estimate
      }
      
      ## if there are multiple tables and they need to be added
      if(grepl("\\+", temp.tblval) & !grepl("\\-", temp.tblval)){
        tempA <- unlist(strsplit(split = " \\+ ", x = temp.tblval))
        
        master.data[ir,ic] <- sum(raw.data[raw.data$variable %in% tempA & 
                                             raw.data$GEOID == master.table$GEOID[ir] & 
                                             raw.data$yr == master.table$year[ir],]$estimate)
        
        
        rm(tempA)
      }
      ## if there are multiple tables and they need to be operated in some way
      ## other than addition
      if(!grepl("\\+", temp.tblval) & grepl("\\-", temp.tblval)){
        
      }
      
    }else{
      # skip the logic
    }
    
  }
}


# settle non-hispanic
for(i in 1:nrow(master.data)){
  
  
  if(master.data$subpopulation[i] == "Not Hispanic"){
    temp.geoid <- master.data$GEOID[i]
    temp.year  <- master.data$year[i]
    temp.pop   <- master.data$population[i]
    
    # ALL.PPL_total
    master.data$ALL.PPL_total[i] <- 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "TOTAL",]$ALL.PPL_total - 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "Hispanic",]$ALL.PPL_total
    
    # ALL.PPL_in.fwc
    master.data$ALL.PPL_in.fwc[i] <- 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "TOTAL",]$ALL.PPL_in.fwc - 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "Hispanic",]$ALL.PPL_in.fwc
    
    # IN.POV_total
    master.data$IN.POV_total[i] <- 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "TOTAL",]$IN.POV_total - 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "Hispanic",]$IN.POV_total
    
    # IN.POV_in.fwc
    master.data$IN.POV_in.fwc[i] <- 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "TOTAL",]$IN.POV_in.fwc - 
      master.data[master.data$year == temp.year &
                    master.data$GEOID == temp.geoid & 
                    master.data$population == temp.pop & 
                    master.data$subpopulation == "Hispanic",]$IN.POV_in.fwc
  }
  
  
  
}

# join counties-----
geo.info <- raw.data[,c("GEOID", "NAME")] %>%
  mutate(., 
         state = unlist(lapply(strsplit(NAME, ", "),last)), 
         county = unlist(lapply(strsplit(NAME, " County, "),first))) %>%
  .[!colnames(.) %in% "NAME"] %>%
  .[!duplicated(.),]

master.data <- left_join(master.data, 
                         geo.info) 

subpop_type <- data.frame(subpopulation = c("Asian/Pacific Islander",
                                            "Black", 
                                            "Native American/Alaskan", 
                                            "Other/Multi-Racial", 
                                            "White", 
                                            "Hispanic", 
                                            "Not Hispanic", 
                                            "TOTAL"), 
                          pop_type       = c(rep("race", 5), 
                                             rep("ethnicity", 2), 
                                             rep("TOTAL", 1)))

master.data <- master.data %>%
  left_join(subpop_type)


write_csv(x = master.data,
          file = "census_table_data.csv")


# load githubs----
ghdf <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_data.csv")


ghdf %>%
  group_by(population, subpop_bype = ifelse(subpopulation == "TOTAL", "total", "racial_subgroup")) %>%
  summarise(n = n(), 
            NA_apt = sum(is.na(ALL.PPL_total))/length(ALL.PPL_total), 
            NA_apif = sum(is.na(ALL.PPL_in.fwc))/length(ALL.PPL_in.fwc),
            NA_ipt = sum(is.na(IN.POV_total))/length(IN.POV_total),
            NA_ipif = sum(is.na(IN.POV_in.fwc))/length(IN.POV_in.fwc))


master.table %>%
  group_by(population, subpop_bype = ifelse(subpopulation == "TOTAL", "total", "racial_subgroup")) %>%
  summarise(n = n(), 
            NA_apt = sum(is.na(ALL.PPL_total))/length(ALL.PPL_total), 
            NA_apif = sum(is.na(ALL.PPL_in.fwc))/length(ALL.PPL_in.fwc),
            NA_ipt = sum(is.na(IN.POV_total))/length(IN.POV_total),
            NA_ipif = sum(is.na(IN.POV_in.fwc))/length(IN.POV_in.fwc))




# QA check-----

rm(list=ls());cat('\f');gc()

master.data  <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_data.csv")
master.table <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_source_info.csv")
#raw.data     <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/raw_county_data.csv")
# tidy----
master.data$population_f <- factor(master.data$population, 
                                   levels = c("All_People", "Youth", "Veteran"))
master.data$pop_type_f <- factor(master.data$pop_type, 
                                 levels = c("TOTAL", "race", "ethnicity"))
master.data$subpopulation_f <- factor(master.data$subpopulation, 
                                      levels = sort(unique(master.data$subpopulation))[c(7,1,2,4,8,6,3,5)])



gc()


# master defs----
vars.19 <- tidycensus::load_variables(2019, "acs5") %>%
  .[!colnames(.) %in% c("geography")]

def.apt_tbls <- master.table$ALL.PPL_total %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  .[!is.na(.)] 


def.aptfwc_tbls <- master.table$ALL.PPL_in.fwc %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  .[!is.na(.)] 

def.ipt_tbls <- master.table$IN.POV_total %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  .[!is.na(.)]

def.iptfwc_tbls <- master.table$IN.POV_in.fwc %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  .[!is.na(.)]


defs2 <- vars.19[vars.19$name %in% c(def.apt_tbls, def.aptfwc_tbls, 
                                     def.ipt_tbls, def.iptfwc_tbls),]


race.ptrns <- c(" ALONE\\)$", " RACES\\)$", " LATINO\\)$", 
                " ALONE HOUSEHOLDER\\)$", " RACES HOUSEHOLDER\\)$", " LATINO HOUSEHOLDER\\)$") %>%
  paste(., sep = "|", collapse = "|")

defs2 %>%
  .[!grepl(race.ptrns, defs2$concept),c("label", "concept")] %>%
  group_by(label, concept) %>%
  summarise() %>%
  group_by(label) %>%
  summarise(n = n()) %>%
  .[.$n > 1,]

defs2 %>%
  .[!grepl(race.ptrns, defs2$concept),] %>%
  .$label %>%
  strsplit(., split = ":") %>%
  lapply(., length) %>%
  unlist() %>%
  max()


defs3 <- mutate(defs2[!grepl(race.ptrns, defs2$concept),], 
                h1 = NA, 
                h2 = NA, 
                h3 = NA, 
                h4 = NA, 
                h5 = NA)

defs3$h1 <- defs2 %>%
  .[!grepl(race.ptrns, .$concept),] %>%
  .$label %>%
  strsplit(., split = ":") %>%
  lapply(., nth, 1) %>%
  unlist()

defs3$h2 <- defs2 %>%
  .[!grepl(race.ptrns, .$concept),] %>%
  .$label %>%
  strsplit(., split = ":") %>%
  lapply(., nth, 2) %>%
  unlist()

defs3$h3 <- defs2 %>%
  .[!grepl(race.ptrns, .$concept),] %>%
  .$label %>%
  strsplit(., split = ":") %>%
  lapply(., nth, 3) %>%
  unlist()

defs3$h4 <- defs2 %>%
  .[!grepl(race.ptrns, .$concept),] %>%
  .$label %>%
  strsplit(., split = ":") %>%
  lapply(., nth, 4) %>%
  unlist()

defs3$h5 <- defs2 %>%
  .[!grepl(race.ptrns, .$concept),] %>%
  .$label %>%
  strsplit(., split = ":") %>%
  lapply(., nth, 5) %>%
  unlist()

defs3$h1 <- gsub("Estimate!!Total", "TOTAL", defs3$h1)
defs3$h2 <- gsub("^!!", "", defs3$h2)
defs3$h3 <- gsub("^!!", "", defs3$h3)
defs3$h4 <- gsub("^!!", "", defs3$h4)
defs3$h5 <- gsub("^!!", "", defs3$h5)

# remove gender
table(defs3$h3)
table(defs3$h4)
table(defs3$h5)

grep("Male|Female", defs3$h4, ignore.case = T, value = T)

defs3$h2[defs3$h2 %in% c("Male", "Female")] <- NA
defs3$h4 <- gsub("^Male h|^Female h", "H", defs3$h4)


# def_tbl <- data.frame(table = c(def.apt_tbls, def.aptfwc_tbls, def.ipt_tbls, def.iptfwc_tbls))
# 
# def_tbl <- left_join(def_tbl, 
#                      vars.19, 
#                      by = c("table" = "name")) %>%
#   as_tibble()

# master table defs----
vars.19 <- tidycensus::load_variables(2019, "acs5") %>%
  .[!colnames(.) %in% c("geography")]

apt_tbls <- master.table$ALL.PPL_total %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  strsplit(., "_.*$") %>%
  unlist() %>%
  unique()

aptfwc_tbls <- master.table$ALL.PPL_in.fwc %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  strsplit(., "_.*$") %>%
  unlist() %>%
  unique()

ipt_tbls <- master.table$IN.POV_total %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  strsplit(., "_.*$") %>%
  unlist() %>%
  unique()

iptfwc_tbls <- master.table$IN.POV_in.fwc %>% 
  unique() %>%
  .[!grepl("\\-", .)] %>%
  strsplit(., 
           " \\+ ") %>%
  unlist() %>%
  unique() %>%
  strsplit(., "_.*$") %>%
  unlist() %>%
  unique()


tbl.out <- NULL

for(i in c(apt_tbls,
           aptfwc_tbls, 
           ipt_tbls, 
           iptfwc_tbls)[!is.na(c(apt_tbls,
                                 aptfwc_tbls, 
                                 ipt_tbls, 
                                 iptfwc_tbls))]){
  tbl.out <- rbind(tbl.out, 
                   get_acs(geography = "state", 
                           table     = i, 
                           year      = 2019, 
                           state     = "NC")[,c("GEOID", "NAME", "variable", "estimate")])
}

vars.19 <- tidycensus::load_variables(2019, "acs5") %>%
  .[!colnames(.) %in% c("geography")]

tbl.out <- left_join(tbl.out, 
                     vars.19, 
                     by = c("variable" = "name"))

get_acs("state", 
        table = "B01001", 
        year = 2019, 
        state = "NC")$variable %>% unique()



master.data %>%
  .[.$year == 2019,] %>%
  group_by(year, state, population, pop_type_f, subpopulation) %>%
  summarise(ALL.PPL_total = sum(ALL.PPL_total), 
            ALL.PPL_in.fwc = sum(ALL.PPL_in.fwc), 
            IN.POV_total   = sum(IN.POV_total), 
            IN.POV_in.fwc  = sum(IN.POV_in.fwc)) %>%
  View()


# problems ----
master.data %>%
  .[.$year == 2019 & 
      .$population == "All_People",c("ALL.PPL_in.fwc")]



# revise data / check data----

# in families with children total 2019 = 3,960,358
master.table[1,]

vars.19 <- tidycensus::load_variables(2019, "acs5") %>%
  .[!colnames(.) %in% c("geography")]

vars.19[grepl("^Estimate!!Total:$", vars.19$label),]

check.these.vars <- vars.19[grepl("^Estimate!!Total:$", vars.19$label) & 
          !grepl(" ALONE\\)$| RACES\\)$| LATINO\\)$", vars.19$concept),]$name %>% unique

vars.19[grepl("POVERTY", vars.19$concept) & 
  grepl("FAMILY TYPE", vars.19$concept),]$concept %>% unique()

# compare.vals <- NULL
# 
# library(tidycensus)
# 
# for(i in check.these.vars){
#   
#   compare.vals <- rbind(compare.vals, 
#                         get_acs(geography = "state", 
#                   variables = i, 
#                   #table     = "B17010", 
#                   year      = 2019, 
#                   state     = "NC", 
#                   county    = NULL, 
#                   survey    = "acs5")[,c("NAME", "variable", "estimate")])
# }
# 
# 
# compare.vals[compare.vals$estimate == 1467591 ,] %>%
#   left_join(., 
#             vars.19, 
#             by = c("variable" = "name"))
# 
# state.x <- get_acs(geography = "state", 
#                    #variables = "B17010_001", 
#                    table     = "B17010", 
#                    year      = 2019, 
#                    state     = "NC", 
#                    county    = NULL, 
#                    survey    = "acs5")
# 
# 
# 
# 
# 
# 
# 
# state.x
# 
# left_join(state.x, 
#           vars.19, 
#           by = c("variable" = "name"))$concept %>% unique()
# state.x
# vars.19
# vars.19[vars.19$name %in% state.x$variable,]
# 
# vars.19[grepl("family|families", vars.19$concept, ignore.case = T) & 
#           grepl("children", vars.19$label, ignore.case = T), ]$concept %>% unique
# 
# grep("^PRESENCE OF OWN CHILDREN UNDER 18 YEARS", 
#      vars.19$concept, ignore.case = T, value = T) %>% unique()
# 
# grep("PRESENCE OF OWN CHILDREN", 
#      vars.19$concept, ignore.case = T, value = T) %>% unique()
# 
# 
# vars.19 %>%
#   .[grepl(pattern = "\\(WHITE ALONE\\)$", x = .$concept),] %>%
#   .[grepl(pattern = "family", x = .$label, ignore.case = T),]
# 
# vars.19[grepl("FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS", vars.19$concept),]$label %>% unique()
# 
# vars.19[grepl("^HOUSEHOLD TYPE \\(INCLUDING LIVING ALONE\\)$|^HOUSEHOLD TYPE \\(INCLUDING LIVING ALONE\\) \\(.*\\)$$", vars.19$concept),]
# 
# get_acs("state", "B11001_001", year = 2019, state = "NC", survey = "acs5")
