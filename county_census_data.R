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

# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- ""
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- ""
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- " + "
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- ""
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- ""
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- " + "
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- ""
# master.table$IN.POV_total[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- " - "
# 
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "TOTAL"]                   <- ""
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "White"]                   <- ""
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Asian/Pacific Islander"]  <- " + "
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Black"]                   <- ""
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Native American/Alaskan"] <- ""
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Other/Multi-Racial"]      <- " + "
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Hispanic"]                <- ""
# master.table$IN.POV_in.fwc[master.table$population == "All_People" & master.table$subpopulation == "Not Hispanic"]            <- " - "

master.table %>% as_tibble()

write_csv(master.table, file = "census_table_source_info.csv")


# Data Pullsdown----

master.data <- master.table %>%
  mutate(., 
         ALL.PPL_total  = NA_real_, 
         ALL.PPL_in.fwc = NA_real_, 
         IN.POV_total   = NA_real_, 
         IN.POV_in.fwc  = NA_real_, 
         EXP.HL_total   = NA_real_, 
         EXP.HL_in.fwc  = NA_real_)

for(ic in 4:ncol(master.data)){
  for(ir in 1:nrow(master.data)){
    temp.tbls <- master.table[ir,ic] %>% 
      unlist %>% 
      unname
    
    if(!is.na(temp.tbls)){
      temp.tbls <- strsplit(temp.tbls, split = " ") %>%
        unlist()
    }else{
      temp.tbls <- NA
      # why is this happening? ----
    }
      
    # logic for  handling multiple table queries
    if(length(temp.tbls) == 1){
      # single table query
      if(is.na(temp.tbls)){
        temp.val <- NA
        # this is a problem----
      }else{
        temp.val <- tidycensus::get_acs(geography = "state", 
                                        variables = temp.tbls, 
                                        table = NULL, 
                                        year = master.data$year[ir], 
                                        state = "NC", 
                                        county = NULL, 
                                        geometry = F, 
                                        survey = "acs5")$estimate
      }
      
    }else{
      # multiple table query
      # TO DO ----
      
      # if all operators are "+" then just get all tables and add them together
      if(all(temp.tbls[nchar(temp.tbls) == 1] == "+")){
        all.tbls <- temp.tbls[nchar(temp.tbls) != 1]
        sum.tbls <- NULL
        for(i.tbl in all.tbls){
          sum.tbls <- c(sum.tbls, 
                        tidycensus::get_acs(geography = "state", 
                              variables = i.tbl, 
                              table = NULL, 
                              year = master.data$year[ir], 
                              state = "NC", 
                              county = NULL, 
                              geometry = F, 
                              survey = "acs5")$estimate)
        }
        temp.val <- sum(sum.tbls)
        rm(sum.tbls,i.tbl)
        
      }else{
        # TO DO----
        # some addition and some subtraction in operators
        temp.val <- NA
      }
      
      
      
      #temp.val <- NA
    }
    # write to master.data
    master.data[ir,ic] <- temp.val
    rm(temp.val, temp.tbls)
  }
}

View(master.data)



library(ggplot2)

temp.plot <- master.data[,1:4]

for(i.pop in c("All_People", "Veteran", "Youth")){
  for(i.year in c(2019,2020)){
    temp.plot[temp.plot$subpopulation == "Not Hispanic" & 
                temp.plot$population  == i.pop & 
                temp.plot$year        == i.year,]$ALL.PPL_total <- temp.plot[temp.plot$subpopulation == "TOTAL" & 
                                                                               temp.plot$population  == i.pop & 
                                                                               temp.plot$year        == i.year,]$ALL.PPL_total -
      temp.plot[temp.plot$subpopulation == "Hispanic" & 
                  temp.plot$population  == i.pop & 
                  temp.plot$year        == i.year,]$ALL.PPL_total
  }
}

temp.plot$by_type <- NA
temp.plot$by_type[temp.plot$subpopulation == "TOTAL"] <- "TOTAL"
temp.plot$by_type[temp.plot$subpopulation %in% c("Hispanic", 
                                                 "Not Hispanic")] <- "Ethnicity"
temp.plot$by_type[is.na(temp.plot$by_type)] <- "Race"

library(data.table)

temp.plot %>%
  as.data.table() %>%
  dcast(., 
      by_type + subpopulation + population ~ year, 
      value.var = "ALL.PPL_total") %>%
  as.data.frame() %>% 
  mutate(., 
         delta = `2020` - `2019`, 
         #pct_delta2 = delta/`2019`,
         pct_delta = scales::percent(delta / `2019`,accuracy = 0.1)) %>%
  ggplot(data = .,
         aes(x = by_type, y = pct_delta2)) + 
  geom_point(size = 4, 
             aes(shape = population,color = subpopulation)) +
  geom_line(aes(group = subpopulation))+
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(-1,1,by=0.05))

ggplot(data = temp.plot) + 
  geom_col(aes(x = factor(year), y = ALL.PPL_total, fill = subpopulation, 
               group = by_type)) +
  facet_wrap(by_type~population, scales = "free_x")+
  scale_y_continuous(labels = scales::comma)


# Data Lookup Tools below----

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


"VETERAN STATUS FOR THE CIVILIAN POPULATION 18"
"^Estimate!!Total:$"

library(data.table)
# how to get each component----
which.veteran.concept  <- which(grepl("VETERAN STATUS", x = acs5.2020.vars$concept))
which.families.concept <- which(grepl("^POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN", x = acs5.2020.vars$concept))
which.poverty.concept  <- which(grepl("^POVERTY STATUS IN THE PAST 12 MONTHS BY S{0,1}E{0,1}X{0,1} {0,1}B{0,1}Y{0,1} {0,1}AGE$|^POVERTY STATUS IN THE PAST 12 MONTHS BY S{0,1}E{0,1}X{0,1} {0,1}B{0,1}Y{0,1} {0,1}AGE [^B]", x = acs5.2020.vars$concept))

acs5.2020.vars[1:nrow(acs5.2020.vars) %in% which.poverty.concept,]$concept %>% unique()

acs5.2020.vars[1:nrow(acs5.2020.vars) %in% which.poverty.concept,c("concept", "label")] %>%
  .[!duplicated(.),] %>%
  as.data.table() %>%
  dcast(., 
        label~concept, fun.aggregate = length, fill = 0) %>%
  as.data.table() %>%
  as_tibble() %>%
  .$label


master.table[master.table$population == "Veteran",]
colnames(master.table)

# search terms vars----
regex.youth       <- c("!!Under 5 years", 
                       "!!5 to 9 years",
                       "!!10 to 14 years", 
                       "!!15 to 17 years", 
                       "!!18 and 19 years", 
                       "!!20 years", 
                       "!!21 years", 
                       "!!22 to 24 years", 
                       "!!20 to 24 years") %>% paste(., sep = "|", collapse = "|")
regex.race        <- c("(WHITE ALONE)", 
                       "(BLACK OR AFRICAN AMERICAN ALONE)", 
                       "(AMERICAN INDIAN AND ALASKA NATIVE ALONE)", 
                       "(ASIAN ALONE)", 
                       "(NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)", 
                       "(SOME OTHER RACE ALONE)",
                       "(TWO OR MORE RACES)", 
                       "(HISPANIC OR LATINO)")  %>% paste(., sep = "|", collapse = "|")
regex.vet         <- c("VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER")  %>% paste(., sep = "|", collapse = "|")
regex.fam.w.child <- c("FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS")  %>% paste(., sep = "|", collapse = "|")
regex.poverty     <- c("POVERTY STATUS IN THE PAST 12 MONTHS") %>% paste(., sep = "|", collapse = "|")
regex.allppl      <- c("Estimate!!Total:") %>% paste(., sep = "|", collapse = "|")



get_tables2 <- function(regex.concept.v, 
                        regex.label.v,
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
  
  # concept filter
  df.concept <- df$concept
  out.concept <- list()
  for(i in 1:length(regex.concept.v)){
    out.concept[[i]] <- grepl(pattern = regex.concept.v[i], x = df.concept)
  }
  
  # label filter
  df.label <- df$label
  out.label <- list()
  for(i in 1:length(regex.label.v)){
    out.label[[i]] <- grepl(pattern = regex.label.v[i], x = df.label)
  }
  
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
