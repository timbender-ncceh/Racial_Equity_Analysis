#stellap

library(dplyr)
library(readxlsb)
library(data.table)
library(ggplot2)
library(readr)
library(lubridate)
library(glue)

setwd("C:/Users/TimBender/North Carolina Coalition to End Homelessness/PM Data Center - Documents/Reporting/Reporting  PIT HIC/Statewide Reporting- PIT+HIC/Working Files Mastersheet/Racial Equity Reference data")

# https://ncceh.sharepoint.com/sites/boscoccoordination/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Fboscoccoordination%2FShared%20Documents%2F4%2D%20BoS%20Developing%20CoC%20System%2FRacial%20Equity%20Subcommittee%2FRacial%20Equity%20Assessment%2F2022%20NOFA%20RE%20Assessment&p=true&ga=1


rm(list=ls());cat('\f')
gc()
# Functions----
sp_man_race22 <- function(x, fieldname = NULL, year = NA){
  long_names <- c("All HH", 
                  "American Indian, Alaska Native or Indigenous", 
                  "Asian or Asian American", 
                  "Black or African American or African", 
                  "Multiple Races", 
                  "Native Hawaiian or Pacific Islander", 
                  "White", 
                  "Hispanic / Latino", 
                  "Non-Hispanic / Non-Latino", 
                  "American Indian, Alaska Native or Indigenous; HISP",
                  "American Indian, Alaska Native or Indigenous; non-HISP", 
                  "Black; HISP", 
                  "Black; non-HISP", 
                  "Multi-Racial or Asian or Native Hawaiian or Pacific Islander - HISP",
                  "Multi-Racial or Asian or Native Hawaiian or Pacific Islander - non-HISP",
                  "White; HISP", 
                  "White; non-HISP")
  out <- data.frame(names = long_names, 
                    yr = year,
                    vals = x)
  colnames(out)[colnames(out) == "vals"] <- fieldname
  return(as_tibble(out))
}

sp_man_race <- function(x, fieldname = NULL, year = NA){
  long_names <- c("All HH", 
                  "American Indian, Alaska Native or Indigenous", 
                  "Asian or Asian American", 
                  "Black or African American or African", 
                  "Multiple Races", 
                  "Native Hawaiian or Pacific Islander", 
                  "White", 
                  "Hispanic / Latino", 
                  "Non-Hispanic / Non-Latino", 
                  "American Indian, Alaska Native or Indigenous; HISP",
                  "American Indian, Alaska Native or Indigenous; non-HISP", 
                  "Black; HISP", 
                  "Black; non-HISP", 
                  "White; HISP", 
                  "White; non-HISP")
  out <- data.frame(names = long_names, 
                    yr = year,
                    vals = x)
  colnames(out)[colnames(out) == "vals"] <- fieldname
  return(as_tibble(out))
}

# data entry----

# PIT----



# Stella_P----

# 2018----
data.18 <- full_join(sp_man_race(x = c(1414,10,5,626,35,3,735,53,1358,2,8,12,614,32,703), 
                                 fieldname = c("perm_exits")),
                     sp_man_race(x = c(1369,5,5,504,53,1,800,45,1321,0,5,4,500,34,766), 
                                 fieldname = c("temp_exits"))) %>%
  full_join(., sp_man_race(x = c(1510,16,3,674,33,5,774,43,1460,2,14,13,661,23,751), 
                           fieldname = c("unknown_exits"))) %>%
  full_join(., sp_man_race(x = c(76,0,0,33,4,0,39,2,74,0,0,1,32,1,38), 
                           fieldname = c("returns_from_perm_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(100,1,0,36,3,0,60,1,99,0,1,0,36,1,59), 
                           fieldname = c("returns_from_perm_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(368,1,0,161,12,1,193,11,356,0,1,2,159,7,186), 
                           fieldname = c("returns_from_##_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(490,2,0,196,16,1,274,13,476,0,2,2,194,9,265), 
                           fieldname = c("returns_from_##_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(73,59,67,77,55,47,71,60,73,124,48,55,77,61,72), 
                           fieldname = c("avg_length_stay_days"))) %>%
  full_join(., sp_man_race(x = c(6517,
                                 47,17,2902,171,13, 
                                 3367, # white
                                 NA,NA,NA,NA,NA,NA,
                                 128,3222), 
                           fieldname = c("overall_population")))

# data.18 <- rows_append(data.18, 
#                        data.frame(names = "Unknown", 
#                                   overall_population = -1))

data.18$yr <- "FY18"




as_tibble(data.18)

# 2019----
data.19 <- full_join(sp_man_race(x = c(1598,13,4,710,36,5,829,62,1535,2,11,9,701,41,788), 
                                 fieldname = c("perm_exits")),
                     sp_man_race(x = c(1581,17,6,682,53,6,815,53,1528,1,16,7,675,36,779), 
                                 fieldname = c("temp_exits"))) %>%
  full_join(., sp_man_race(x = c(1113,10,4,429,28,5,635,34,1078,1,9,8,421,20,615), 
                           fieldname = c("unknown_exits"))) %>%
  full_join(., sp_man_race(x = c(85,0,0,32,3,1,49,3,82,0,0,2,30,1,48), 
                           fieldname = c("returns_from_perm_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(156,0,0,64,6,1,85,7,149,0,0,3,61,3,82), 
                           fieldname = c("returns_from_perm_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(335,0,0,141,13,2,179,9,326,0,0,2,139,6,173), 
                           fieldname = c("returns_from_##_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(616,2,0,268,28,2,316,20,596,0,2,5,263,13,303), 
                           fieldname = c("returns_from_##_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(87,52,104,95,62,49,84,59,88,90,49,47,96,68,85), 
                           fieldname = c("avg_length_stay_days"))) %>%
  full_join(., sp_man_race(x = c(6572,
                                54,22,2936,164,21, 
                                 3375, # white
                                NA,NA,NA,NA,NA,NA,
                                 123,3239), 
                           fieldname = c("overall_population")))
# data.19 <- rows_append(data.19, 
#                        data.frame(names = "Unknown", 
#                                   overall_population = -1))

data.19$yr <- "FY19"


#2020----
data.20 <- full_join(sp_man_race(x = c(1417,10,8,577,23,4,793,26,1389,1,9,0,577,19,774), 
                                 fieldname = c("perm_exits")),
                     sp_man_race(x = c(1579,18,8,639,30,2,876,47,1527,2,16,8,631,33,843), 
                                 fieldname = c("temp_exits"))) %>%
  full_join(., sp_man_race(x = c(554,6,3,216,13,0,316,10,543,0,6,1,215,9,307), 
                           fieldname = c("unknown_exits"))) %>%
  full_join(., sp_man_race(x = c(57,1,0,16,0,0,40,3,54,1,0,0,16,2,38), 
                           fieldname = c("returns_from_perm_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(252,1,0,101,8,1,141,9,243,1,0,1,100,5,136), 
                           fieldname = c("returns_from_perm_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(281,4,1,108,7,1,160,9,272,1,3,1,107,6,154), 
                           fieldname = c("returns_from_##_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(544,3,1,214,17,2,307,16,528,1,2,4,210,9,298), 
                           fieldname = c("returns_from_##_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(101,76,105,108,79,47,97,73,102,59,77,50,109,76,97), 
                           fieldname = c("avg_length_stay_days"))) %>%
  full_join(., sp_man_race(x = c(5921,
                                 56,26,2645,117,9, 
                                 3068, #white
                                 NA,NA,NA,NA,NA,NA,
                                 95,2961), 
                           fieldname = c("overall_population")))

# data.20 <- rows_append(data.20, 
#                        data.frame(names = "Unknown", 
#                                   overall_population = -1))
data.20$yr <- "FY20"

# 2021----
data.21 <- full_join(sp_man_race(x = c(1146,17,6,514,30,3,572,42,1104,1,16,9,505,28,544), 
                                 fieldname = c("perm_exits")),
                     sp_man_race(x = c(1685,20,7,593,35,2,1018,75,1602,2,18,9,584,53,965), 
                                 fieldname = c("temp_exits"))) %>%
  full_join(., sp_man_race(x = c(599,3,4,274,16,2,299,18,581,2,1,5,269,6,293), 
                           fieldname = c("unknown_exits"))) %>%
  full_join(., sp_man_race(x = c(33,0,0,14,0,0,19,1,32,0,0,0,14,1,18), 
                           fieldname = c("returns_from_perm_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(92,1,2,29,2,0,58,4,88,1,0,0,29,3,55), 
                           fieldname = c("returns_from_perm_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(210,2,1,73,8,0,126,7,203,0,2,1,72,5,121), 
                           fieldname = c("returns_from_##_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(387,4,4,144,10,1,224,11,376,1,3,2,142,6,218), 
                           fieldname = c("returns_from_##_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(112,100,103,128,94,80,99,81,113,67,104,51,129,91,99), 
                           fieldname = c("avg_length_stay_days"))) %>%
  full_join(., sp_man_race(x = c(6715,
                                 83,25,3055,178,15, 
                                 3359,# white
                                 NA,NA,NA,NA,NA,NA,
                                 141,3186), 
                           fieldname = c("overall_population")))

# data.21 <- rows_append(data.21, 
#                        data.frame(names = "Unknown", 
#                                   overall_population = -1))

data.21$yr <- "FY21"

# 2022----
data.22 <- full_join(sp_man_race22(x = c(1726,35,6,794,54,5,827,79,1644,6,26,15,779,10,60,48,779), 
                                 fieldname = c("perm_exits")),
                     sp_man_race22(x = c(2663,45,17,1112,76,9,1393,105,2548,15,30,17,1095,16,97,57,1336), 
                                 fieldname = c("temp_exits"))) %>%
  full_join(., sp_man_race22(x = c(515,3,3,227,19,2,261,12,501,0,3,3,224,1,23,8,253), 
                           fieldname = c("unknown_exits"))) %>%
  full_join(., sp_man_race22(x = c(54,1,1,21,1,0,30,3,51,0,1,0,21,0,1,2,28), 
                           fieldname = c("returns_from_perm_within_6_months"))) %>%
  full_join(., sp_man_race22(x = c(79,1,0,36,2,0,40,5,74,0,1,0,36,0,1,4,36), 
                           fieldname = c("returns_from_perm_within_12_months"))) %>%
  full_join(., sp_man_race22(x = c(347,1,1,141,10,0,192,13,332,0,1,2,139,0,10,8,184),#187,0,1,83,5,0,98,5,182,0,0,0,83,4,94), 
                           fieldname = c("returns_from_##_within_6_months"))) %>%
  full_join(., sp_man_race22(x = c(442,6,4,167,16,0,248,22,418,1,5,2,165,0,18,16,232),#350,4,1,124,12,0,209,18,331,1,3,2,122,13,196), 
                           fieldname = c("returns_from_##_within_12_months"))) %>%
  full_join(., sp_man_race22(x = c(116,105,62,135,100,96,101,99,116,57,120,78,136,93,102,114,100),#132,113,63,155,106,121,113,111,133,65,124,92,156,125,113), 
                           fieldname = c("avg_length_stay_days"))) %>%
  full_join(., sp_man_race22(x = c(6251,102,28,2763,183,22,3133,244,5986,24,78,44,2719,34,219,142,2991), 
                           fieldname = c("overall_population")))

# data.22 <- rows_append(data.22, 
#                        data.frame(names = "Unknown", 
#                                   overall_population = -1))

data.22$yr <- "FY22"


# 2023----
data.23 <- full_join(sp_man_race(x = c(NA), 
                                 fieldname = c("perm_exits")),
                     sp_man_race(x = c(NA), 
                                 fieldname = c("temp_exits"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("unknown_exits"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("returns_from_perm_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("returns_from_perm_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("returns_from_##_within_6_months"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("returns_from_##_within_12_months"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("avg_length_stay_days"))) %>%
  full_join(., sp_man_race(x = c(NA), 
                           fieldname = c("overall_population")))

# data.23 <- rows_append(data.23, 
#                        data.frame(names = "Unknown", 
#                                   overall_population = -1))

data.23$yr <- "FY23_half"



# consolidated----

all.cons <- rbind(melt(as.data.table(data.20),
                       id.vars = c("names", "yr")), 
                  melt(as.data.table(data.19),
                       id.vars = c("names", "yr")),
                  melt(as.data.table(data.18),
                       id.vars = c("names", "yr")),
                  melt(as.data.table(data.21),
                       id.vars = c("names", "yr")),
                  melt(as.data.table(data.22),
                       id.vars = c("names", "yr"))) %>%
  dcast(., variable+names~yr, 
        value.var = "value", fun.aggregate = sum, 
        fill = 0) %>%
  as_tibble()

all.cons

all.cons$sd <- NA
all.cons$avg <- NA
all.cons$var <- NA
all.cons$diff_range <- NA
# all.cons$min <- NA
# all.cons$max <- NA

for(i in 1:nrow(all.cons)){
  all.cons$sd[i] <- sd(c(all.cons$FY18[i], 
                         all.cons$FY19[i],
                         all.cons$FY20[i],
                         all.cons$FY21[i], 
                         all.cons$FY22half[i]), 
                       na.rm = T)
  all.cons$avg[i] <- mean(c(all.cons$FY18[i], 
                            all.cons$FY19[i],
                            all.cons$FY20[i],
                            all.cons$FY21[i], 
                            all.cons$FY22half[i]), 
                          na.rm = T)
  
  all.cons$var[i] <- var(c(all.cons$FY18[i], 
                           all.cons$FY19[i],
                           all.cons$FY20[i],
                           all.cons$FY21[i], 
                           all.cons$FY22half[i]), 
                         na.rm = T)
  # all.cons$min[i] <- min(c(all.cons$FY18[i], 
  #                          all.cons$FY19[i],
  #                          all.cons$FY20[i],
  #                          all.cons$FY21[i], 
  #                          all.cons$FY22half[i]), 
  #                        na.rm = T)
  # all.cons$max[i] <- max(c(all.cons$FY18[i], 
  #                          all.cons$FY19[i],
  #                          all.cons$FY20[i],
  #                          all.cons$FY21[i], 
  #                          all.cons$FY22half[i]), 
  #                        na.rm = T)
  all.cons$diff_range[i] <- diff(range(c(all.cons$FY18[i], 
                                         all.cons$FY19[i],
                                         all.cons$FY20[i],
                                         all.cons$FY21[i], 
                                         all.cons$FY22half[i]), 
                                       na.rm = T)) / 
    mean(c(all.cons$FY18[i], 
          all.cons$FY19[i],
          all.cons$FY20[i],
          all.cons$FY21[i], 
          all.cons$FY22half[i])[c(all.cons$FY18[i], 
                                  all.cons$FY19[i],
                                  all.cons$FY20[i],
                                  all.cons$FY21[i], 
                                  all.cons$FY22half[i])>0], 
        na.rm = T)
  
  
}


# rename variable

all.cons$variable2 <- as.character(all.cons$variable)
unique(all.cons$variable2)

all.cons$variable2[all.cons$variable2 == "perm_exits"] <- "Permanent_Housing_Exits"
all.cons$variable2[all.cons$variable2 == "temp_exits"] <- "Temporary_Housing_Exits"
all.cons$variable2[all.cons$variable2 == "unknown_exits"] <- "Unknown_Housing_Exits"
all.cons$variable2[all.cons$variable2 == "returns_from_perm_within_6_months"] <- "Returns_From_Permanent_Housing_within_6_Months"
all.cons$variable2[all.cons$variable2 == "returns_from_##_within_6_months"] <- "Returns_From_All_Exit_Types_within_6_Months"
all.cons$variable2[all.cons$variable2 == "returns_from_perm_within_12_months"] <- "Returns_From_Permanent_Housing_within_12_Months"
all.cons$variable2[all.cons$variable2 == "returns_from_##_within_12_months"] <- "Returns_From_All_Exit_Types_within_12_Months"
all.cons$variable2[all.cons$variable2 == "avg_length_stay_days"] <- "Average_Length_of_Stay_in_Days"
all.cons$variable2[all.cons$variable2 == "overall_population"] <- "Overall_Population"




# hispanic note
all.cons[order(all.cons$diff_range,decreasing = T),]
all.cons$hisp <- NA
all.cons$hisp[grepl("hisp|hispanic", all.cons$names, ignore.case = T)] <- T
all.cons$hisp[grepl("non-hisp|non-hispanic", all.cons$names, ignore.case = T)] <- F


# ggplot() + 
#   geom_col(data = all.cons[all.cons$variable=="temp_exits",], 
#            aes(x = diff_range, y = names)) + 
#   facet_grid(variable~., scales = "free", space = 'free')+
#   theme(strip.text.y = element_text(angle = 0))

all.cons$race      <- ifelse(is.na(all.cons$hisp),T,F)
all.cons$ethnicity <- ifelse(is.na(all.cons$hisp) & all.cons$names != "All HH",F,T)

# keep V ----
unique(all.cons$names)
all.cons$intersect <- NA

all.cons$intersect[all.cons$hisp == T & 
                     ! is.na(all.cons$hisp)] <- "Hispanic / Latino"
all.cons$intersect[all.cons$hisp == F & 
                     ! is.na(all.cons$hisp)] <- "Non-Hispanic / Non-Latino"
all.cons$intersect[is.na(all.cons$hisp)] <- "Unreporting"

all.cons$race_1 <- NA
all.cons$race_1[grep("hawaii|pacific", all.cons$names, ignore.case = T, value = F)]    <- "Native Hawaiian or Pacific Islander"
all.cons$race_1[grep("black", all.cons$names, ignore.case = T, value = F)]             <- "Black or African American or African"
all.cons$race_1[grep("indian|indigenous", all.cons$names, ignore.case = T, value = F)] <- "American Indian, Alaska Native or Indigenous"
all.cons$race_1[grep("multiple|unknown", all.cons$names, ignore.case = T, value = F)]  <- "Multiple Races"
all.cons$race_1[grep("white", all.cons$names, ignore.case = T, value = F)]             <- "White"
all.cons$race_1[grep("asian", all.cons$names, ignore.case = T, value = F)]             <- "Asian or Asian American"
all.cons$race_1[grep("all hh", all.cons$names, ignore.case = T, value = F)]            <- "All Households"
all.cons$race_1[grep("latino|latina", all.cons$names, ignore.case = T, value = F)]            <- NA
# keep ^ ----


# make new tables----

all.cons_race <- all.cons %>%
  .[.$race == T,] %>%
  .[!colnames(.) %in% c("sd", "avg", "var", "diff_range", "variable")]

all.cons_eth <- all.cons %>%
  .[.$ethnicity == T,] %>%
  .[!colnames(.) %in% c("sd", "avg", "var", "diff_range", "variable")]

all.cons_intersect <- all.cons %>%
  .[!is.na(.$intersect),] %>%
  .[!colnames(.) %in% c("sd", "avg", "var", "diff_range", "variable")]

# remove NAs from "overall"----

all.cons_race      <- all.cons_race[!is.na(all.cons_race$FY18),]
all.cons_eth       <- all.cons_eth[!is.na(all.cons_eth$FY18),]
all.cons_intersect <- all.cons_intersect[!is.na(all.cons_intersect$FY18),]


# # older vvv ----
# all.cons_race      <- all.cons[all.cons$names %in% c("All HH", "White", 
#                                                      "Native Hawaiian or Pacific Islander", 
#                                                      "Multiple Races", 
#                                                      "Black or African American or African", 
#                                                      "Asian or Asian American", 
#                                                      "American Indian, Alaska Native or Indigenous"),] %>%
#   .[!colnames(.) %in% c("sd", "avg", "var", "diff_range", "variable")]
# 
# 
# all.cons_eth       <- all.cons[all.cons$names %in% 
#                                  c("All HH", unique(grep("Hispanic", all.cons$names, value = T))),] %>%
#   .[!colnames(.) %in% c("sd", "avg", "var", "diff_range", "variable")]
# 
# all.cons_intersect <- all.cons[all.cons$names %in% 
#                                  c("All HH", 
#                                    unique(grep("HISP", all.cons$names, value = T)), 
#                                    "Native Hawaiian or Pacific Islander",
#                                    "Asian or Asian American", 
#                                    "Multiple Races"),] %>%
#   .[!colnames(.) %in% c("sd", "avg", "var", "diff_range", "variable")]


all.cons_eth$tbl_type <- "ethnicity"
all.cons_intersect$tbl_type <- "intersectional"
all.cons_race$tbl_type <- "race"

all.cons_eth <- all.cons_eth[,c("tbl_type", "variable2", "names", "hisp", "FY18", 
                "FY19", "FY20", "FY21", "FY22")] %>%
  .[order(.$variable2),]
all.cons_intersect <- all.cons_intersect[,c("tbl_type", "variable2", "names", "hisp", "FY18", 
                "FY19", "FY20", "FY21", "FY22")] %>%
  .[order(.$variable2),]
all.cons_race <- all.cons_race[,c("tbl_type", "variable2", "names", "hisp", "FY18", 
                "FY19", "FY20", "FY21", "FY22")] %>%
  .[order(.$variable2),]

#percent change
all.cons_eth$FY18_to_FY19 <- ifelse(all.cons_eth$FY18 == 0, NA, 
                                    (all.cons_eth$FY19 - all.cons_eth$FY18) / all.cons_eth$FY18) 
all.cons_eth$FY19_to_FY20 <- ifelse(all.cons_eth$FY19 == 0, NA, 
                                    (all.cons_eth$FY20 - all.cons_eth$FY19) / all.cons_eth$FY19)
all.cons_eth$FY20_to_FY21 <- ifelse(all.cons_eth$FY20 == 0, NA, 
                                    (all.cons_eth$FY21 - all.cons_eth$FY20) / all.cons_eth$FY20)
all.cons_eth$FY21_to_FY22 <- ifelse(all.cons_eth$FY21 == 0, NA, 
                                    (all.cons_eth$FY22 - all.cons_eth$FY21) / all.cons_eth$FY21)

all.cons_race$FY18_to_FY19 <- ifelse(all.cons_race$FY18 == 0, NA, 
                                     (all.cons_race$FY19 - all.cons_race$FY18) / all.cons_race$FY18) 
all.cons_race$FY19_to_FY20 <- ifelse(all.cons_race$FY19 == 0, NA, 
                                     (all.cons_race$FY20 - all.cons_race$FY19) / all.cons_race$FY19)
all.cons_race$FY20_to_FY21 <- ifelse(all.cons_race$FY20 == 0, NA, 
                                     (all.cons_race$FY21 - all.cons_race$FY20) / all.cons_race$FY20)
all.cons_race$FY21_to_FY22 <- ifelse(all.cons_race$FY21 == 0, NA, 
                                     (all.cons_race$FY22 - all.cons_race$FY21) / all.cons_race$FY21)

all.cons_intersect$FY18_to_FY19 <- ifelse(all.cons_intersect$FY18 == 0, NA, 
                                          (all.cons_intersect$FY19 - all.cons_intersect$FY18) / all.cons_intersect$FY18) 
all.cons_intersect$FY19_to_FY20 <- ifelse(all.cons_intersect$FY19 == 0, NA, 
                                          (all.cons_intersect$FY20 - all.cons_intersect$FY19) / all.cons_intersect$FY19)
all.cons_intersect$FY20_to_FY21 <- ifelse(all.cons_intersect$FY20 == 0, NA, 
                                          (all.cons_intersect$FY21 - all.cons_intersect$FY20) / all.cons_intersect$FY20)
all.cons_intersect$FY21_to_FY22 <- ifelse(all.cons_intersect$FY21 == 0, NA, 
                                          (all.cons_intersect$FY22 - all.cons_intersect$FY21) / all.cons_intersect$FY21)


#percent share

all.cons_eth$FY18_pct_share <- NA
all.cons_eth$FY19_pct_share <- NA
all.cons_eth$FY20_pct_share <- NA
all.cons_eth$FY21_pct_share <- NA
all.cons_eth$FY22_pct_share <- NA

for(i in 1:nrow(all.cons_eth)){
  all.cons_eth$FY18_pct_share[i] <- all.cons_eth$FY18[i] / all.cons_eth$FY18[all.cons_eth$names == "All HH" & 
                                                                               all.cons_eth$variable2 == all.cons_eth$variable2[i]]
  all.cons_eth$FY19_pct_share[i] <- all.cons_eth$FY19[i] / all.cons_eth$FY19[all.cons_eth$names == "All HH" & 
                                                                               all.cons_eth$variable2 == all.cons_eth$variable2[i]]
  all.cons_eth$FY20_pct_share[i] <- all.cons_eth$FY20[i] / all.cons_eth$FY20[all.cons_eth$names == "All HH" & 
                                                                               all.cons_eth$variable2 == all.cons_eth$variable2[i]]
  all.cons_eth$FY21_pct_share[i] <- all.cons_eth$FY21[i] / all.cons_eth$FY21[all.cons_eth$names == "All HH" & 
                                                                               all.cons_eth$variable2 == all.cons_eth$variable2[i]]
  all.cons_eth$FY22_pct_share[i] <- all.cons_eth$FY22[i] / all.cons_eth$FY22[all.cons_eth$names == "All HH" & 
                                                                               all.cons_eth$variable2 == all.cons_eth$variable2[i]]
  
}


all.cons_race$FY18_pct_share <- NA
all.cons_race$FY19_pct_share <- NA
all.cons_race$FY20_pct_share <- NA
all.cons_race$FY21_pct_share <- NA
all.cons_race$FY22_pct_share <- NA

for(i in 1:nrow(all.cons_race)){
  all.cons_race$FY18_pct_share[i] <- all.cons_race$FY18[i] / all.cons_race$FY18[all.cons_race$names == "All HH" & 
                                                                                  all.cons_race$variable2 == all.cons_race$variable2[i]]
  all.cons_race$FY19_pct_share[i] <- all.cons_race$FY19[i] / all.cons_race$FY19[all.cons_race$names == "All HH" & 
                                                                                  all.cons_race$variable2 == all.cons_race$variable2[i]]
  all.cons_race$FY20_pct_share[i] <- all.cons_race$FY20[i] / all.cons_race$FY20[all.cons_race$names == "All HH" & 
                                                                                  all.cons_race$variable2 == all.cons_race$variable2[i]]
  all.cons_race$FY21_pct_share[i] <- all.cons_race$FY21[i] / all.cons_race$FY21[all.cons_race$names == "All HH" & 
                                                                                  all.cons_race$variable2 == all.cons_race$variable2[i]]
  all.cons_race$FY22_pct_share[i] <- all.cons_race$FY22[i] / all.cons_race$FY22[all.cons_race$names == "All HH" & 
                                                                                            all.cons_race$variable2 == all.cons_race$variable2[i]]
  
}


all.cons_intersect$FY18_pct_share <- NA
all.cons_intersect$FY19_pct_share <- NA
all.cons_intersect$FY20_pct_share <- NA
all.cons_intersect$FY21_pct_share <- NA
all.cons_intersect$FY22_pct_share <- NA

for(i in 1:nrow(all.cons_intersect)){
  all.cons_intersect$FY18_pct_share[i] <- all.cons_intersect$FY18[i] / all.cons_intersect$FY18[all.cons_intersect$names == "All HH" & 
                                                                                                 all.cons_intersect$variable2 == all.cons_intersect$variable2[i]]
  all.cons_intersect$FY19_pct_share[i] <- all.cons_intersect$FY19[i] / all.cons_intersect$FY19[all.cons_intersect$names == "All HH" & 
                                                                                                 all.cons_intersect$variable2 == all.cons_intersect$variable2[i]]
  all.cons_intersect$FY20_pct_share[i] <- all.cons_intersect$FY20[i] / all.cons_intersect$FY20[all.cons_intersect$names == "All HH" & 
                                                                                                 all.cons_intersect$variable2 == all.cons_intersect$variable2[i]]
  all.cons_intersect$FY21_pct_share[i] <- all.cons_intersect$FY21[i] / all.cons_intersect$FY21[all.cons_intersect$names == "All HH" & 
                                                                                                 all.cons_intersect$variable2 == all.cons_intersect$variable2[i]]
  all.cons_intersect$FY22_pct_share[i] <- all.cons_intersect$FY22[i] / all.cons_intersect$FY22[all.cons_intersect$names == "All HH" & 
                                                                                                           all.cons_intersect$variable2 == all.cons_intersect$variable2[i]]
  
}

# order var2

var2order <- c("Average_Length_of_Stay_in_Days", 
               "Overall_Population",
               "Permanent_Housing_Exits",
               "Temporary_Housing_Exits",
               "Unknown_Housing_Exits",
               "Returns_From_Permanent_Housing_within_6_Months",
               "Returns_From_All_Exit_Types_within_6_Months",
               "Returns_From_Permanent_Housing_within_12_Months",
               "Returns_From_All_Exit_Types_within_12_Months")

if(any(!all.cons$variable2 %in% var2order)){
  stop("missing variables; see above ^^^")
}else{
  print("proceed")
}

all.cons_eth$variable2_f <- factor(all.cons_eth$variable2, 
                                   levels = var2order)
all.cons_race$variable2_f <- factor(all.cons_race$variable2, 
                                   levels = var2order)
all.cons_intersect$variable2_f <- factor(all.cons_intersect$variable2, 
                                   levels = var2order)

all.cons_eth <- all.cons_eth[order(all.cons_eth$variable2_f),]
all.cons_race <- all.cons_race[order(all.cons_race$variable2_f),]
all.cons_intersect <- all.cons_intersect[order(all.cons_intersect$variable2_f),]

# insert blank rows between tables----
new_eth <- NULL
for(i in unique(all.cons_eth$variable2)){
  new_eth <- rbind(new_eth, 
                   all.cons_eth[all.cons_eth$variable2 == i,])
  new_eth <- rows_append(x = new_eth, 
                         y = data.frame(tbl_type = c("", NA), 
                                        variable2 = c("", NA))) 
}
new_eth <- rows_append(x = new_eth[is.na(new_eth$tbl_type),][1,], 
                       y = new_eth)  
for(i in 1:nrow(new_eth)){
  if(is.na(new_eth$names[i]) & 
     !is.na(new_eth$variable2[i+1])){
    new_eth$names[i] <- new_eth$variable2[i+1]
  }
}
new_eth

new_race <- NULL
for(i in unique(all.cons_race$variable2)){
  new_race <- rbind(new_race, 
                    all.cons_race[all.cons_race$variable2 == i,])
  new_race <- rows_append(x = new_race, 
                         y = data.frame(tbl_type = c("", NA), 
                                        variable2 = c("", NA))) 
}
new_race <- rows_append(x = new_race[is.na(new_race$tbl_type),][1,], 
                       y = new_race)  
for(i in 1:nrow(new_race)){
  if(is.na(new_race$names[i]) & 
     !is.na(new_race$variable2[i+1])){
    new_race$names[i] <- new_race$variable2[i+1]
  }
}
new_race

new_intersect <- NULL
for(i in unique(all.cons_intersect$variable2)){
  new_intersect <- rbind(new_intersect, 
                         all.cons_intersect[all.cons_intersect$variable2 == i,])
  new_intersect <- rows_append(x = new_intersect, 
                          y = data.frame(tbl_type = c("", NA), 
                                         variable2 = c("", NA))) 
}
new_intersect <- rows_append(x = new_intersect[is.na(new_intersect$tbl_type),][1,], 
                        y = new_intersect)  
for(i in 1:nrow(new_intersect)){
  if(is.na(new_intersect$names[i]) & 
     !is.na(new_intersect$variable2[i+1])){
    new_intersect$names[i] <- new_intersect$variable2[i+1]
  }
}
new_intersect

new_eth <- new_eth[!colnames(new_eth) %in% c("hisp", "variable2_f")]
new_race <- new_race[!colnames(new_race) %in% c("hisp", "variable2_f")]
new_intersect <- new_intersect[!colnames(new_intersect) %in% c("hisp", "variable2_f")]
# write csv files----
write.csv(x = new_eth,#all.cons_eth, 
          "all_cons_eth2023.csv")
write.csv(x = new_intersect,#all.cons_intersect, 
          "all.cons_intersect2023.csv")
write.csv(x = new_race,#all.cons_race, 
          "all.cons_race2023.csv")


# charts

all.bind <- rbind(all.cons_eth, 
                  all.cons_race, 
                  all.cons_intersect)

all.bind$n_NA <- is.na(all.bind$FY18) +
  is.na(all.bind$FY19) +
  is.na(all.bind$FY20) + 
  is.na(all.bind$FY21) + 
  is.na(all.bind$FY22)

all.bind[all.bind$n_NA > 0,] %>%
  group_by(tbl_type, variable2, names) %>%
  summarise(n = n())

as.data.table(all.bind[,c("tbl_type", "variable2_f", "names", 
                          "FY18", "FY19", "FY20", "FY21", "FY22")]) %>%
  melt(., id.vars = c("tbl_type", "variable2_f", "names")) %>% as_tibble()%>%
  .[.$tbl_type == "race",] %>%
  ggplot(data = ., 
         aes(x = as.numeric(variable), y = value, color = names))+
  geom_line() + 
  geom_point()+
  facet_wrap(variable2_f~., scales = "free_y")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

as.data.table(all.bind[,c("tbl_type", "variable2_f", "names", 
                          "FY18_to_FY19", "FY19_to_FY20", "FY20_to_FY21", "FY21_to_FY22")]) %>%
  melt(., id.vars = c("tbl_type", "variable2_f", "names")) %>% as_tibble()%>%
  .[.$tbl_type == "race",] %>%
  ggplot(data = ., 
         aes(x = as.numeric(variable), y = value, color = names))+
  geom_path() + 
  geom_point()+
  facet_wrap(variable2_f~.,scales = "free_y")+
  theme(legend.position = "bottom", legend.direction = "horizontal")


as.data.table(all.bind[,c("tbl_type", "variable2_f", "names", 
                          "FY18_pct_share", "FY19_pct_share", "FY20_pct_share", 
                          "FY21_pct_share", "FY22_pct_share")]) %>%
  melt(., id.vars = c("tbl_type", "variable2_f", "names")) %>% 
  as_tibble() %>%
  .[.$tbl_type == "race",] %>%
  ggplot(data = ., 
         aes(x = as.numeric(variable), y = value, color = names))+
  geom_path() + 
  geom_point()+
  facet_wrap(variable2_f~.,scales = "free_y")+
  theme(legend.position = "bottom", legend.direction = "horizontal")



# charts----


unique(all.cons_intersect$variable2) # returns, exits, length of stay, population

unique(all.cons_intersect$names)


# 6month vs 12month returns for all types----
# white & black vs others
# pct_change

# return types
m6v12_returns <- all.cons_intersect %>%
  .[grepl("returns_from", x = .$variable2, 
          ignore.case = T),]
# months
m6v12_returns$ret_months <- NA
m6v12_returns$ret_months[grepl("6_Months", m6v12_returns$variable2)] <- "After 6 Months"
m6v12_returns$ret_months[grepl("12_Months", m6v12_returns$variable2)] <- "After 12 Months"

# race
m6v12_returns <- m6v12_returns #%>%  .[is.na(.$hisp),]

m6v12_returns$race2 <- NA
m6v12_returns$race2[grepl("White", m6v12_returns$names)] <- "White"
m6v12_returns$race2[grepl("Black", m6v12_returns$names)] <- "Black"
m6v12_returns$race2[grepl("All HH", m6v12_returns$names)] <- "All HH"
m6v12_returns$race2[grepl("Latino", m6v12_returns$names) & 
                      grepl("Non-", m6v12_returns$names)] <- "Non-Hispanic"

m6v12_returns$race2[grepl("Latino", m6v12_returns$names) & 
                      is.na(m6v12_returns$race2)]        <- "Hispanic"

m6v12_returns$race2[is.na(m6v12_returns$race2)] <- "Other"

m6v12_returns <- m6v12_returns[!grepl("HISP",m6v12_returns$names),] 


m6v12_returns <- m6v12_returns %>%
  group_by(ret_months, race2, variable2) %>%
  summarise(t18 = sum(FY18), 
            t19 = sum(FY19), 
            t20 = sum(FY20), 
            t21 = sum(FY21), 
            t22 = sum(FY22))

m6v12_returns$t18_19 <- (m6v12_returns$t19 - m6v12_returns$t18) / m6v12_returns$t18
m6v12_returns$t19_20 <- (m6v12_returns$t20 - m6v12_returns$t19) / m6v12_returns$t19
m6v12_returns$t20_21 <- (m6v12_returns$t21 - m6v12_returns$t20) / m6v12_returns$t20
m6v12_returns$t21_22 <- (m6v12_returns$t22 - m6v12_returns$t21) / m6v12_returns$t21

the.chart_hips <- NA

the.chart <- rbind(cbind(melt(as.data.table(m6v12_returns[,c(1:3,4:8)]), 
                 id.vars = c("ret_months", "race2", "variable2"), 
                 variable.factor = F),
            data.frame(chart = "A")),
      cbind(melt(as.data.table(m6v12_returns[,c(1:3,9:12)]), 
                 id.vars = c("ret_months", "race2", "variable2"),
                 variable.factor = F),
            data.frame(chart = "B"))) %>%
  as_tibble() %>%
  .[.$chart == "A",]

the.chart$race_f <- factor(the.chart$race2, 
                           levels = c("All HH", "Hispanic", 
                                      "Non-Hispanic", 
                                      "Black", "White", "Other"))

the.chart$variable2 <-  gsub("_within.*Months", "",the.chart$variable2)

as_tibble(the.chart)$variable2 %>% unique()
the.chart$ret_months_f <- factor(the.chart$ret_months, 
                                 levels = c("After 6 Months", 
                                            "After 12 Months"))


# ggplot(data = the.chart, 
#          aes(x = as.numeric(factor(variable)), y = value, color = race2)) + 
#   geom_line() + 
#   geom_point()+
#   facet_grid(variable2~ret_months_f, scales = "free_y") +
#   scale_x_continuous(labels = unique(the.chart$variable))+
#   scale_y_continuous(labels = scales::comma)+
#   theme(legend.position = "bottom", legend.direction = "vertical")+
#   labs(title = "Returns to Homelessness after Exit")
#   
# ggplot(data = the.chart[the.chart$variable2 == "Returns_From_All_Exit_Types",], 
#        aes(x = ret_months_f, y = value, fill = ret_months_f) ) + 
#   #geom_line() + 
#   geom_col(color = "black")+
#   facet_grid(race2~variable, scales = "free_y") +
#   scale_x_discrete()+
#   scale_y_continuous(labels = scales::comma)+
#   theme(legend.position = "none", legend.direction = "vertical", 
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   labs(title = "Returns to Homelessness after Exit")

ggplot(data = the.chart,#[the.chart$variable2 == "Returns_From_All_Exit_Types",], 
       aes(x = variable, 
           group = ret_months_f, y = value, color = ret_months_f) ) + 
  geom_point(size = 4) + 
  geom_line(#color = "black"#,
            #position = "dodge"
            size = 1)+
  facet_grid(race_f~variable2, scales = "free_y") +
  scale_x_discrete(name = "year")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom", legend.direction = "vertical", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        strip.text.y = element_text(angle = 0))+
  labs(title = "Returns to Homelessness after Exit")+
  scale_color_discrete(name = "Returned After (Months)")

# year-to-year, 
y2y <- all.cons_intersect %>%
  .[grepl(pattern = "returns", 
          x=.$variable2,
          ignore.case = T),] %>%
  #.[is.na(.$hisp),]%>%
  .[colnames(.) %in% c("variable2", "names", "hisp", 
                       "FY18", "FY19", "FY20", "FY21", "FY22")] %>%
                       #"FY18_to_FY19", "FY19_to_FY20", 
                       #"FY20_to_FY21", "FY21_to_FY22")] %>%
  as.data.table() %>%
  melt(., id.vars = c("variable2", "names", "hisp"))

y2y$year_int <- as.numeric(gsub(pattern = "\\D", "",  y2y$variable)) + 2000
as_tibble(y2y)

if(all(y2y$year_int > 2100)){
  temp <- data.frame(year_int = unique(y2y$year_int),
             year_order = order(unique(y2y$year_int)))
  for(i in 1:nrow(temp)){
    y2y$year_int[y2y$year_int == temp$year_int[i]] <- temp$year_order[i]
  }
  
}
y2y$year_int


y2y$race2 <- NA
y2y$race2[grepl("white", y2y$names, ignore.case = T)] <- "White"
y2y$race2[grepl("black", y2y$names, ignore.case = T)] <- "Black" 
y2y$race2[!grepl("white|black|latino|all hh", y2y$names, ignore.case = T)] <- "Other (Median Value)"

y2y <- y2y[!is.na(y2y$race2),]

y2y %>%
  as_tibble() %>%
  group_by(variable2, names, year_int) %>%
  summarise(avg_val = median(value, na.rm = T)) %>%
  ggplot(data = .,#[!is.na(y2y$race2),],
       aes(x = year_int, y = avg_val, color = variable2)) + 
  geom_point(size =3)+
  geom_smooth(se = F, size = 1)+
  facet_wrap(names~., scales = "free_y")+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_x_continuous(name = "Year")+
  scale_y_continuous(name = "Returns from Exit", 
                     labels = scales::comma)+
  labs(title = "Returns by Race and Ethnicity, 2018-2022")

y2y$eth_TF <- ifelse(is.na(y2y$hisp),F,T)
y2y$race_maj.min <- ifelse(grepl("white|black", 
                                 y2y$race2, ignore.case = T), 
                           "White or black", "other")
y2y$eth_type <- ifelse(is.na(y2y$hisp), "N/A", NA)
y2y$eth_type[is.na(y2y$eth_type)] <- ifelse(grepl("non-hisp", 
                                                  y2y$names[is.na(y2y$eth_type)],
                                                                  ignore.case = T), 
                                                  "Non-Hispanic", "Hispanic")
y2y %>%
  .[.$variable2 == unique(.$variable2)[1],] %>%
  as_tibble() %>%
  group_by(variable2, names, year_int, race2, eth_TF,eth_type,
           race_maj.min) %>%
  summarise(avg_val = median(value, na.rm = T)) %>%
  ggplot(data = .,#[!is.na(y2y$race2),],
         aes(x = year_int, y = avg_val, color = names)) + 
  geom_point(size =3)+
  geom_smooth(se = F, size = 1)+
  facet_wrap(variable2+eth_type + race2~., scales = "free_y")+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  scale_x_continuous(name = "Year")+
  scale_y_continuous(name = "Returns from Exit", 
                     labels = scales::comma)+
  labs(title = "Returns by Race and Ethnicity, 2018-2022")



# pct_change_each_year, 
y2y <- all.cons_intersect %>%
  .[grepl(pattern = "exits", 
          x=.$variable2,
          ignore.case = T),] %>%
  .[is.na(.$hisp),]%>%
  .[colnames(.) %in% c("variable2", "names", "hisp", 
                       #"FY18", "FY19", "FY20", "FY21", "FY22_half")] %>%
                       "FY18_to_FY19", "FY19_to_FY20", 
                       "FY20_to_FY21", "FY21_to_FY22")] %>%
  as.data.table() %>%
  melt(., id.vars = c("variable2", "names", "hisp"))
y2y$year_int <- as.numeric(gsub(pattern = "\\D", "",  y2y$variable)) + 2000

if(all(y2y$year_int > 2100)){
  temp <- data.frame(year_int = unique(y2y$year_int),
                     year_order = order(unique(y2y$year_int)))
  for(i in 1:nrow(temp)){
    y2y$year_int[y2y$year_int == temp$year_int[i]] <- temp$year_order[i]
  }
  
}
y2y$year_int


y2y$race2 <- NA
y2y$race2[grepl("white", y2y$names, ignore.case = T)] <- "White"
y2y$race2[grepl("black", y2y$names, ignore.case = T)] <- "Black" 
y2y$race2[!grepl("white|black|latino|all hh", y2y$names, ignore.case = T)] <- "Other (Median Value)"

y2y <- y2y[!is.na(y2y$race2),]

y2y %>%
  as_tibble() %>%
  group_by(variable2, race2, year_int) %>%
  summarise(avg_val = median(value, na.rm = T)) %>%
  ggplot(data = .,#[!is.na(y2y$race2),],
         aes(x = year_int, y = avg_val, color = race2)) + 
  geom_point(size =3)+
  geom_smooth(se = F, size = 1)+
  facet_wrap(variable2~., scales = "free_x")+
  theme(legend.position = "bottom")+
  scale_x_continuous(name = "Year", 
                     labels = paste("FY", 
                                    c("18-19","19-20","20-21","21-22"), 
                                    sep = ""))+
  scale_y_continuous(name = "Percent Change", 
                     labels = scales::percent)+
  labs(title = "Percent Change in Returns by Major Race Groups, 2018-2022")

# pct_share_by_race
psr <- all.cons_eth %>%
  .[grepl(pattern = "returns", 
          x=.$variable2,
          ignore.case = T),] %>%
  #.[is.na(.$hisp),]%>%
  .[colnames(.) %in% c("variable2", "names", "hisp", 
                       "FY18_pct_share", "FY19_pct_share",
                         "FY20_pct_share", "FY21_pct_share",
                       "FY22_pct_share")] %>%
                       #"FY18", "FY19", "FY20", "FY21", "FY22_half")] %>%
                       # "FY18_to_FY19", "FY19_to_FY20", 
                       # "FY20_to_FY21", "FY21_to_FY22")] %>%
  as.data.table() %>%
  melt(., id.vars = c("variable2", "names", "hisp"))
psr$year_int <- as.numeric(gsub(pattern = "\\D", "",  psr$variable)) + 2000

if(all(psr$year_int > 2100)){
  temp <- data.frame(year_int = unique(psr$year_int),
                     year_order = order(unique(psr$year_int)))
  for(i in 1:nrow(temp)){
    psr$year_int[psr$year_int == temp$year_int[i]] <- temp$year_order[i]
  }
  
}
psr$year_int


psr$race2 <- NA
psr$race2[grepl("white", psr$names, ignore.case = T)] <- "White"
#psr$race2[grepl("black", psr$names, ignore.case = T)] <- "Black" 
psr$race2[!grepl("white|latino|all hh", psr$names, ignore.case = T)] <- "Other (Median Value)"

#psr <- psr[!is.na(psr$race2),]

psr %>%
  .[grepl("latino", .$names, ignore.case = T),] %>%
  as_tibble() %>%
  group_by(variable2, names, year_int) %>%
  summarise(avg_val = mean(value, na.rm = T)) %>%
  ggplot(data = .,#[!is.na(psr$race2),],
         aes(x = year_int, y = avg_val, color = variable2)) + 
  geom_point(size =3)+
  geom_smooth(se = F, size = 1)+
  facet_wrap(names~., scales = "free")+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_x_continuous(name = "Year")+
  scale_y_continuous(name = "Percent Change", 
                     labels = scales::percent, 
                     limits = c(NA,NA))+
  labs(title = "Percent Change in Returns by Ethnicity, 2018-2022")

# PIT----
pit.data <- read_tsv(I("geo	race	2017_people experiencing homelessness by race (PIT)	2018_people experiencing homelessness by race (PIT)	2019_people experiencing homelessness by race (PIT)	2020_people experiencing homelessness by race (PIT)	2021_people experiencing homelessness by race (PIT)	2022_people experiencing homelessness by race (PIT)
NC	hispanic	518		411	460	320	
NC	non hispanic	8,444		8,903	8,820	6,822	
NC	asian pacific islander	54		50	50	53	
NC	black	4,553		4,832	4,757	4,254	
NC	native american alaskan	190		94	93	75	
NC	white	3,745		3,992	4,060	2,527	
NC	other multi racial	420		346	320	233	
NC-503	hispanic	160		121	156	94	
NC-503	non hispanic	2,894		2,868	3,029	1,426	
NC-503	asian pacific islander	15		19	19	16	
NC-503	black	1,139		1,112	1,205	651	
NC-503	native american alaskan	120		47	51	30	
NC-503	white	1,620		1,696	1,793	768	
NC-503	other multi racial	160		115	117	55	
"))

pit.data <- pit.data[,c(1,2,5,6,7)]

pit.data$hisp <- grepl("hispanic", pit.data$race)

pit.data2 <- pit.data %>%
  as.data.table() %>%
  melt(., 
       id.vars= c("geo", "race", "hisp"))

pit.data2$year_int <- as.numeric(gsub("\\D", "", pit.data2$variable))

as_tibble(pit.data2) 
pit.data$race %>% unique()

pit.data2$race_eth_grp <- "race"
pit.data2$race_eth_grp[grepl("hispanic", pit.data2$race)] <- "ethnicity"

library(glue)

pit.data2$race_eth_pct_share <- NA
for(y1 in unique(pit.data2$year_int)){
  for(g1 in unique(pit.data2$geo[pit.data2$year_int == y1])){
    for(reg1 in unique(pit.data2[pit.data2$geo == g1 & 
                                 pit.data2$year_int == y1,]$race_eth_grp)){
      for(i in 1:nrow(pit.data2[pit.data2$geo == g1 & 
                                pit.data2$race_eth_grp == reg1 & 
                                pit.data2$year_int == y1,])){
        #cat(glue("{y1}-{g1}-{reg1}-{i}\n\n"))
        pit.data2$race_eth_pct_share[pit.data2$geo == g1 & 
                                       pit.data2$race_eth_grp == reg1 & 
                                       pit.data2$year_int == y1][i] <- pit.data2[pit.data2$geo == g1 & 
                    pit.data2$race_eth_grp == reg1 & 
                    pit.data2$year_int == y1,][i,]$value / 
          sum(pit.data2[pit.data2$geo == g1 & 
                          pit.data2$race_eth_grp == reg1 & 
                          pit.data2$year_int == y1,]$value)
      }
    }
  }
}

pit.data2 %>%
  group_by(year_int, geo, race_eth_grp) %>%
  summarise(t_reps = scales::percent(sum(race_eth_pct_share)))

pit.data2$race_f <- factor(pit.data2$race, 
                           levels = unique(pit.data2$race)[c(1,2,4,6,7,5,3)])

ggplot(data = pit.data2, 
       aes(x = year_int, y = value, color = race_f)) + 
  geom_line(aes(linetype = geo), size = 1)+
  geom_point(size = 3)+
  facet_wrap(race_f~., scales = "free")+
  scale_x_continuous(breaks = seq(2000,3000,by=1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  

ggplot(data = pit.data2, 
       aes(x = year_int, y = race_eth_pct_share, color = race_f)) + 
  geom_line(aes(linetype = geo), size = 1)+
  geom_point(size = 3)+
  facet_wrap(race_f~., scales = "free")+
  scale_x_continuous(breaks = seq(2000,3000,by=1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(labels = scales::percent)

# statewide concentration of races

pit.data2$BOS_pct_of_NC <- ifelse(pit.data2$geo == "NC", 1, NA)
for(y1 in unique(pit.data2$year_int)){
  for(reg1 in unique(pit.data2$race_eth_grp[pit.data2$year_int == y1])){
    for(r1 in unique(pit.data2$race[pit.data2$year_int == y1 & 
                                    pit.data2$race_eth_grp == reg1])){
      pit.data2[pit.data2$year_int == y1 & 
                  pit.data2$race_eth_grp == reg1 & 
                  pit.data2$race == r1 & 
                  pit.data2$geo == "NC-503",]$BOS_pct_of_NC <- pit.data2[pit.data2$year_int == y1 & 
                  pit.data2$race_eth_grp == reg1 & 
                  pit.data2$race == r1 & 
                  pit.data2$geo == "NC-503",]$value / 
        pit.data2[pit.data2$year_int == y1 & 
                  pit.data2$race_eth_grp == reg1 & 
                  pit.data2$race == r1 & 
                  pit.data2$geo == "NC",]$value
    }
  }
}

ggplot(data = pit.data2[pit.data2$geo == "NC-503",], 
       aes(x = year_int, y = BOS_pct_of_NC, fill = race_eth_grp)) + 
  geom_col()+
  facet_wrap(~race_f)+
  scale_x_continuous(breaks = seq(2000,3000,by=1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = "right", legend.direction = "vertical")+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,100,by=.10))+
  labs(title = "Proportion of BoS People Experiencing Homelessness vs State of NC, 
       by year")


# total homelessness NC vs BoS

pit.data2 %>%
  group_by(year_int, geo) %>%
  summarise()



# SLIDE 34----
slide34 <- all.cons_intersect %>%
  .[,c(1:9)] %>%
  as.data.table() %>%
  melt(.,id.vars = c("tbl_type", "variable2", "names", "hisp"), 
       variable.name = "year", value.name = "value", 
       variable.factor = F) %>% 
  dcast(., 
        tbl_type + names + hisp + year ~ variable2) %>%
  as_tibble() 

colnames(slide34)


all.cons_intersect

ggplot() + 
  geom_col(data = slide34, 
           aes(x = Returns_From_Permanent_Housing_within_12_Months, y = names))
