library(dplyr)
library(lubridate)
library(tidycensus)
library(data.table)
library(readr)

rm(list=ls()[!ls() %in% c("vars.acs5")]) # clears all variables (except for variables so you don't have to download it again)
cat('\f');gc()

setwd("C:/Users/TimBender/Documents/R/ncceh/data/census_data")


# Tables / Variables----

gh.vars <- "https://raw.githubusercontent.com/timbender-ncceh/Racial_Equity_Analysis/main/census_table_source_info.csv" %>%
  read_csv() %>%
  .[,c(2:8),] %>%
  .[!duplicated(.),]

gh.vars$rid <- 1:nrow(gh.vars)


# pull variables list
if(!"vars.acs5" %in% ls()){ # memory saving logic so you don't DL large dataset if you alrady have it.
  vars.acs5 <- tidycensus::load_variables(year    = 2021,
                                          dataset = "acs5")
}


# build ds----
out <- gh.vars %>%
  group_by(rid, population, subpopulation) %>%
  summarise()

out2 <- NULL


for(p in unique(gh.vars$population)){
  for(s in unique(gh.vars$subpopulation)){
    # All.PPL_total
    try(temp.rid <- gh.vars[gh.vars$population == p & gh.vars$subpopulation == s,]$rid)
    try(temp.df <- gh.vars[gh.vars$rid == temp.rid,])
    try(temp.vars <- temp.df %>%
      .$ALL.PPL_total %>%
      strsplit(x = ., split = " \\+ |\\) - \\(| - ") %>%
            unlist() %>%
            gsub(pattern = "\\(|\\)", replacement = "", x = .) %>%
            unique() %>%
            sort())
    
    try(out2 <- rbind(out2, 
                  data.frame(rid = temp.rid, 
                             variable_name = temp.vars, 
                             population = p, 
                             subpopulation = s, 
                             poptype = "All.PPL", 
                             famtype = "total")) )
    rm(temp.rid, temp.df, temp.vars)

    
    # All.PPL_in.fwc
    try(temp.rid <- gh.vars[gh.vars$population == p & gh.vars$subpopulation == s,]$rid)
    try(temp.df <- gh.vars[gh.vars$rid == temp.rid,])
    try(temp.vars <- temp.df %>%
          .$ALL.PPL_in.fwc %>%
          strsplit(x = ., split = " \\+ |\\) - \\(| - ") %>%
          unlist() %>%
          gsub(pattern = "\\(|\\)", replacement = "", x = .) %>%
          unique() %>%
          sort())
    
    try(out2 <- rbind(out2, 
                  data.frame(rid = temp.rid, 
                             variable_name = temp.vars, 
                             population = p, 
                             subpopulation = s, 
                             poptype = "All.PPL", 
                             famtype = "in.fwc")) )
    rm(temp.rid, temp.df, temp.vars)
    
    # IN.POV_total
    try(temp.rid <- gh.vars[gh.vars$population == p & gh.vars$subpopulation == s,]$rid)
    try(temp.df <- gh.vars[gh.vars$rid == temp.rid,])
    try(temp.vars <- temp.df %>%
          .$IN.POV_total %>%
          strsplit(x = ., split = " \\+ |\\) - \\(| - ") %>%
          unlist() %>%
          gsub(pattern = "\\(|\\)", replacement = "", x = .) %>%
          unique() %>%
          sort())
    
    try(out2 <- rbind(out2, 
                  data.frame(rid = temp.rid, 
                             variable_name = temp.vars, 
                             population = p, 
                             subpopulation = s, 
                             poptype = "IN.POV", 
                             famtype = "total")))
    rm(temp.rid, temp.df, temp.vars)
    
    # IN.POV_in.fwc
    try(temp.rid <- gh.vars[gh.vars$population == p & gh.vars$subpopulation == s,]$rid)
    try(temp.df <- gh.vars[gh.vars$rid == temp.rid,])
    try(temp.vars <- temp.df %>%
          .$IN.POV_in.fwc %>%
          strsplit(x = ., split = " \\+ |\\) - \\(| - ") %>%
          unlist() %>%
          gsub(pattern = "\\(|\\)", replacement = "", x = .) %>%
          unique() %>%
          sort())
    
    try(out2 <- rbind(out2, 
                  data.frame(rid = temp.rid, 
                             variable_name = temp.vars, 
                             population = p, 
                             subpopulation = s, 
                             poptype = "IN.POV", 
                             famtype = "in.fwc"))) 
    rm(temp.rid, temp.df, temp.vars)
    
  }
}


out <- left_join(out, out2)
rm(out2)

out <- left_join(out, 
          vars.acs5[,c("name", "concept", "label")], 
          by = c("variable_name" = "name")) %>%
  #.[!colnames(.) %in% "rid"] %>%
  .[!duplicated(.),]


out <- out %>%
  group_by(population,subpopulation) %>%
  summarise() %>%
  mutate(., 
         summary_group = 1:length(population)) %>%
  right_join(., out)

# calculation

out <- gh.vars[,c(1:6,8)] %>%
  as.data.table() %>%
  melt(., 
       id.vars = c("population", "subpopulation", "rid"), 
       variable.name = "colname", 
       value.name = "group_calculation") %>%
  as.data.frame() %>%
  as_tibble() %>%
  .[complete.cases(.),] %>%
  mutate(., 
         poptype = gsub("_.*$", "", colname), 
         famtype = gsub("^.*_", "", colname)) %>%
  .[,c(1:3,5:7)] %>%
  right_join(., 
             out) %>%
  .[order(.$summary_group, .$subpopulation, .$population),] %>%
  .[!colnames(.) %in% c("summary_group")] %>%
  .[!is.na(.$group_calculation),] %>% 
  .[!duplicated(.),] %>%
  .[order(.$group_calculation),]

out2 <- NULL
for(yi in c(2018,2019,2020,2021)){
  
  temp <- tidycensus::get_acs(geography = "county", 
                              variables = out$variable_name, 
                              year = yi, 
                              state = "NC", 
                              survey = "acs5") %>%
    mutate(., 
           year = yi)
  out2 <- rbind(out2, 
                temp)
  rm(temp)
}

gc()

out3 <- left_join(out, out2, by = c("variable_name" = "variable"))

# out3 %>%
#   .[,c(1,2,4,5,6,7,8,9)] %>%
#   .[!duplicated(.),]

out3 %>%
  as.data.table() %>%
  dcast(., 
        population + subpopulation + 
          concept + label + 
          year + poptype + famtype + NAME ~ variable_name, 
        fun.aggregate = sum, 
        value.var = "estimate", 
        fill = NA) %>%
  .[!duplicated(.),] %>% nrow()

out4 <- out3 %>%
  as.data.table() %>%
  dcast(., 
        population + 
          subpopulation + 
          poptype + 
          famtype +
          group_calculation + 
          #concept + 
          #label + 
          year +  NAME ~ variable_name, 
        fun.aggregate = sum, 
        value.var = "estimate", 
        fill = NA)

gc()

rm(out, out2, out3)
gc()




c("sum(", paste(paste("out5$", colnames(out5)[8:ncol(out5)], sep = ""), sep = "[i], ", 
#c("sum(", paste(colnames(out4)[8:ncol(out4)], sep = "[i], ", 
      collapse = "[i], "), "[i], na.rm = T)") %>% 
  paste(., sep = "", collapse = "") %>%
  cat()

out5 <- out4
out5$out_calc <- NA

for(i in 1:nrow(out5)){
  out5$out_calc[i] <- sum(out5$B17001A_004[i], out5$B17001A_005[i], out5$B17001A_006[i], out5$B17001A_007[i], out5$B17001A_008[i], out5$B17001A_009[i], out5$B17001A_010[i], out5$B17001A_018[i], out5$B17001A_019[i], out5$B17001A_020[i], out5$B17001A_021[i], out5$B17001A_022[i], out5$B17001A_023[i], out5$B17001A_024[i], out5$B17001B_004[i], out5$B17001B_005[i], out5$B17001B_006[i], out5$B17001B_007[i], out5$B17001B_008[i], out5$B17001B_009[i], out5$B17001B_010[i], out5$B17001B_018[i], out5$B17001B_019[i], out5$B17001B_020[i], out5$B17001B_021[i], out5$B17001B_022[i], out5$B17001B_023[i], out5$B17001B_024[i], out5$B17001C_004[i], out5$B17001C_005[i], out5$B17001C_006[i], out5$B17001C_007[i], out5$B17001C_008[i], 
                          out5$B17001C_009[i], out5$B17001C_010[i], out5$B17001C_018[i], out5$B17001C_019[i], out5$B17001C_020[i], out5$B17001C_021[i], out5$B17001C_022[i], out5$B17001C_023[i], out5$B17001C_024[i], out5$B17001D_004[i], out5$B17001D_005[i], out5$B17001D_006[i], out5$B17001D_007[i], out5$B17001D_008[i], out5$B17001D_009[i], out5$B17001D_010[i], out5$B17001D_018[i], out5$B17001D_019[i], out5$B17001D_020[i], out5$B17001D_021[i], out5$B17001D_022[i], out5$B17001D_023[i], out5$B17001D_024[i], out5$B17001E_004[i], out5$B17001E_005[i], out5$B17001E_006[i], out5$B17001E_007[i], out5$B17001E_008[i], out5$B17001E_009[i], out5$B17001E_010[i], out5$B17001E_018[i], out5$B17001E_019[i], out5$B17001E_020[i], out5$B17001E_021[i], out5$B17001E_022[i], out5$B17001E_023[i], 
                          out5$B17001E_024[i], out5$B17001F_004[i], out5$B17001F_005[i], out5$B17001F_006[i], out5$B17001F_007[i], out5$B17001F_008[i], out5$B17001F_009[i], out5$B17001F_010[i], out5$B17001F_018[i], out5$B17001F_019[i], out5$B17001F_020[i], out5$B17001F_021[i], out5$B17001F_022[i], out5$B17001F_023[i], out5$B17001F_024[i], out5$B17001G_004[i], out5$B17001G_005[i], out5$B17001G_006[i], out5$B17001G_007[i], out5$B17001G_008[i], out5$B17001G_009[i], out5$B17001G_010[i], out5$B17001G_018[i], out5$B17001G_019[i], out5$B17001G_020[i], out5$B17001G_021[i], out5$B17001G_022[i], out5$B17001G_023[i], out5$B17001G_024[i], out5$B17001I_004[i], out5$B17001I_005[i], out5$B17001I_006[i], out5$B17001I_007[i], out5$B17001I_008[i], out5$B17001I_009[i], out5$B17001I_010[i], 
                          out5$B17001I_018[i], out5$B17001I_019[i], out5$B17001I_020[i], out5$B17001I_021[i], out5$B17001I_022[i], out5$B17001I_023[i], out5$B17001I_024[i], out5$B17001_004[i], out5$B17001_005[i], out5$B17001_006[i], out5$B17001_007[i], out5$B17001_008[i], out5$B17001_009[i], out5$B17001_010[i], out5$B17001_018[i], out5$B17001_019[i], out5$B17001_020[i], out5$B17001_021[i], out5$B17001_022[i], out5$B17001_023[i], out5$B17001_024[i], out5$B17010A_004[i], out5$B17010A_011[i], out5$B17010A_017[i], out5$B17010B_004[i], out5$B17010B_011[i], out5$B17010B_017[i], out5$B17010C_004[i], out5$B17010C_011[i], out5$B17010C_017[i], out5$B17010D_004[i], out5$B17010D_011[i], out5$B17010D_017[i], out5$B17010E_004[i], out5$B17010E_011[i], out5$B17010E_017[i], out5$B17010F_004[i], out5$B17010F_011[i], 
                          out5$B17010F_017[i], out5$B17010G_004[i], out5$B17010G_011[i], out5$B17010G_017[i], out5$B17010I_004[i], out5$B17010I_011[i], out5$B17010I_017[i], out5$B17010_004[i], out5$B17010_011[i], out5$B17010_017[i], out5$B17020A_002[i], out5$B17020B_002[i], out5$B17020C_002[i], out5$B17020D_002[i], out5$B17020E_002[i], out5$B17020F_002[i], out5$B17020G_002[i], out5$B17020I_002[i], out5$B17020_002[i], out5$out_calc[i], na.rm = T)
    }
gc()

out5 %>%
  as_tibble()




rm(out4)

gc()

out5

write_csv(x = out5, "NEW_census_rea_output_data_2018_to_2022.csv")
