rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

# Redacted FEMA NFIP claims dataset which comes from here https://www.fema.gov/media-library/assets/documents/180374#
setwd('/Users/michaellink/Desktop/__NYCDSA/_Projects/Shiny/fema_claims_new')

raw.df = read.csv('./data/openFEMA_claims20190831.csv')

# Creating annual version of raw.df with most important cost columns
filter.raw.df = 
  raw.df %>% 
  mutate(., amountpaidonbuildingclaim = ifelse(is.na(amountpaidonbuildingclaim), 0, amountpaidonbuildingclaim)) %>%
  mutate(., amountpaidoncontentsclaim = ifelse(is.na(amountpaidoncontentsclaim), 0, amountpaidoncontentsclaim)) %>% 
  mutate(., amountpaidonincreasedcostofcomplianceclaim = ifelse(is.na(amountpaidonincreasedcostofcomplianceclaim), 0, amountpaidonincreasedcostofcomplianceclaim)) %>% 
  mutate(., amountpaidtotal = 
           amountpaidonbuildingclaim +
           amountpaidoncontentsclaim +
           amountpaidonincreasedcostofcomplianceclaim) %>% 
  mutate(., floodzone = gsub('[[:digit:]]+', '', floodzone)) %>% 
  mutate(., floodzone = ifelse(str_detect(floodzone, "A") | str_detect(floodzone, "V"), '<100_yr', floodzone)) %>% 
  mutate(., floodzone = ifelse(str_detect(floodzone, "B"), '<500_yr', floodzone)) %>%
  mutate(., floodzone = ifelse(str_detect(floodzone, "X"), '~500_yr', floodzone)) %>%
  mutate(., floodzone = ifelse(str_detect(floodzone, "C"), '>500_yr', floodzone)) %>%
  mutate(., floodzone = ifelse(str_detect(floodzone, "D"), '~100_yr', floodzone)) %>%
  mutate(., floodzone = ifelse(floodzone == "", "Not Listed",floodzone)) %>% 
  mutate(., floodzone = as.factor(floodzone)) %>% 
  filter(., floodzone != "OOO") %>% 
  filter(., !is.na(state), state != "", !is.na(yearofloss), !is.na(amountpaidtotal)) %>% 
  group_by(., state, yearofloss, floodzone) %>% 
  summarise(., amountpaidtotal = sum(amountpaidtotal))



US_abbreviations <- read.csv("/Users/michaellink/Documents/catholicism/us_census/US_abbreviations.csv", stringsAsFactors = FALSE)
US_abbreviations = rename(US_abbreviations, state = State) #rename for later join which is case sensitive

State_Names = 
  filter.raw.df %>% 
  filter(., state != "") %>% 
  group_by(., state) %>% 
  summarise(., n()) %>% 
  select(., state)
State_Names = State_Names[['state']]

Top_Ten_State_Names = 
  filter.raw.df %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  arrange(., desc(Total_Summed_Loss)) %>% 
  select(., state) %>% 
  head(., 10)
Top_Ten_State_Names = Top_Ten_State_Names[['state']]

# Assembling a new dataframe from filter.raw.df that only includes top ten states
Top_10_DF = filter.raw.df[0,]
for (i in 1:10) {
  intermediate = filter.raw.df %>% 
    filter(., state == Top_Ten_State_Names[i])
  
  Top_10_DF = rbind(Top_10_DF, intermediate)
  rm(intermediate)
}

# make the second for loop based on filter year<=i and group_by state ony
Accumulate_DF = 
  filter.raw.df %>% 
  filter(., yearofloss <= i, state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidtotal)) %>%
  group_by(., state, floodzone) %>% 
  summarise(., accumulated_loss = sum(amountpaidtotal)) %>% 
  mutate(., yearofloss = 1990)
Accumulate_DF = Accumulate_DF[0,]

for (i in min(filter.raw.df$yearofloss):max(filter.raw.df$yearofloss)) {
  intermediate = filter.raw.df %>% 
    filter(., yearofloss <= i, state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidtotal)) %>%
    group_by(., state, floodzone) %>% 
    summarise(., accumulated_loss = sum(amountpaidtotal)) %>% 
    mutate(., yearofloss = i)
  
  Accumulate_DF = rbind(Accumulate_DF, intermediate)
}

#assembling the accumulated dataframe from 0 to 1 for each state
Accumulate_DF_0_to_1 = data.frame()
for (i in 1:length(State_Names)) {
  intermediate = Accumulate_DF %>% 
    filter(., state == State_Names[i]) %>% 
    mutate(., standardized_accumulation = accumulated_loss/max(accumulated_loss))
  Accumulate_DF_0_to_1 = rbind(Accumulate_DF_0_to_1, intermediate)
}

# Import and process major storms data which came from here https://www.fema.gov/significant-flood-events
major_storms_raw = read.csv('./data/major_storms_raw.csv', header = F)

index = 1:5
major_storms_raw = major_storms_raw %>% 
  cbind(., index) %>% 
  mutate(., V1 = as.character(V1))
storms_wide <- pivot_wider(major_storms_raw, names_from = index, values_from = V1) 
Event = unlist(storms_wide[[1]])[2:length(unlist(storms_wide[[1]]))]
Year = unlist(storms_wide[[2]])[2:length(unlist(storms_wide[[2]]))]
PD_Losses = unlist(storms_wide[[3]])[2:length(unlist(storms_wide[[3]]))]
Amount_PD = unlist(storms_wide[[4]])[2:length(unlist(storms_wide[[4]]))]
Avg_PD_Losses = unlist(storms_wide[[5]])[2:length(unlist(storms_wide[[5]]))]
major_storms = data.frame(Year, Event, PD_Losses, Amount_PD, Avg_PD_Losses)
major_storms = 
  major_storms %>% 
  mutate(., Year = ifelse(is.na(as.numeric((substr(Year,1,1)))),
                          gsub(".*-","",Year),
                          gsub("-.*","", Year))) %>% 
  mutate(., Year = ifelse(as.numeric(Year) > 20, as.numeric(Year)+1900, as.numeric(Year) + 2000)) %>% 
  mutate(., PD_Losses = as.numeric(str_replace_all(PD_Losses, "[^[:alnum:]]", ""))) %>% 
  mutate(., Amount_PD = as.numeric(str_replace_all(Amount_PD, "[^[:alnum:]]", ""))) %>% 
  mutate(., Avg_PD_Losses = as.numeric(str_replace_all(Avg_PD_Losses, "[^[:alnum:]]", ""))) 

save(State_Names,
     filter.raw.df,
     Top_10_DF,
     Accumulate_DF,
     Accumulate_DF_0_to_1,
     major_storms,
     file = "./FEMA_Flood_Claims/processed_data.Rdata")

rm(list = ls())
