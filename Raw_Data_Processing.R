rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

# Redacted FEMA NFIP claims dataset which comes from here https://www.fema.gov/media-library/assets/documents/180374#
setwd('/Users/michaellink/Desktop/__NYCDSA/_Projects/Shiny/fema_claims_new')

raw.df = read.csv('./data/openFEMA_claims20190831.csv')
save(raw.df, file = '/Users/michaellink/Desktop/__NYCDSA/_Projects/rawDf.Rdata')
load('/Users/michaellink/Desktop/__NYCDSA/_Projects/rawDf.Rdata')

US_abbreviations <- read.csv("./data/US_abbreviations.csv", stringsAsFactors = FALSE)
US_abbreviations = rename(US_abbreviations, state = State) #rename for later join which is case sensitive

census_regions <- read.csv("./data/census_regions.csv", stringsAsFactors = FALSE)
census_regions = census_regions %>%
  mutate(., region_number = sapply(strsplit(census_regions$region_num," "), `[`, 2)) %>%
  mutate(., region_name = sapply(strsplit(census_regions$region_num," "), `[`, 3)) %>%
  mutate(., division_number = sapply(strsplit(census_regions$division_num," "), `[`, 2)) %>%
  mutate(., division_name = substr(census_regions$division_num, start = 12, stop = 1000)) %>% 
  select(., state, region_name, division_name)

Regions = 
  left_join(US_abbreviations, census_regions, by = 'state') %>% 
  select(., state = Code, state_name = state, region_name, division_name) %>% 
  mutate(., state = as.factor(state), state_name = as.factor(state_name), region_name = as.factor(region_name), division_name = as.factor(division_name))

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
  mutate(., state = as.factor(state)) %>% 
  left_join(., Regions, by = 'state') %>% 
  filter(., floodzone != "OOO") %>% 
  filter(., !is.na(state_name), state_name != "", !is.na(yearofloss), !is.na(amountpaidtotal)) %>% 
  group_by(., state_name, yearofloss, floodzone, region_name, division_name) %>% 
  summarise(., amountpaidtotal = sum(amountpaidtotal)) %>% 
  rename(., state = state_name)

FIPS_County_Codes = 
  read.csv('./data/FIPS_County_Codes.csv') %>% 
  select(., countycode = FIPS, county_name = Name)

raw.df %>% 
  count()
  filter(., is.na(countycode)) %>% 
  count(.,)


filter.raw.df.counties = 
  raw.df %>% 
  filter(., !is.na(countycode), countycode != '') %>% 
  left_join(., FIPS_County_Codes, by = 'countycode') %>% 
  filter(., !is.na(county_name), county_name != '') %>% 
  mutate(., UID = paste(county_name, countycode, sep = '_')) %>% 
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
  mutate(., state = as.factor(state)) %>% 
  left_join(., Regions, by = 'state') %>% 
  filter(., floodzone != "OOO") %>% 
  filter(., !is.na(state_name), state_name != "", !is.na(yearofloss), !is.na(amountpaidtotal)) %>% 
  group_by(., state_name, yearofloss, floodzone, region_name, division_name, countycode, county_name, UID) %>% 
  summarise(., amountpaidtotal = sum(amountpaidtotal)) %>% 
  rename(., state = state_name)

Accumulate_DF_Counties = 
  filter.raw.df.counties %>% 
  ungroup() %>% 
  select(., UID, yearofloss, amountpaidtotal) %>% 
  group_by(., UID) %>% 
  mutate(., accumulated_loss = cumsum(amountpaidtotal)) %>% 
  arrange(., UID)

filter.raw.df.counties = left_join(filter.raw.df.counties,
                                   Accumulate_DF_Counties,
                                   by = c("UID" = "UID",
                                          "yearofloss" = "yearofloss",
                                          'amountpaidtotal' = 'amountpaidtotal'
                                   ))

State_Names = 
  filter.raw.df %>% 
  filter(., state != "", !is.na(state)) %>% 
  group_by(., state) %>% 
  summarise(., n()) %>% 
  select(., state)
State_Names = State_Names[['state']]

County_Names = 
  filter.raw.df.counties %>% 
  filter(., county_name != "") %>% 
  mutate(., UID = paste(county_name, countycode, sep = '_')) %>% 
  group_by(., UID) %>% 
  summarise(., n()) %>% 
  select(., UID)
County_Names = County_Names[['UID']]

Top_Ten_State_Names = 
  filter.raw.df %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  arrange(., desc(Total_Summed_Loss)) %>% 
  select(., state, Total_Summed_Loss) %>% 
  head(., 10)
Top_Ten_State_Names = Top_Ten_State_Names[['state']]

Top_Ten_State_Names_Table = 
  filter.raw.df %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  arrange(., desc(Total_Summed_Loss)) %>% 
  select(., state, Total_Summed_Loss) %>% 
  head(., 10)


Top_Ten_County_Names_Table = 
  filter.raw.df.counties %>% 
  group_by(., state, county_name) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  top_n(n = 10, wt = Total_Summed_Loss)
Top_Ten_County_Names = Top_Ten_County_Names[['UID']]


# make the second for loop based on filter year<=i and group_by state ony
Accumulate_DF = 
  filter.raw.df %>% 
  filter(., yearofloss <= 1990, state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidtotal)) %>%
  group_by(., state, floodzone, region_name, division_name) %>% 
  summarise(., accumulated_loss = sum(amountpaidtotal)) %>% 
  mutate(., yearofloss = 1990)
Accumulate_DF = Accumulate_DF[0,]

for (i in min(filter.raw.df$yearofloss):max(filter.raw.df$yearofloss)) {
  intermediate = filter.raw.df %>% 
    filter(., yearofloss <= i, state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidtotal)) %>%
    group_by(., state, floodzone, region_name, division_name) %>% 
    summarise(., accumulated_loss = sum(amountpaidtotal)) %>% 
    mutate(., yearofloss = i)
  
  Accumulate_DF = rbind(Accumulate_DF, intermediate)
}

# Accumulate_DF_Counties = 
#   filter.raw.df.counties %>% 
#   filter(., yearofloss == 1990, state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidtotal)) %>%
#   group_by(., state, floodzone, county_name) %>% 
#   summarise(., accumulated_loss = sum(amountpaidtotal)) %>% 
#   mutate(., yearofloss = 1990)
# Accumulate_DF_Counties = Accumulate_DF_Counties[0,]
# 
# for (i in min(filter.raw.df.counties$yearofloss):max(filter.raw.df.counties$yearofloss)) {
#   intermediate = filter.raw.df.counties %>% 
#     filter(., yearofloss <= i, state != "", !is.na(state), !is.na(county_name), !is.na(yearofloss), !is.na(amountpaidtotal)) %>%
#     group_by(., state, floodzone, county_name) %>% 
#     summarise(., accumulated_loss = sum(amountpaidtotal)) %>% 
#     mutate(., yearofloss = i)
#   
#   Accumulate_DF_Counties = rbind(Accumulate_DF_Counties, intermediate)
# }

#assembling the accumulated dataframe from 0 to 1 for each state
Accumulate_DF_0_to_1 = Accumulate_DF[0,]
for (i in 1:length(State_Names)) {
  intermediate = Accumulate_DF %>% 
    filter(., state == State_Names[[i]][i]) %>% 
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
     Accumulate_DF,
     Accumulate_DF_0_to_1,
     major_storms,
     Top_Ten_State_Names_Table,
     Top_Ten_County_Names_Table,
     file = "./FEMA_Flood_Claims/processed_data.Rdata")

rm(list = ls())
