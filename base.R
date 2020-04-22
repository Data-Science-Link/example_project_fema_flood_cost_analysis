rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(plotly)
library(googleVis)

setwd('/Users/michaellink/Desktop/__NYCDSA/_Projects/Shiny/fema_claims/R_Scripts')

raw.df = read.csv('/Users/michaellink/Desktop/__NYCDSA/_Projects/Shiny/fema_claims/FIMA_NFIP_Redacted_Claims_Data_Set/openFEMA_claims20190831.csv')

str(raw.df)

head_claims = head(raw.df, 100)

# Creating a table of the sum of all claims for each state throughout the dataset
Total_State_Summed_Claims = 
  raw.df %>% 
  filter(., !is.na(state), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))

# Creating a table of the all claims for nation throughout the years
Total_Nation_Summed_Claims = 
  raw.df %>% 
  filter(., !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>% 
  group_by(., yearofloss) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))

# Creating a line plot of claims for the nation for whole dataset
GG_Total_Nation_Summed_Claims = 
Total_Nation_Summed_Claims %>% 
  ggplot(., aes(x = yearofloss, y = Total_Summed_Loss)) +
  geom_line() +
  labs(title = 'Annual NFIP Claims Across the Nation', x = 'year', y = '$') + 
  scale_y_continuous(labels = scales::comma)
GG_Total_Nation_Summed_Claims

# Creating a descending bar plot of the above data
GG_Summed_Claim_Cost_by_States = 
Total_State_Summed_Claims %>% 
  arrange(., desc(Total_Summed_Loss)) %>% 
  ggplot(., aes(x = reorder(state, -Total_Summed_Loss), y = Total_Summed_Loss)) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Total Money Claimed to Date by Each State', x = 'State', y = '$') + 
  theme(axis.text.x = element_text(angle = 270)) + 
  scale_y_continuous(labels = scales::comma)
GG_Summed_Claim_Cost_by_States

# Creating list of top 10 state names in terms of total claims paid 
Top_Ten_Total_State_Summed_Claims = 
  Total_State_Summed_Claims%>% 
  arrange(., desc(Total_Summed_Loss)) %>% 
  select(., state) %>% 
  head(., 10)
Top_Ten_Total_State_Summed_Claims = Top_Ten_Total_State_Summed_Claims[['state']]

# Assembling a new dataframe from Raw.df that only includes top ten states
Top_10_DF = data.frame()
for (i in 1:10) {
  intermediate = raw.df %>% 
    filter(., state == Top_Ten_Total_State_Summed_Claims[i])
  
  Top_10_DF = rbind(Top_10_DF, intermediate)
}

# Creating a descending bar plot of the total claims paid out by state for top 10 states
GG_Summed_Claim_Cost_by_Top_10_States = 
Top_10_DF %>% 
  filter(., !is.na(state), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim)) %>% 
  ggplot(., aes(x = reorder(state, -Total_Summed_Loss), y = Total_Summed_Loss)) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Total Money Claimed to Date for Top 10 States', x = 'State', y = '$') + 
  scale_y_continuous(labels = scales::comma)
GG_Summed_Claim_Cost_by_Top_10_States

# Organizing raw.df by state, year, and summed losses. Creating a table. This is to be used with a time series analysis
Annual_State_Summed_Claims = 
  raw.df %>% 
    filter(., !is.na(state), !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>% 
    group_by(., state, yearofloss) %>% 
    summarise(., Annual_Summed_Loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))

# Same as above but just for top 10 states
Annual_State_Summed_Claims_Top_10 = 
  Top_10_DF %>% 
  filter(., !is.na(state), !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>% 
  group_by(., state, yearofloss) %>% 
  summarise(., Annual_Summed_Loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))

# All 50 states over time 
GG_Temporal_Claims_All_States = 
Annual_State_Summed_Claims %>% 
  ggplot(., aes(x = yearofloss, y = Annual_Summed_Loss, color = state)) +
  geom_line(show.legend = FALSE) + 
  labs(title = 'Total Money Claimed by State and Year', x = 'Year', y = '$') + 
  scale_y_continuous(labels = scales::comma) 
GG_Temporal_Claims_All_States

# Top 10 states over time
GG_Temporal_Claims_Top_10_States = 
Annual_State_Summed_Claims_Top_10 %>% 
  ggplot(., aes(x = yearofloss, y = Annual_Summed_Loss, color = state)) +
  geom_line() + 
  labs(title = 'Total Money Claimed by State and Year for Top 10', x = 'Year', y = '$') + 
  scale_y_continuous(labels = scales::comma) 
GG_Temporal_Claims_Top_10_States

# Top 10 states over time
GG_Temporal_Claims_Top_10_States_by_Percent_of_Total_Program_Cost = 
Annual_State_Summed_Claims_Top_10 %>% 
  ggplot(., aes(x = yearofloss, y = 100*(Annual_Summed_Loss / sum(Annual_Summed_Loss)), color = state)) +
  geom_line() + 
  labs(title = 'Annual / Total Money Claimed to Date for NFIP Program', x = 'Year', y = 'Percent of $')
GG_Temporal_Claims_Top_10_States_by_Percent_of_Total_Program_Cost




# make the second for loop based on filter year<=i and group_by state ony

Accumulate_DF = data.frame()

for (i in min(raw.df$yearofloss):max(raw.df$yearofloss)) {
    intermediate = raw.df %>% 
      filter(., yearofloss <= i, state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>%
      group_by(., state) %>% 
      summarise(., accumulated_loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim)) %>% 
      mutate(., yearofloss = i)
    
    Accumulate_DF = rbind(Accumulate_DF, intermediate)
}

# Removing a few extra NA's
Accumulate_DF = Accumulate_DF %>% filter(., !is.na(state))

# Creating an Accumlation of claim $ plot for all states
GG_Accumulation_for_States = 
Accumulate_DF %>% 
  ggplot(., aes(x = yearofloss, y = accumulated_loss, color = state)) +
  geom_line(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::comma)
GG_Accumulation_for_States

# Creating an Accumlation of claim $ plot for whole country
GG_Accumulation_for_Nation = 
Accumulate_DF %>% 
  group_by(., yearofloss) %>% 
  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
  ggplot(., aes(x = yearofloss, y = accumulated_loss)) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma)
GG_Accumulation_for_Nation

# Creating list of all state names
state_names = raw.df %>% 
  filter(., state != "", !is.na(state), !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>%
  distinct(state) %>% 
  select(., state)
state_names = state_names[['state']]
state_names

#assembling the accumulated dataframe from 0 to 1 for each state
Accumulate_DF_0_to_1 = data.frame()
for (i in 1:length(state_names)) {
  intermediate = Accumulate_DF %>% 
    filter(., state == state_names[i]) %>% 
    mutate(., standardized_accumulation = accumulated_loss/max(accumulated_loss))
  Accumulate_DF_0_to_1 = rbind(Accumulate_DF_0_to_1, intermediate)
}

# Creating Standardized plot for accumulation for all 50 states to see if line type (log, linear, exponential) is categorical
GG_Standardized_Accumulation = 
Accumulate_DF_0_to_1 %>% 
  ggplot(., aes(x = yearofloss, y = standardized_accumulation, color = state)) +
  geom_line(show.legend = FALSE)
GG_Standardized_Accumulation

# Regular version
GGvis_reg <- gvisGeoChart(Total_State_Summed_Claims, "state", "Total_Summed_Loss",
                      options=list(region="US", displayMode="regions",
                                   resolution="provinces",
                                   width=600, height=400))
plot(GGvis_reg)

# Log version
Log_Total_State_Summed_Claims = Total_State_Summed_Claims %>% 
  mutate(., Log_Total_Summed_loss = log(x = Total_Summed_Loss, base = 10))

GGvis_log <- gvisGeoChart(Log_Total_State_Summed_Claims, "state", "Log_Total_Summed_loss",
                      options=list(region="US", displayMode="regions",
                                   resolution="provinces",
                                   width=600, height=400))
plot(GGvis_log)

# Attempt to take out top 2 states version
Sans_Top_2_Total_State_Summed_Claims = Total_State_Summed_Claims %>% 
  filter(., state != 'TX', state != 'LA')

GGvis_without_LA_TX <- gvisGeoChart(Sans_Top_2_Total_State_Summed_Claims, "state", "Total_Summed_Loss",
                                options=list(region="US", displayMode="regions",
                                             resolution="provinces",
                                             width=600, height=400))
plot(GGvis_without_LA_TX)

list = ls(all=TRUE)


# Save an object to a file
saveRDS(list, file = "base_data.rds")

#asda































# i=1
# Accumulate_DF = data.frame()
# for (i in 1:10) {
#   intermediate = raw.df %>% 
#     filter(., state == Top_Ten_Total_State_Summed_Claims[i]) 
#   for (i in 1990:2000) {
#     intermediate = intermediate %>% 
#       filter(., yearofloss == i) %>%
#       group_by(., state, yearofloss) %>% 
#       summarise(., accumulated_loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))
#   }
#   
#   Accumulate_DF = rbind(Accumulate_DF, intermediate)
# }
# 
# i=1
# Top_10_DF = data.frame()
# for (i in 1:10) {
#   intermediate = raw.df %>% 
#     filter(., state == Top_Ten_Total_State_Summed_Claims[i]) 
#   Top_10_DF = rbind(Top_10_DF, intermediate)
# }
# 
# 
# Accumulate_DF = data.frame()
# i=1990
# for (i in 1990:2000) {
#   if (i == 1990){
#     intermediate = raw.df %>% 
#       filter(., yearofloss == i, !is.na(state), !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>%
#       group_by(., state, yearofloss) %>% 
#       summarise(., accumulated_loss = sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))
#     Accumulate_DF = dplyr::bind_rows(Accumulate_DF, intermediate)
#   } else {
#     intermediate = raw.df %>% 
#       filter(., yearofloss == i, !is.na(state), !is.na(yearofloss), !is.na(amountpaidonbuildingclaim), !is.na(amountpaidoncontentsclaim)) %>%
#       group_by(., state, yearofloss) %>% 
#       summarise(., accumulated_loss = Accumulate_DF$accumulated_loss +  sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim))
#     
#     Accumulate_DF = rbind(Accumulate_DF, intermediate + Accumulate_DF)
#   }
# }






