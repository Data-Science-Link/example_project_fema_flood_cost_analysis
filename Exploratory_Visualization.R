rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd('/Users/michaellink/Desktop/__NYCDSA/_Projects/Shiny/fema_claims_new')
load(file = "./FEMA_Flood_Claims/processed_data.Rdata")

#GGplot specifications
title_text_sz = 18
axis_text_sz = 10
axis_title_sz = 12

# Creating a line plot of claims for the nation for whole dataset
GG_Total_Nation_Summed_Claims = 
  filter.raw.df %>% 
  group_by(., yearofloss) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  ggplot(., aes(x = yearofloss, y = Total_Summed_Loss)) +
  geom_line() +
  labs(title = 'Annual NFIP Claims Across the Nation', x = 'year', y = '$') + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Total_Nation_Summed_Claims

# Creating a descending bar plot of the above data
GG_Summed_Claim_Cost_by_States = 
  filter.raw.df %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  arrange(., desc(Total_Summed_Loss)) %>% 
  ggplot(., aes(x = reorder(state, -Total_Summed_Loss), y = Total_Summed_Loss)) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Total Money Claimed to Date by Each State', x = 'State', y = '$') + 
  theme(axis.text.x = element_text(angle = 270)) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Summed_Claim_Cost_by_States

# Creating a descending bar plot of the total claims paid out by state for top 10 states
GG_Summed_Claim_Cost_by_Top_10_States = 
  Top_10_DF %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  ggplot(., aes(x = reorder(state, -Total_Summed_Loss), y = Total_Summed_Loss)) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Total Money Claimed to Date for Top 10 States', x = 'State', y = '$') + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Summed_Claim_Cost_by_Top_10_States

# All 50 states over time 
GG_Temporal_Claims_All_States = 
  filter.raw.df %>% 
  group_by(., state, yearofloss) %>% 
  summarise(., Annual_Summed_Loss = sum(amountpaidtotal)) %>% 
  ggplot(., aes(x = yearofloss, y = Annual_Summed_Loss, color = state)) +
  geom_line(show.legend = FALSE) + 
  labs(title = 'Total Money Claimed by State and Year', x = 'Year', y = '$') + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Temporal_Claims_All_States

# Top 10 states over time
GG_Temporal_Claims_Top_10_States = 
  Top_10_DF %>% 
  group_by(., state, yearofloss) %>% 
  summarise(., Annual_Summed_Loss = sum(amountpaidtotal)) %>% 
  ggplot(., aes(x = yearofloss, y = Annual_Summed_Loss, color = state)) +
  geom_line() + 
  labs(title = 'Total Money Claimed by State and Year for Top 10', x = 'Year', y = '$') + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Temporal_Claims_Top_10_States

# Top 10 states over time
GG_Temporal_Claims_Top_10_States_by_Percent_of_Total_Program_Cost = 
  Top_10_DF %>% 
  group_by(., state, yearofloss) %>% 
  summarise(., Annual_Summed_Loss = sum(amountpaidtotal)) %>% 
  ggplot(., aes(x = yearofloss, y = 100*(Annual_Summed_Loss / sum(Annual_Summed_Loss)), color = state)) +
  geom_line() + 
  labs(title = 'Annual / Total Money Claimed to Date for NFIP Program', x = 'Year', y = 'Percent of $') +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Temporal_Claims_Top_10_States_by_Percent_of_Total_Program_Cost

# Creating an Accumlation of claim $ plot for all states
GG_Accumulation_for_States = 
  Accumulate_DF %>% 
  ggplot(., aes(x = yearofloss, y = accumulated_loss, color = state)) +
  geom_line(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Accumulation_for_States

# Creating an Accumlation of claim $ plot for whole country
GG_Accumulation_for_Nation = 
  Accumulate_DF %>% 
  group_by(., yearofloss) %>% 
  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
  ggplot(., aes(x = yearofloss, y = accumulated_loss)) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Accumulation_for_Nation #ADD DOTS ON THE LINE FOR WHEN MAJOR STORMS HAPPENED
#PRE AND POST KATRINA COMPARISON

# Creating an Accumlation of claim $ plot for whole country
GG_Accumulation_for_Nation_Standardized = 
  Accumulate_DF %>% 
  group_by(., yearofloss) %>% 
  summarise(., accumulated_loss = sum(accumulated_loss)/69751802599) %>% #total paid throughout program
  ggplot(., aes(x = yearofloss, y = accumulated_loss)) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Accumulation_for_Nation_Standardized

str(Accumulate_DF)

alpha = 2.115946e+09
beta = 7.445744e-02
theta = -4.923348e+09

#### Regression ####
GG_Accumulation_for_Nation_Regression = 
  Accumulate_DF %>% 
  filter(., !is.na(yearofloss), !is.nan(yearofloss), yearofloss != Inf, yearofloss != -Inf, yearofloss != '') %>% 
  group_by(., yearofloss) %>% 
  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
  mutate(., yearofloss = yearofloss - min(yearofloss)) %>% 
  filter(., yearofloss > 0, accumulated_loss > 0) %>% 
  ggplot(.) +
  geom_point(aes(x = yearofloss, y = accumulated_loss)) + 
  geom_line(colour = 'red', linetype = 1, aes(x = yearofloss, y = alpha*exp( as.numeric(beta) * yearofloss) + theta)) +
  geom_smooth(colour = 'blue', size = 0.5, aes(x = yearofloss, y = accumulated_loss), method="lm", formula= (y ~ x), se=FALSE, linetype = 1) + 
  geom_smooth(colour = 'green', size = 0.5, aes(x = yearofloss, y = accumulated_loss), method="lm", formula= (y ~ x + I(x^2) + I(x^3)), se=FALSE, linetype = 1) + 
  scale_color_discrete(name = "Y series", labels = c("Y2", "Y1", "Y3")) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Accumulation_for_Nation_Regression

#### GEOM_AREA ####
# Creating an Accumlation of claim $ plot for whole country with geom_area state instead
GG_Accumulation_for_Nation_Geom_Area = 
  Accumulate_DF %>% 
  group_by(., state, yearofloss) %>% 
  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
  ggplot(., aes(x = yearofloss, y = accumulated_loss, fill = state)) +
  geom_area(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Accumulation_for_Nation_Geom_Area

# Creating an Accumlation of claim $ plot for whole country with geom_area flood zone instead
GG_Accumulation_for_Nation_Geom_Area_Flood_Zone = 
  Accumulate_DF %>% 
  group_by(., floodzone, yearofloss) %>% 
  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
  ggplot(., aes(x = yearofloss, y = accumulated_loss, fill = floodzone)) +
  geom_area(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Accumulation_for_Nation_Geom_Area_Flood_Zone

# Creating Standardized plot for accumulation for all 50 states to see if line type (log, linear, exponential) is categorical
GG_Standardized_Accumulation_State = 
  Accumulate_DF_0_to_1 %>% 
  ggplot(., aes(x = yearofloss, y = standardized_accumulation, color = state)) +
  geom_line(show.legend = FALSE) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz),
        axis.title=element_text(size = axis_title_sz,face="bold"))
GG_Standardized_Accumulation_State

# Regular version
Total_State_Summed_Claims = 
  filter.raw.df %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) 
GGvis_reg <- gvisGeoChart(Total_State_Summed_Claims, "state", "Total_Summed_Loss",
                          options=list(region="US", displayMode="regions",
                                       resolution="provinces",
                                       width=600, height=400))
plot(GGvis_reg)

# Log version
Log_Total_State_Summed_Claims = 
  filter.raw.df %>% 
  group_by(., state) %>% 
  summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
  mutate(., Log_Total_Summed_loss = log(x = Total_Summed_Loss, base = 10))

GGvis_log <- gvisGeoChart(Log_Total_State_Summed_Claims, "state", "Log_Total_Summed_loss",
                          options=list(region="US", displayMode="regions",
                                       resolution="provinces",
                                       width=600, height=400))
plot(GGvis_log)
#ADD SECOND COLUMN WITH VALUES SO THAT CURSOR CAN REFERENCE

# Attempt to take out top 2 states version
Sans_Top_2_Total_State_Summed_Claims = Total_State_Summed_Claims %>% 
  filter(., state != 'TX', state != 'LA')

GGvis_without_LA_TX <- gvisGeoChart(Sans_Top_2_Total_State_Summed_Claims, "state", "Total_Summed_Loss",
                                    options=list(region="US", displayMode="regions",
                                                 resolution="provinces",
                                                 width=600, height=400))
plot(GGvis_without_LA_TX)

library(plotly)

#### MAJOR_STORMS ####

GG_Amount_PD = 
  major_storms %>% 
  ggplot(., aes(x = Year, y = Amount_PD, size = Amount_PD, text = paste0(Event, "<br>", Amount_PD), group = 1)) +
  geom_point() +
  labs(title = 'Paid Losses of Major Storms', x = '', y = '$') + 
  scale_y_continuous(limits = c(0,2e+10), labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz, face = 'bold'),
        axis.title=element_text(size = axis_title_sz))
ggplotly(GG_Amount_PD, tooltip = 'text')



major_storms %>% 
  ggplot(., aes(x = Year)) + geom_histogram()
ggplotly()

GG_Avg_PD_Losses = major_storms %>% 
  ggplot(., aes(x = Year, y = Avg_PD_Losses, text = paste0(Event, "<br>", Avg_PD_Losses), group = 1)) +
  geom_point() +
  geom_smooth(method = loess, se = FALSE, formula = 'y ~ x') +
  labs(title = 'Average Paid Losses of Major Storms', x = '', y = '$') + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
        axis.text=element_text(size = axis_text_sz, face = 'bold'),
        axis.title=element_text(size = axis_title_sz))
ggplotly(GG_Avg_PD_Losses, tooltip = "text")

text = major_storms$Event


