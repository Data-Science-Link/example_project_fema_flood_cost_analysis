# library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

#GGplot specifications
title_text_sz = 18
axis_text_sz = 10
axis_title_sz = 12

alpha = 2.115946e+09
beta = 7.445744e-02
theta = -4.923348e+09

load(file = "processed_data.Rdata")

START_HERE_TEXT_pt1 = 
  "The National Flood Insurance Program (NFIP) was created in 1968 to enforce responsible building practices (i.e. not building your home in the middle of a river) and to alleviate the financial woes of Americans after disastrous flooding events. In short, FEMA mandates that people who live close to rivers and streams buy flood insurance. Insured property owners are reimbursed according to the severity of the flood damage and according to their policy coverage."
START_HERE_TEXT_pt2 = 
  "Since this program was created, there have been 70 billion dollars paid in claims. There are three primary insights gleamed from this dashboard:" 
START_HERE_TEXT_pt2_1 = 
  "1. Residents of the south, in particular Louisiana and Texas, are responsible for the majority of flood insurance claims."
START_HERE_TEXT_pt2_2 = 
  "2. The majority of money paid out for FEMA claims is tied to modern mega-storm catastrophes like Hurricanes Katrina, Harvey, and Superstorm Sandy."
START_HERE_TEXT_pt2_3 = 
"3. The average flood claim expense has increased over the years." 
START_HERE_TEXT_pt3 = 
'This application has been built to effectively convey the spatial and temporal trends of flood insurance claims. This dashboard can help NFIP managers diagnose leaks in the financial sustainability of their program.'
OUR_NATION_TEXT_pt1 = 
  'This tab offers three metrics:'
OUR_NATION_TEXT_pt2 = 
  '1. Accumulated Claims - how much money has been claimed to date'
OUR_NATION_TEXT_pt3 = 
  '2. Annual Claims - which years were the most expensive and why'
OUR_NATION_TEXT_pt4 = 
  '3. Summed Flood Claims - which states have the most water on their hands'
OUR_STATES_TEXT_pt1 = 
  'This tab offers three metrics:'
OUR_STATES_TEXT_pt2 = 
  '1. Accumulated Claims - how much money has been claimed to date'
OUR_STATES_TEXT_pt3 = 
  '2. Annual Claims- which years were the most expensive'
OUR_STATES_TEXT_pt4 = 
  '3. Summed Flood Claims - which cities have the most water on their hands'
OUR_STORY_pt1 = 
  'The National Flood Insurance Program (NFIP) was created in 1968 to enforce responsible building practices and to help citizens get back on their financial feet after floods. As a water resources engineer, I can tell you that our modern predictions of flood frequency are nowhere near perfect, let alone scientist’s predictions from 20-30 years ago. If you mix this imperfect science with increasingly erratic weather patterns you are bound to have a few hiccups (Hurricane Katrina, Harvey, Sandy, etc.).'
OUR_STORY_pt2 = 
  'These hiccups endanger the lives of Americans and endanger the financial sustainability of the NFIP program. The analysis below comes to the aid of NFIP managers in identifying which states that exhibit exponential growth in their flood claims. This information can help key stakeholders decide where to place their limited resources as flood and financial risk continue to skyrocket.'
OUR_STORY_pt3 = 
  'Point 1: Storms are becoming increasingly expensive.'
OUR_STORY_pt4 = 
  'Point 2: We can model accumulated paid claims with regression analysis and consequently predict future losses. Try out the following three options to see which you think is the best fit.'
OUR_STORY_pt5 = 
  'Point 3: Let’s standardize the accumulation plot. A value of zero means no money has been paid out. A value of one means that all 70 billion dollars have been paid out.'
OUR_STORY_pt6 = 
  'Point 4: Let’s do the same thing for all states across the United States. Be warned… this is a messy plot.'
OUR_STORY_pt7 = 
  'Point 5: Lets split the states into three camps based on linear regression and the minimization of standard error. Camp 1 - Linear, Camp 2 - Exponential, Camp 3 - Logarithmic'
OUR_STORY_pt8 = 
  'Point 6: Here is a table for the three camps with their, equation, standard error, and future projections of accumulated claims paid.'
OUR_STORY_pt9 = 
  'Point 7: FEMA NFIP managers should focus on the most costly of these future projections.'
