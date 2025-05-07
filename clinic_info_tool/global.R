# THE GLOBAL.R HELPS YOU TO SEPARATE THINGS OUT. THE WHOLE APP NEEDS ACCESS TO PACKAGES, USE WRANGLED DATA, AND 
# ANYTHING THAT IS RUN IN THE GLOBAL SPACE CAN BE CALLED AND USED IN THE UI AND THE SERVER, SO
# THEY DON'T HAVE TO BE SEPARATELY WRITTEN THERE EACH TIME YOU WANT TO USE THEM.

# LOAD LIBRARIES ---- 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(leaflet)
library(shinycssloaders)
library(fresh)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)


#create custom ggplot theme
myCustomTheme <- function() {
  theme_light() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      legend.position = "bottom",
      panel.border = element_rect(linewidth = 0.7)
    )
}


# DATA WRANGLING ----
all_outages_2023 <- read.csv('data/outage_summaries_2023.csv')

# Create static custom bins for outage durations (for 2023)
all_outages_2023$custom_bins <- cut(all_outages_2023$total_duration_minutes,
                                    breaks = c(0, 15, 30, 45, 60, 90, 120, 240, 480, 720, 1440, 2880),
                                    right = FALSE)

outages_by_county_master <- read.csv('data/outages_by_county_master.csv')

clinic_data <- read.csv('data/finalized_dashboard_data.csv', stringsAsFactors = FALSE)

# Ensure all required columns are characters 
clinic_data <- clinic_data %>%
  mutate(
    Site_Address = as.character(Site_Address),
    Site_City = as.character(Site_City),
    Site_State_Abbreviation = as.character(Site_State_Abbreviation),
    Site_Postal_Code = as.character(Site_Postal_Code)
  )

# Create complete_address column in a properly formatted way
clinic_data <- clinic_data %>%
  mutate(
    complete_address = paste(Site_Address, Site_City, Site_State_Abbreviation, Site_Postal_Code, sep = ", ")
  )


####################
## TEMESGEN'S CODE # GUIDE/FAQ WIDGET
####################
# FAQ Data with Updated Explanations
faq_data <- list(
  
  # Solar Potential Guide ----
  "Solar Potential" = list(
    question = "How is solar potential determined?",
    answer = HTML("In order to give you a precise assessment of your solar potential, we rely on Google Solar API.<br><br>
                    Google Solar API uses Google Earth images to assess building rooftops and record solar potential of buildings throughout the United States.<br><br>
                    It evaluates factors such as:<br>
                    - Where the sun moves in the sky.<br>
                    - If there are nearby trees or buildings that block sunlight.<br><br>
                    <b>Calculation:</b> Roof Area × Panel Efficiency = Estimated Solar Output"),
    why_it_matters = list(
      "More available space means a greater opportunity to offset energy costs with solar power.",
      "Limited roof space may require high-efficiency panels or alternative placements."
    )
  ), # END Solar Potential Guide
  
  
  # Battery Storage Guide ----
  "Battery Storage" = list(
    question = "How is battery storage capacity calculated?",
    answer = HTML("Battery energy storage is sized based on clinic energy needs and outage risk, using cost per kWh estimates from NREL.<br><br>
                    <b>Formula:</b> Battery Size = (Average Daily Consumption × Outage Duration) ÷ Battery Efficiency"),
    why_it_matters = list(
      "Battery storage ensures power availability during outages.",
      "Correct sizing helps reduce overall energy costs."
    )
  ), # END Battery Storage Guide
  
  
  # Power Outage Risk Guide ----
  "Power Outage Risk" = list(
    question = "How is power outage risk assessed?",
    answer = HTML("Power outage risk is measured using historical data on electricity interruptions from 2014 to 2022. Two key indices are used:<br><br>
                    <b>SAIDI (System Average Interruption Duration Index):</b> Measures the average length of time each customer experiences a power outage. It is calculated by summing the total duration of outages affecting customers and dividing it by the total number of customers.<br><br>
                    <b>SAIFI (System Average Interruption Frequency Index):</b> Measures how often power outages occur. It is calculated by summing the total number of customers affected by outages and dividing it by the total number of customers."),
    why_it_matters = list(
      "Understanding outage risk helps clinics plan for backup power solutions, such as battery storage or generators.",
      "Reduces disruptions to critical medical operations, ensuring essential equipment remains functional during outages."
    )
  ), # END Power Outage Risk Guide
  
  
  # Natural Disaster Risk Guide ----
  "Natural Disaster Risk" = list(
    question = "How is natural disaster risk evaluated?",
    answer = HTML("FEMA’s National Risk Index and Climate Vulnerability Index GitHub data provide vulnerability scores for various extreme events.<br><br>
                    Risk Index includes Expected Annual Loss, Social Vulnerability, and Community Resilience scores."),
    why_it_matters = list(
      "Assessing disaster risk helps clinics build resilience strategies.",
      "Ensures critical medical services remain operational."
    )
  ), # END Natural Disaster Risk Guide
  
  
  # Financial Assessment Guide ----
  "Financial Assessment" = list(
    question = "How is the financial viability of solar determined?",
    answer = HTML("Return on Investment is calculated based on installation costs, energy savings, and incentives.<br><br>
                    <b>Formula:</b> Payback Period = Net Installation Cost ÷ Annual Energy Savings"),
    why_it_matters = list(
      "A shorter payback period means faster cost recovery.",
      "Financial modeling supports investment decisions in clean energy."
    )
  ) # END Financial Assessment Guide
) # END FAQ/Guide Data

########################
# END TEMESGEN'S CODE ##
########################
