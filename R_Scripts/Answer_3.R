##################### EduAnalytics Empirical-Test: Answer 3 #############################
#Note: Please use the 'install.packages("package_name")' in case a library doesn't run
library(writexl)
library(haven)
library(tidyverse)
library(tidyr)
library(readxl)
library(writexl)
library(lubridate)
library(countrycode)
library(tidyquant)
library(ggplot2)
library(xtable)
library(lubridate)
library(rdbnomics)
library(patchwork)
library(RColorBrewer)
library(stringr)
library(jtools)
library(huxtable)
library(ggpubr)
library(readr)
library(showtext)
library(extrafont)
library(tibble)
library(openxlsx)
library(dplyr)
#install.packages(c("mvtnorm", "expm", "msm", "ltm"))
#library(ltm)
#install.packages("psych")
library(psych)

#install.packages("wbstats")
library(wbstats) # For this exercise we need to load the "wbstats" library
install.packages("officer")
library(officer)

## **************** Pulling the Required Datasets ******************** ##
indicator <- "SH.STA.STNT.ME.ZS" # First defining the required indicator and aggregates
# WBG defination: Prevalence of stunting, height for age (% of children under 5)

countries <- c("ZWE", "LMY", "SSA") # LMY for Lower Middle Income, SSA for Sub-Saharan Africa

# Getting the data for the specified indicator and country/regions
stunting_data <- wb_data(indicator = indicator, country = countries) %>%
  rename(stunting_value_pct = !!indicator) %>%
  # Checking the dataset reveals that stunting data only present beyond 2000
  filter(date >= 2000) 


## **************** Analysis ******************** ##
table_recent <- stunting_data %>%
  filter(!is.na(stunting_value_pct)) %>%
  group_by(iso3c) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(
    Region = country,
    Year = date,
    Stunting_Prevalence = stunting_value_pct
  ) 

# Creating a Word document and adding the table for exporting
doc <- read_docx() %>%
  body_add_table(value = table_recent, style = "table_template") %>%
  body_add_par("Table 1: Prevalence of Stunting in Selected Regions", style = "caption")

# Save the document
print(doc, target = "Stunting_Prevalence_Report.docx")


# Plotting the stunting values across years
ggplot(stunting_data %>% filter(!is.na(stunting_value_pct)), 
       aes(x = date, y = stunting_value_pct, color = country, label = round(stunting_value_pct, 1))) +
  geom_line() +
  geom_point(show.legend = TRUE) +
  geom_text(vjust = -0.5, show.legend = FALSE , size = 2.5) + 
  labs(title = "Prevalence of Stunting Over Time",
       x = "Year",
       y = "Stunting Value (%)",
       color = NULL) +  
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

######################## END OF SCRIPT ########################################