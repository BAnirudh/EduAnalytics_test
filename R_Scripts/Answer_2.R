##################### EduAnalytics Empirical-Test: Answer 2 #############################
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
#install.packages("officer")
#install.packages("flextable")
library(officer)
library(flextable)
#install.packages("psych")
library(psych)

## **************** Loading the Required Datasets ******************** ##
#***(a)***#
# Note: Remember to set your working directory!
ZWE_under5_int=read.csv("Zimbabwe_children_under5_interview.csv") 

#Checking NA values
na_values <- ZWE_under5_int %>%
  summarise_all(~sum(is.na(.))) 
# Other than 26 values in birthdays, there are no missing values in the dataset!

##########################################################
#***(b)***#
# Recoding the variables EC6 to EC15 such that '1' stays as it is, '2' is '0' and '8' is '0'
ZWE_under5_int <- ZWE_under5_int %>%
  mutate(across(EC6:EC15, ~ case_when(
    . == 1 ~ 1,
    . %in% c(2, 8) ~ 0,
    TRUE ~ .  # This retains the original values for other cases (we don't need it in this case)
  )))

##########################################################
#***(c)***#
# Here, when any datapoint in EC6-EC15 take the value '1', that is correct and '0' is incorrect
summary_pct_correct <- ZWE_under5_int %>% # Summary stats by child age (in years)
  group_by(child_age_years) %>% # Grouping as required
  summarise(across(EC6:EC15, ~ mean(.x, na.rm = TRUE) * 100, 
                   .names = "percent_{col}")) %>%
  ungroup()

##########################################################
#***(d)***#
# Calculating an index buy taking the arithmetic average of all the 10 items
ZWE_under5_int <- ZWE_under5_int %>%
  rowwise() %>%
  mutate(index = mean(c_across(EC6:EC15), na.rm = TRUE)) %>%
  ungroup()

##########################################################
#***(e)***# 
# Calculating the Cronbach's Alpha (it gives us how closely related a set of items are as a group)
EC_items <- ZWE_under5_int %>% select(EC6:EC15)
Calpha_output <- psych::alpha(EC_items, na.rm = TRUE)
cronbach_alpha <- Calpha_output$total$raw_alpha
num_observations <- Calpha_output$n.obs

CA_results_table <- tibble(
  Metric = c("Cronbach's Alpha", "Number of Observations"),
  Value = c(cronbach_alpha, num_observations)) 
# CA = 0.704 suggesting that the internal consistance of the data is 'Acceptable'

doc <- read_docx() # Create a Word document (to save all plots and the regression table)

CA_table_flextable <- flextable(CA_results_table) # Converting into flextable
doc <- doc %>% # Adding CA table the Word doc
  body_add_flextable(CA_table_flextable) 

##########################################################
#***(f)***#
# We first calculate age in months
ZWE_under5_int <- ZWE_under5_int %>%
  mutate(interview_date = ymd(interview_date), # Changing format for later analysis
         child_birthday = ymd(child_birthday)) %>%
  mutate(age_months = interval(child_birthday, interview_date) %/% months(1))

# Calculating the conditional mean of the index by age in months (for plotting)
age_index <- ZWE_under5_int %>%
  group_by(age_months) %>%
  summarise(mean_index = round(mean(index, na.rm = TRUE), 3)) %>%
  ungroup() %>%
  # Removing rows with NA values (one row has missing value in months)
  filter(!is.na(age_months) & !is.na(mean_index))  

# Generating the required plot
Index_age_plot <- ggplot(age_index, aes(x = age_months, y = mean_index)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_text(aes(label = round(mean_index, 2)), vjust = -0.5, size = 3) +  
  labs(title = "Conditional Mean Index by Children's Age",
       x = "Age (in months)",
       y = "Mean Index") +
  theme_minimal() 

doc <- doc %>% # Adding the above plot to the Word document
  body_add_gg(value = Index_age_plot, style = "centered")

##########################################################
#***(g)***#
## We use a loop by utilizing a similar methodology for construction as above

# First, defining the items for each sub-index
sub_indices <- list(
  literacy_math = c("EC6", "EC7", "EC8"),
  physical = c("EC9", "EC10"),
  learning = c("EC11", "EC12"),
  socio_emotional = c("EC13", "EC14", "EC15")
)

# Calculating the sub-indices, grouping by months, calculating mean values and generating the required plots
for (index_name in names(sub_indices)) {
  ZWE_under5_int <- ZWE_under5_int %>%
    rowwise() %>%
    mutate(!!index_name := 
             mean(c_across(all_of(sub_indices[[index_name]])), na.rm = TRUE)) %>%
    ungroup()
}

#Defining the folder where all graphs will be saved
save_folder_main <- "/Users/anirudhbharadwaj/Desktop/IP2022/EduAnalytics_Test/Graphs_Tables/Response_2"

for (index_name in names(sub_indices)) {
  age_index_sub <- ZWE_under5_int %>%
    group_by(age_months) %>%
    summarise(mean_index = round(mean(!!sym(index_name), na.rm = TRUE), 3)) %>%
    ungroup() %>%
    filter(!is.na(age_months) & !is.na(mean_index))
  
  sub_plot <- ggplot(age_index_sub, aes(x = age_months, y = mean_index)) +
    geom_text(aes(label = round(mean_index, 2)), vjust = -0.75, size = 3.5)  +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(title = paste(" '", index_name, "' index by childrens age"),
         x = "Age (in months)",
         y = "Conditional Mean Index") +
    theme_minimal()
  
  print(sub_plot) # Printing the plot
  
  # Defining the file path for each plot
  save_path <- file.path(save_folder_main, paste0( index_name, "_age.png"))
  
  # Save the plot
  ggsave(filename = save_path, plot = sub_plot, width = 5, height = 3, dpi = 250, bg = "white")
  
  doc <- doc %>% # Add all the plots to the Word document
    body_add_gg(value = sub_plot, style = "centered") 
  
}

##########################################################
#***(h)***#
# We once again use a loop for getting our required regression results
sub_indices_reg <- list(
  overall = c("EC6", "EC7", "EC8", "EC9", "EC10", "EC11", "EC12", "EC13", "EC14", "EC15"),
  literacy_math = c("EC6", "EC7", "EC8"),
  physical = c("EC9", "EC10"),
  learning = c("EC11", "EC12"),
  socio_emotional = c("EC13", "EC14", "EC15")
)

for (index_name in names(sub_indices_reg)) { # This loop generates the required sub-indices
  ZWE_under5_int <- ZWE_under5_int %>%
    rowwise() %>%
    mutate(!!index_name := 
             mean(c_across(all_of(sub_indices_reg[[index_name]])), 
                  na.rm = TRUE)) %>%
    ungroup()
}

# We run these regressions through the whole dataset rather than on individual age values
reg_list <- list()
for (index_name in names(sub_indices_reg)) {
  formula <- as.formula(paste(index_name, "~ age_months"))
  model <- lm(formula, data = ZWE_under5_int)
  tidy_model <- tidy(model)
  glance_model <- glance(model)
  
  reg_list[[index_name]] <- tibble(
    Index = index_name,
    Coefficient = format(tidy_model$estimate[2], scientific = FALSE),
    Std_Error = tidy_model$std.error[2],
    p_value = tidy_model$p.value[2],
    R_Squared = glance_model$r.squared,
    N = glance_model$nobs,
    Overall_Index = mean(ZWE_under5_int[[index_name]], na.rm = TRUE)
  )
}

results_table <- bind_rows(reg_list) # Combining the results into a single table
print(results_table) 

# Convert the results table to a flextable and adding to the Word document
results_flextable <- flextable(results_table)
doc <- doc %>%
  body_add_flextable(results_flextable) 

# Saving the Word document in the local folder
print(doc, target = "/Users/anirudhbharadwaj/Desktop/IP2022/EduAnalytics_Test/Graphs_Tables/Response_2/Graphs_Tables_R2.docx")

######################## END OF SCRIPT ########################################