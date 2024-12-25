# Script written by: Teodor  Andrei Georgescu
# Date:
# Purpose: 
    #To clean and analyze CCHS data. 
    #This code can be modified for all variables in the 2015-16 CCHS dataset.


# Load packages -----------------------------------------------------------

# Ensures the package "pacman" is installed

if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  tidyverse,  # data management and visualization
  rio,        # importing data  
  here,        # relative file pathways 
  gtsummary,
  dplyr, #for data manipulation
  broom,
  knitr,
  survey,
  janitor,
  epitools,
  questionr,
  stargazer,
  officer,
  flextable,
  htmltools,
  rmarkdown,
  devtools,
  psych,
  sjPlot,
  sjmisc,
  sjlabelled,
  survival,
  boot, #for bootstrap
  Hmisc, #for data cleaning
  ggfittext,
  ggplot2,
  gridExtra,
  apyramid,
  patchwork,
  srvyr,
  demography,
  skimr,
  survey,
  kableExtra,
  gt
)

# Import data -------------------------------------------------------------

# Read the BC CCHS data file (replace 'BC CCHS data.csv' with your actual file name)

linelist <- import(here("data","Canada CCHS data.csv")) %>% 
  mutate(WTS_M = as.numeric(WTS_M))

# filter for BC

cchs_data <- linelist %>% 
  filter(GEO_PRV %in% c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61, 62)) 

# Select desired variables analysis (update with your selected variables of choice) ---------------------------------------

mental_health <- cchs_data %>%
  select(WTS_M, # survey weights
         GEN_030, #Sense of belonging to local community
         DHHGAGE, # age
         SWL_035, #Satisfaction relationship and friends
         SWL_030, #Relationship with family members
         SDC_035, #Sexual orientation
         CCC_195) %>% #Has mood disorder
  filter(
    CCC_195 %in% c(1, 2), # keep "Yes", "No", 
    SDC_035 %in% c(2, 3), # keep "Homosexual, that is lesbian or gay" & "bisexual"
    DHHGAGE %in% c(2, 3, 4),# keep 15-17,18-19, and 20-24
    SWL_030 %in% c(1, 2, 3, 4, 5), # Very satisfied, Satisfied, Neither satisfied nor dissatisfied, Dissatisfied, Very dissatisfied
    SWL_035 %in% c(1, 2, 3 ,4  ,5), # Very satisfied, Satisfied, Neither satisfied nor dissatisfied, Dissatisfied, Very dissatisfied
    GEN_030 %in% c(1, 2, 3, 4),#Very Strong, Somewhat strong,Somewhat weak, Very weak,  
  )



# Data Cleaning & Preparation for Analysis (modify as needed for your selected variables) -----------------------------------------------------------

# Create new age categories 

mental_health <- mental_health %>%
  mutate(age_group = case_when(
    DHHGAGE %in% c(2) ~ "15-17",
    DHHGAGE %in% c(3) ~ "18-19",
    DHHGAGE %in% c(4) ~ "20-24"
  )) %>% 
  mutate(age_group = factor(age_group)) 

# rename variables

mental_health <- mental_health %>% 
  mutate(sense_of_community_beonging = GEN_030, 
         friends_relationship_satisfaction = SWL_035, 
         family_relationship_satisfaction = SWL_030,
         sexual_orientation = SDC_035,
         mood_disorder = CCC_195)
# recode sex variable 

mental_health$sense_of_community_beonging <- factor(mental_health$sense_of_community_beonging,
                             levels = c(1, 2, 3, 4),
                             labels = c("Very Strong", "Somewhat strong","Somewhat weak", "Very weak"))

# recode perceived life stress variable (independent variable)

mental_health$friends_relationship_satisfaction <- factor(mental_health$friends_relationship_satisfaction, 
                                          levels = c(1, 2, 3 ,4  ,5), 
                                          labels = c("Very satisfied", "Satisfied", "Neither satisfied nor dissatisfied", "Dissatisfied", "Very dissatisfied"))

mental_health$family_relationship_satisfaction <- factor(mental_health$family_relationship_satisfaction, 
                                                          levels = c(1, 2, 3 ,4  ,5), 
                                                          labels = c("Very satisfied", "Satisfied", "Neither satisfied nor dissatisfied", "Dissatisfied", "Very dissatisfied"))
                                                        
mental_health$sexual_orientation <- factor(mental_health$sexual_orientation,
                             levels = c(2, 3),
                             labels = c("Homosexual, that is lesbian or gay", "bisexual"))

# Recode anxiety variable from 1 and 2 to 0 and 1 (dependent variable)
# NOTE: if i left the code as is, it would work, but i would be running my regression looking at the ABSENCE of an anxiety disorder
# currently 1 is yes and 2 is no (see data dictionary); the code below is changing 2 to 0 and 'everything else' to 1
# this is because i want to detect the presence of an anxiety disorder
# logistic regression assumes '1' means the presence and '0' means the absence

mental_health$mood_disorder <- ifelse(mental_health$mood_disorder == 2, 0, 1)  

# Convert anxiety variable to a factor

mental_health$mood_disorder <- factor(mental_health$mood_disorder,
                                      levels = c(0, 1),
                                      labels = c("No", "Yes"))

# Create survey design ----------------------------------------------------

survey_design <- mental_health %>% # change 'stress_anxiety' to the name of the dataframe you are using
  as_survey_design(ids = 1, # no cluster ids (do not change)
                   weights = WTS_M) # weight variable (do not change)


# Unadjusted Odds Ratio ---------------------------------------------------

# Unadjusted ORs

log_model_mental_health <- svyglm(mood_disorder ~ family_relationship_satisfaction, # dependent variable ~ independent variable
                                   data = mental_health, # data frame
                                   family = binomial(link="logit"), # logistic regression
                                   design = survey_design) # using 'survey_design' we created

# create table summary of model

unadjusted_table <- log_model_mental_health  %>% # replace 'log_model_stress_anxiety' with the name of your object in the previous line of code
  tbl_regression(exponentiate = TRUE) %>% 
  bold_labels() %>%
  bold_p(t = .1) 


 
# display the table in the 'Viewer' pane

unadjusted_table

# Export Unadjusted Odds Ratio Table --------------------------------------

# Extract table data

table_data <- as.data.frame(unadjusted_table)

# Create a flextable

flex_table <- flextable(table_data)

# Save the table to a Word document in the 'outputs' folder using the 'here' function

doc <- read_docx() %>%
  body_add_flextable(value = flex_table) %>%
  print(target = here("outputs", "mental_health_unadjusted_OR.docx"))

# Convert tbl_regression to gt table and save as HTML

gt_table <- as_gt(unadjusted_table)
file_path <- here("outputs", "mental_health_unadjusted_OR.html")
gtsave(gt_table, file = file_path)

# Adjusted Odds Ratio -----------------------------------------------------

# adjusted odds ratio for age and sex

log_model_mental_health <- svyglm(mood_disorder ~ family_relationship_satisfaction + age_group, 
                                   data = mental_health, 
                                   family = binomial(link="logit"), 
                                   design = survey_design)

# create table summary of model 

adjusted_table <- log_model_mental_health  %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_labels() %>% 
  bold_p(t = .1) 

# display the table in the 'Viewer' pane

adjusted_table

community_mental_health <- mental_health %>%
  filter(mood_disorder == "No",
         family_relationship_satisfaction %in% c("Very satisfied", "Satisfied", "Neither satisfied nor dissatisfied", "Dissatisfied", "Very dissatisfied"),
         sense_of_community_beonging %in% c("Very Strong", "Somewhat strong","Somewhat weak", "Very weak"))

community_summary <- community_mental_health %>%
  group_by(family_relationship_satisfaction, sense_of_community_beonging) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count/sum(count)  * 100)

community_table <- flextable(community_summary)%>%
  merge_v( j ="family_relationship_satisfaction") %>%
  set_table_properties(layout = "autofit") %>%
  autofit()

communiyt_doc <- read_docx() %>%
  body_add_flextable(value = community_table) %>%
  print(target = here("outputs", "mental_health_community.docx"))

community_table


family_mental_health <- mental_health %>%
  filter(mood_disorder == "No",
         family_relationship_satisfaction %in% c("Very satisfied", "Satisfied", "Neither satisfied nor dissatisfied", "Dissatisfied", "Very dissatisfied"),
         friends_relationship_satisfaction %in%  c("Very satisfied", "Satisfied", "Neither satisfied nor dissatisfied", "Dissatisfied", "Very dissatisfied"))

family_summary <- family_mental_health %>%
  group_by(family_relationship_satisfaction, friends_relationship_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)  * 100)

family_table <- flextable(family_summary)%>%
  merge_v(j = "family_relationship_satisfaction") %>%
  set_table_properties(layout = "autofit") %>%
  autofit()

family_table



# Export Adjusted Odds Ratio Table ----------------------------------------

# Extract table data

table_data <- as.data.frame(adjusted_table)

# Create a flextable

flex_table <- flextable(table_data)

# Save the table to a Word document in the 'outputs' folder using the 'here' function

doc <- read_docx() %>%
  body_add_flextable(value = flex_table) %>%
  print(target = here("outputs", "mental_health_adjusted_OR.docx"))

# Convert tbl_regression to gt table and save as HTML

gt_table <- as_gt(adjusted_table)
file_path <- here("outputs", "mental_health_adjusted_OR.html")
gtsave(gt_table, file = file_path)

# Weighted Prevalence Estimate Tables --------------------------------------------------------------------

# List of variables to analyze (adjust this based on your dataset)

variables_to_analyze <- c("mood_disorder", "age_group", "family_relationship_satisfaction","sexual_orientation", "sense_of_community_beonging", "friends_relationship_satisfaction" ) # update this with your variables

# Create an empty list to store tables for each variable

result_list <- list()

# Loop through each variable

for (variable in variables_to_analyze) {
  
  # Calculate weighted prevalence using survey package functions
  
  weighted_prevalence_est <- svymean(as.formula(paste0("~", variable)), design = survey_design)
  
  # Calculate the 95% confidence interval for the weighted prevalence estimate
  
  weighted_prevalence_ci <- confint(weighted_prevalence_est)
  
  # Extract the weighted prevalence estimate and convert to percentage with rounding
  
  weighted_prevalence <- round(as.numeric(weighted_prevalence_est) * 100, 1)
  
  # Extract lower and upper bounds of the 95% confidence interval, convert to percentage with rounding
  
  lower_ci <- round(as.numeric(weighted_prevalence_ci[, 1]) * 100, 1)
  upper_ci <- round(as.numeric(weighted_prevalence_ci[, 2]) * 100, 1)
  
  # Calculate counts and proportions
  
  variable_counts <- as.data.frame(table(mental_health[[variable]])) ## update 'stress_anxiety' to correspond to your dataframe ##
  variable_counts$Proportion <- round((variable_counts$Freq / sum(variable_counts$Freq)) * 100, 1)
  
  # Calculate counts with survey weights for the variable
  
  variable_counts_weighted <- as.data.frame(svytable(as.formula(paste0("~", variable)), design = survey_design))
  variable_counts_weighted$Freq <- round(variable_counts_weighted$Freq)
  variable_counts_weighted$Weighted_Proportion <- round((variable_counts_weighted$Freq / sum(variable_counts_weighted$Freq)) * 100, 1)
  
  # Convert factor levels to character
  
  variable_labels <- as.character(variable_counts$Var1)
  
  # Combine the data and create table output
  
  result <- cbind(
    "Variable" = variable_labels,
    "Survey sample, n (%)" = paste0(variable_counts$Freq, " (", variable_counts$Proportion, ")"),
    "Weighted Population, n (%)" = paste0(variable_counts_weighted$Freq, " (", variable_counts_weighted$Weighted_Proportion, ")"),
    "95% CI (Weighted Proportion)" = paste0(lower_ci, " - ", upper_ci)
  )
  
  # Add the result to the result list
  
  result_list[[variable]] <- result
}


# Output each table

for (variable in variables_to_analyze) {
  print(kable(result_list[[variable]], format = "html", align = "c", col.names = c(variable, 
                                                                                   "Survey sample, n (%)", 
                                                                                   "Weighted Population, n (%)", 
                                                                                   "95% CI (Weighted Proportion)")) %>%
          kable_styling(full_width = FALSE))
}


# Loop through each variable and export tables as Word and HTML

for (variable in variables_to_analyze) {

  current_table <- result_list[[variable]]

  if (!is.data.frame(current_table)) {

    if (is.matrix(current_table)) {
      current_table <- as.data.frame(current_table)
    } else {
      next
    }
  }
  
  # Creating a flextable
  
  ft <- flextable(current_table)
  
  # Save as Word document
  
  ft_doc <- read_docx() %>%
    body_add_flextable(value = ft) %>%
    print(target = here("outputs", paste0(variable, "_prevalence_estimate_table.docx")))
  
}
