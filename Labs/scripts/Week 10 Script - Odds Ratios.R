# Ensures the package "pacman" is installed

if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  tidyverse,  # data management and visualization
  rio,        # importing data  
  here,       # relative file pathways 
  gtsummary,
  dplyr, 
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
  boot, 
  Hmisc, 
  ggfittext,
  ggplot2,
  gridExtra,
  apyramid,
  patchwork,
  srvyr,
  demography,
  skimr,
  logbin,
  survey,
  kableExtra,
  ordinal
)


# Read the CCHS data file (replace 'BC CCHS data.csv' with your actual file name)

linelist <- import(here("data","BC CCHS data.csv")) %>% 
  mutate(WTS_M = as.numeric(WTS_M))

# filter for BC

cchs_data <- linelist %>% 
  filter(GEO_PRV == 59)  # filter for BC

# select desired variables

stress_anxiety <- cchs_data %>%
  select(WTS_M, # survey weights
         DHH_SEX, # sex
         DHHGAGE, # age
         GEN_020, # perceived life stress
         CCC_200) %>% # has anxiety disorder 
  filter(
    GEN_020 %in% c(1, 2, 3, 4, 5), # keep "Excellent", "Very good", "Good", "Fair", & "Poor"
    CCC_200 %in% c(1, 2), # keep "Yes" & "No"
  )

# Create new age categories 

stress_anxiety <- stress_anxiety %>%
  mutate(age_group = case_when(
    DHHGAGE %in% c(1, 2, 3) ~ "12-19",
    DHHGAGE %in% c(4, 5, 6, 7) ~ "20-39",
    DHHGAGE %in% c(8, 9, 10, 11) ~ "40-59",
    DHHGAGE %in% c(12, 13, 14, 15) ~ "60-79",
    TRUE ~ "80+"
  )) %>% 
  mutate(age_group = factor(age_group)) 

# rename variables

  stress_anxiety <- stress_anxiety %>% 
    mutate(sex = DHH_SEX, # rename DHH_SEX to sex
           anxiety = CCC_200, # rename CCC_200 to anxiety
           perceived_stress = GEN_020) # rename GEN_020 to perceived_stress

# recode sex variable 
  
  stress_anxiety$sex <- factor(stress_anxiety$sex,
                                   levels = c(1,2),
                                   labels = c("Male", "Female"))
  
 # recode perceived life stress variable
  
  stress_anxiety$perceived_stress <- factor(stress_anxiety$perceived_stress, 
                                   levels = c(1, 2, 3, 4, 5), 
                                   labels = c("Not at all stressful", "Not very stressful", 
                                              "A bit stressful","Quite a bit stressful", "Extremely stressful"))
  
# Recode anxiety variable from 1 and 2 to 1 and 0
  
  stress_anxiety$anxiety <- ifelse(stress_anxiety$anxiety == 2, 0, 1)  
  
# Convert anxiety variable to a factor
  
  stress_anxiety$anxiety <- factor(stress_anxiety$anxiety,
                                   levels = c(0,1),
                                   labels = c("No", "Yes"))
  
# Create a survey design

survey_design <- stress_anxiety %>% 
  as_survey_design(ids = 1, # no cluster ids
                   weights = WTS_M) # weight variable

# Unadjusted ORs

log_model_stress_anxiety <- svyglm(anxiety ~ perceived_stress, 
                                 data = stress_anxiety, 
                                 family = binomial(link="logit"), 
                                 design = survey_design)

# create table summary of model

table <- log_model_stress_anxiety  %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_labels() %>% 
  bold_p(t = .1) 
 
# display the table in the 'Viewer' pane

table

# adjust for age and sex

# adjusted odds ratio for age and sex

log_model_stress_anxiety <- svyglm(anxiety ~ perceived_stress + sex + age_group, 
                                   data = stress_anxiety, 
                                   family = binomial(link="logit"), 
                                   design = survey_design)

# create table summary of model 

table <- log_model_stress_anxiety  %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_labels() %>% 
  bold_p(t = .1) 

table