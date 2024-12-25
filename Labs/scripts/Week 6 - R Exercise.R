#########################################################################

#Instructions: Complete the script below.

# Install packages --------------------------------------------------------

# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

# install (if necessary) from CRAN and load packages to be used

pacman::p_load(
  tidyverse,  # data management and visualization
  rio,        # importing data  
  here,       # relative file pathways 
  skimr,      # summarizing data      
  dplyr,      # data wrangling
  kableExtra, # creating customized tables
  janitor,    # data cleaning 
  gtsummary,  # creating summary tables
  gt          # customize and style tables
)

# Import data -------------------------------------------------------------

# import COVID Long-Term Care Outbreak Dataset using the 'rio' and 'here' funtions

linelist <- import(here("data","COVID19 Long-Term Care Outbreak Dataset_R.xlsx"))                               ########## import data ##########

# View data ---------------------------------------------------------------

# view data in the R console

 ########## write code to view the first 6 rows of the dataset in the R console (head)  ##########
first_6_rows <- head(linelist)
 ########## write code to view the first 20 rows of the dataset in the R console (head)  ##########
first_20_rows <- head(linelist,n=20)
 ########## write code to review the entire data frame summarized by class/type (skim) ##########
#entire_dataframe <- skim(linelist)
 ########## write code to view all column names in your dataset (names)  ##########
all_columns <- names(linelist)
 ########## write code to standardize column names (clean_names)  ##########
cleaned_up_columns <- clean_names(linelist)
# Clean data --------------------------------------------------------------

clean_names(linelist)
linelist_clean <- linelist %>% 
  
  # standardize column names
  
  clean_names() %>%
  
  # see the new column names
  
  #names(linelist_clean) 
  
  # re-name 'date_onset' to 'onset_date' & 'caseid' to 'unique_id
  
   ########## write code to rename date_onset to onset_date ##########
   ########## write code to rename caseid to unique_id ##########
  rename(onset_date = date_onset, unique_id = caseid) 
  
  # see the new column names
  
  #names(linelist_clean)
  
  # create a 'counts' column
  linelist_clean <- linelist_clean %>% 
    mutate(counts = 1)

# create a new column, episode_date, using the minimum date between report_date and onset_date
# use na.rm = TRUE to ignore blank cells in the report_date and onset_date columns when calculating the min value

linelist_clean <- linelist_clean %>% 
  mutate(episode_date = min(report_date, onset_date, na.rm = TRUE)) %>% 
  mutate(age = as.integer(difftime(episode_date, birth_date, units = "days") / 365.25))

# use the 'mutate' and 'case_when' functions to create age groups

linelist_clean <- linelist_clean %>% 
  mutate(age_group = case_when(
    age <= 19 ~ "0-19",
    age > 19 & age <= 29 ~ "20-29",
    age > 29 & age <= 39 ~ "30-39",
    age > 39 & age <= 49 ~ "40-49",
    age > 49 & age <= 59 ~ "50-59",
    age > 59 & age <= 69 ~ "60-69",
    age > 69 & age <= 79 ~ "70-79",
    age > 79 & age <= 89 ~ "80-89",
    age > 89             ~ "90+"
  ))

# summarize statistics in a grouped list

linelist_clean %>% 
  group_by(cluster_role) %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age, na.rm=T),
    max_age  = max(age, na.rm=T),
    min_age  = min(age, na.rm=T),
    n_males  = sum(sex == "Male", na.rm=T),
    n_females = sum(sex == "Female", na.rm=T),
    n_hospitaliztions = sum(hospitalized == "Yes", na.rm=T),
    n_deceased = sum(deceased == "Yes", na.rm=T))

# totals

linelist_clean %>% 
  group_by(cluster_role) %>% 
  summarise(
    "Total Cases"  = n(),
    "Mean Age" = round(mean(age, na.rm=T), 0),
    "Max Age"  = max(age, na.rm=T),
    "Min Age"  = min(age, na.rm=T),
    "Number of Males"    = sum(sex == "Male", na.rm=T),
    "Number of Females"  = sum(sex == "Femmale", na.rm=T),
    "Number of Hospitalized"  = sum(hospitalized == "Yes", na.rm=T),
    "Number of cases deceased" = sum(deceased == "Yes", na.rm=T)) %>%
  
  kable(format = "html", caption = "Summary of Data by Cluster Role") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%

# create formatted table #1

linelist_clean %>% 
  select(age, sex, hospitalized, deceased, cluster_role) %>%         # keep only columns of interest
  tbl_summary(     
    by = cluster_role,                                               # stratify entire table by cluster_role
    statistic = list(all_continuous() ~ "{mean} ({min} - {max})",    # stats and format for continuous columns
                     all_categorical() ~ "{n} / {N} ({p}%)"),        # stats and format for categorical columns
    digits = all_continuous() ~ 1,                                   # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                      # force all categorical levels to display
    label  = list(                                                   # display labels for column names
      age   ~ "Age",                           
      sex   ~ "Sex",
      hospitalized     ~ "Hospitalized",
      deceased  ~ "Deceased")
  )

# create formatted table #2

linelist_clean %>% 
  select(age, sex, hospitalized, deceased, cluster_role) %>%         # keep only columns of interest
  tbl_summary(     
    by = cluster_role,                                               # stratify entire table by cluster_role
    statistic = list(all_continuous() ~ "{mean} ({min} - {max})",    # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),        # stats and format for categorical columns
    digits = all_continuous() ~ 1,                                   # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                      # force all categorical levels to display
    label  = list(                                                   # display labels for column names
      age   ~ "Age",                           
      sex   ~ "Sex",
      hospitalized     ~ "Hospitalized",
      deceased  ~ "Deceased")
  )

