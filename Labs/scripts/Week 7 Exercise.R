
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
  gt,# customize and style tables
  lubridate
)

# Import data -------------------------------------------------------------

# import COVID Long-Term Care Outbreak Dataset using the 'rio' and 'here' funtions

linelist <- import(here("data","COVID19 Long-Term Care Outbreak Dataset_R.xlsx"))


# View data ---------------------------------------------------------------

# view data in the R console

head(linelist) # view the first 6 rows of the dataset in the R console

head(linelist, n = 20) # view the first 20 rows of the dataset in the R console

skim(linelist) # overview of entire dataframe summarized by class/type 

names(linelist) # view all column names in your dataset

clean_names(linelist) # standardize column names

# Clean data --------------------------------------------------------------

linelist_clean <- linelist %>% 
  
  # standardize column names
  
  clean_names() %>% 
  
  # manually re-name columns
  
  # NEW name        # OLD name
  rename(onset_date       = date_onset,
         unique_id        = caseid) 
  
# create a new column, episode_date, using the minimum date between report_date and onset_date
# use na.rm = TRUE to ignore blank cells in the report_date and onset_date columns when calculating the min value

linelist_clean <- linelist_clean %>% 
  mutate(episode_date = pmin(report_date, onset_date, na.rm = TRUE)) %>% 
  mutate(age = as.integer(difftime(episode_date, birth_date, units = "days") / 365.5))

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
  group_by(cluster_role) %>%   # group cases by cluster role
  summarise(                   # summarize the variables below
    n_cases  = n(),                 # total cases
    mean_age = mean(age, na.rm=T),  # mean (average) age
    max_age  = max(age, na.rm=T),   # max age
    min_age  = min(age, na.rm=T),   # min age 
    n_males  = sum(sex == "Male", na.rm=T),  # summary of male cases
    n_females  = sum(sex == "Female", na.rm=T),  # summary of female cases
    n_hospitalized  = sum(hospitalized == "Yes", na.rm=T),  # summary of hospitalized cases
    n_deceased  = sum(deceased == "Yes", na.rm=T))  # summary of deceased cases

# summary statistics same as above; formatted to add labels and create an output in the 'viewer'

linelist_clean %>% 
  group_by(cluster_role) %>% 
  summarise(
    "Total Cases"  = n(),
    "Mean Age" = round(mean(age, na.rm=T), 0),
    "Max Age"  = max(age, na.rm=T),
    "Min Age"  = min(age, na.rm=T),
    "Number of Males"    = sum(sex == "Male", na.rm=T),
    "Number of Females"  = sum(sex == "Female", na.rm=T),
    "Number of cases hospitalized" = sum(hospitalized == "Yes", na.rm=T),
    "Number of cases deceased" = sum(deceased == "Yes", na.rm=T)) %>% 
  
  
  kable(format = "html", caption = "Summary of Data by Cluster Role") %>% # create table in viewer
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


# final table #1

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

# final table #2

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


########## Week 7 Exercise ##########

### PART 1: Working with dates ###

# check the class of a variable

class(linelist_clean$onset_date) # check the class of onset_date

# Convert onset_date from date time to a date class using the base R function as.Date
linelist_clean <- linelist_clean %>%
  mutate(onset_date = as.Date(onset_date,format = "%Y-%m-%d"))

class(linelist_clean$onset_date)

# convert episode_date to a date class using lubridate 
## first install & load lubridate at the top of your script using pacman ##
class(linelist_clean$episode_date)

linelist_clean <- linelist_clean %>%
  mutate(episode_date = ymd(episode_date))

class(linelist_clean$episode_date)
# convert report_date to a date class using lubridate

class(linelist_clean$report_date)

linelist_clean <- linelist_clean %>%
  mutate(report_date = ymd(report_date))

class(linelist_clean$report_date)


# convert birth_date to a date class using lubridate
class(linelist_clean$birth_date)

linelist_clean <- linelist_clean %>%
  mutate(birth_date = ymd(birth_date))

class(linelist_clean$birth_date)



### PART 2: Filtering data ###

# Create a new data frame (line list) that includes only residents

linelist_residents <- linelist_clean %>%
  filter(cluster_role == "Resident/patient")


# Create a new data frame that includes only staff cases
linelist_staff <- linelist_clean %>%
  filter(cluster_role == "Staff/worker")



# Create a new data frame that includes only hospitalized cases
linelist_hospitalized <- linelist_clean %>%
  filter(hospitalized == "Yes")



# Create a new data frame that includes only hospitalized staff cases

linelist_hospitalized_staff <- linelist_clean %>%
  filter(hospitalized == "Yes" & cluster_role == "Staff/worker")



# Create a new data frame that includes only hospitalized or deceased cases
linelist_hospitalized_or_deceased <- linelist_clean %>%
  filter(hospitalized == "Yes" | deceased == "Yes" )




# create an epidemiological curve #
## NOTE: the following code starts simple then builds ##
### In the real world, you would only ever include STEP 7 in your script ###
#### The purpose of showing the code this way is so learners can see step 
#### by step how each line of code modifies the figure ####

########## STEP 1 ##########

#Basic graph with the data on the X axis being the number of cases by episode date.
#all Black and x axis title is episode_date while y axis just says count

ggplot(data = linelist_clean)+ # use the linelist_clean data frame to create the plot
  geom_bar(                    # create a bar plot
    mapping = aes(             # within the histogram, map the aesthetics
      x = episode_date))       # map the x-axis to episode_date



########## STEP 2 ##########

#From graph above the episode date count is now filled in with colors indicating
#How many cases are staff and how many are resident
#A new element on the right has appeared labled "cluster_role" which indicates which 
#color is staff and which color is patient on the graph
ggplot(data = linelist_clean)+
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = cluster_role)) 



########## STEP 3 ##########

#Just adds a black outline around each bar in the chart.
ggplot(data = linelist_clean)+
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = cluster_role),
    color = "black")

########## STEP 4 ##########

#This adds a big title for the Graph top left = "Number of lab-confirmed COVID-19 cases"
#A smaller title underneath that one = "Staff and resident cases by cluster role"
#X axis title is now "Episode Date"
#Y axis title is now "Number of lab-confirmed cases"
#The title fo the legend is now = "Cluster Role"
ggplot(data = linelist_clean)+
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = cluster_role),
    color = "black") +
  labs(title = "Number of lab-confirmed COVID-19 cases", 
       subtitle = "Staff and resident cases by cluster role",
       x = "Episode Date",  
       y = "Number of lab-confirmed cases") +
       scale_fill_discrete(name = "Cluster Role")




########## STEP 5 ##########

#A date for each day is added at the bottom of the chart but text is horizontal so a lot of overlap and hard to read
ggplot(data = linelist_clean)+
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = cluster_role),
    color = "black") +
  labs(title = "Number of lab-confirmed COVID-19 cases", 
       subtitle = "Staff and resident cases by cluster role",
       x = "Episode Date", 
       y = "Number of lab-confirmed cases") +
  scale_fill_discrete(name = "Cluster Role") + 
  scale_x_date(date_breaks = "1 day")



########## STEP 6 ##########

#Makes the dates display vertically now so they are actually readable
ggplot(data = linelist_clean)+
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = cluster_role),
    color = "black") +
  labs(title = "Number of lab-confirmed COVID-19 cases", 
       subtitle = "Staff and resident cases by cluster role",
       x = "Episode Date", 
       y = "Number of lab-confirmed cases") +
  scale_fill_discrete(name = "Cluster Role") + 
  scale_x_date(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


########## STEP 7 ##########

#This reformat the date to show day, 3 letter abbreviation for months, last 2 digits of year
ggplot(data = linelist_clean)+
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = cluster_role),
    color = "black") +
  labs(title = "Number of lab-confirmed COVID-19 cases", 
       subtitle = "Staff and resident cases by cluster role",
       x = "Episode Date", 
       y = "Number of lab-confirmed cases") +
  scale_fill_discrete(name = "Cluster Role") + 
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d-%b-%y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


########## STEP 8 ##########

#Splits graph into 2
#One showing just patients and one showing just staff and labels which is which
#This also makes it so the counts dont overlap anymore
ggplot(data = linelist_clean)+
  geom_histogram(
    mapping = aes(
      x = episode_date,
      fill = cluster_role),
    color = "black") +
  labs(title = "Number of lab-confirmed COVID-19 cases", 
       subtitle = "Staff and resident cases by cluster role",
       x = "Episode Date", 
       y = "Number of lab-confirmed cases") +
  scale_fill_discrete(name = "Cluster Role") +
  facet_wrap(~cluster_role) +
  scale_x_date(date_breaks = "3 days",
               date_labels = "%d-%b-%y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

########## STEP 9 ##########

#changes color pallet to purple and light purple so the colors to represent patients and staff is different
ggplot(data = linelist_clean)+ 
  geom_bar(                    
    mapping = aes(            
      x = episode_date,        
      fill = cluster_role),   
    color = "black") +         
  labs(title = "Number of lab-confirmed COVID-19 cases",  # create a title for the plot
       subtitle = "Staff and resident cases by cluster role", 
       x = "Episode Date",                       
       y = "Number of lab-confirmed cases") + 
  scale_fill_brewer(palette = "PuRd", name = "Cluster Role") +
  facet_wrap(~cluster_role) +  
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d-%b-%y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#### Additional figures (do not need to complete this section but feel free to run the code) ###

ggplot(linelist_clean, aes(x = sex)) +
  geom_bar(fill = "pink", color = "black") +
  scale_fill_discrete(name = "Cluster Role") + 
  facet_wrap(~cluster_role) +
labs(title = "Number of confirmed COVID-19 cases by cluster role and sex", x = "Sex", y = "Count")

ggplot(linelist_clean, aes(x = sex, fill = cluster_role)) +
  geom_bar(color = "black") +
  scale_fill_discrete(name = "Cluster Role") + 
  facet_wrap(~cluster_role) +
labs(title = "Number of confirmed COVID-19 cases by cluster role and sex", x = "Sex", y = "Count")

ggplot(linelist_clean, aes(x = sex, fill = cluster_role)) +
  geom_bar(color = "black") +
  scale_fill_discrete(name = "Cluster Role") + 
  labs(title = "Number of confirmed COVID-19 cases by cluster role and sex", x = "Sex", y = "Count")

