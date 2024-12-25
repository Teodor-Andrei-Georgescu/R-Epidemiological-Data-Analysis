#Done by Teodor Andrei Georgescu (v00979210)

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
  kableExtra
)

#import the Assignment data set into object

a2_data = import(here("data","COVID19_Elementary_School_Outbreak_Line_list.xlsx"))



#clean data, standardize columns, and make age group
cleaned_a2_data <- a2_data %>%
  clean_names() %>%
  rename(case_id = caseid) %>%
  mutate(episode_date = pmin(date_reported,onset_date, na.rm = True)) %>%
  mutate(age = as.integer(difftime(episode_date,birth_date,units = "days")/365.5)) %>%
  mutate(age_group = case_when(
    age <= 19 ~ "0-19",
    age > 19 & age <= 29 ~ "20-29",
    age > 29 & age <= 39 ~ "30-39",
    age > 39 & age <= 49 ~ "40-49",
    age > 49 & age <= 59 ~ "50-59",
    age > 59 & age <= 69 ~ "60-69",
    age > 69 & age <= 79 ~ "70-79",
    age > 79 & age <= 89 ~ "80-89",
    age > 89  ~ "90+",
  ))



#summarize data into grouped list and create output in viewer

cleaned_a2_data %>%
  select(
    age,
    sex,
    ever_hospitalized,
    deceased,
    case_role
  ) %>%
  tbl_summary(
    by = case_role,
    statistic = list(
      all_continuous() ~ "{mean} ({min} - {max})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      age ~ "Age",
      sex ~ "Sex",
      ever_hospitalized ~ "Hospitalized",
      deceased ~ "Deceased"
    )
  )

#starting work on epi curve
class(cleaned_a2_data$onset_date)

cleaned_a2_data <- cleaned_a2_data %>%
  mutate(onset_date = ymd(onset_date)) %>%
  mutate(episode_date = ymd(episode_date)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  mutate(birth_date = ymd(birth_date))

cleaned_a2_data_teachers = cleaned_a2_data %>% filter(case_role == "Teachers")
cleaned_a2_data_students = cleaned_a2_data %>% filter(case_role == "Students")

color_choosen = c("#A8E6CE","#BFD5E2")

ggplot(data = cleaned_a2_data) +
  geom_bar(
    mapping = aes(
      x = episode_date,
      fill = case_role
    ),
    color = "black"
  )+
  labs(title = "Figure 1: Number of Lab-Confirmed Covid-19 cases from October 1st - October 19th 2023",
       subtitle = "Cases by Case role (Student or Teacher)",
       x ="Episode Date",
       y ="Number of lab-confirmed cases"
       )+
  scale_fill_manual(values = color_choosen, name ="") +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d-%b-%y"
               )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )

#learner chosen graph 1 -- cases between males and females
cleaned_a2_data$interval <- cut(cleaned_a2_data$date_reported, breaks = "1 week")
ggplot(data = cleaned_a2_data) +
  geom_bar(
    mapping = aes(
      x = sex,
      fill = case_role
    ),
    color = "black"
  )+
  facet_wrap(~interval, scales = "free_x", nrow =1)+
  labs(title = "Figure 2: Number of Lab-Confirmed Covid-19 cases by sex from October 1st - October 19th 2023",
       x ="Sex",
       y ="Number of lab-confirmed cases"
  )+
  scale_fill_manual(values = color_choosen, name ="Case Role") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
  )


#learner chosen graph 2 -- hospitalization by group
ggplot(data = cleaned_a2_data[ cleaned_a2_data$ever_hospitalized == "Yes", ]) +
  geom_bar(
    mapping = aes(
      x = age_group,
      fill = case_role
    ),
    color = "black"
  )+
  labs(title = "Figure 3: Number of Hospitalized Cases by age group and case role from October 1st-October 19th 2023",
       x ="Age group",
       y ="Number of Hospitalized Cases"
  )+
  scale_fill_manual(values = color_choosen, name ="Case Role") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
  )
