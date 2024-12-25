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
  demography
)


# Read the CCHS data file (replace 'cchs_data.csv' with your actual file name)

linelist <- import(here("data","BC CCHS data.csv")) %>% 
  mutate(WTS_M = as.numeric(WTS_M))

# import source population file

population <- import(here("data","Population_Projections.csv")) %>% 
  filter(Age != "Total") %>%
  filter(Age != "LT12") %>% 
  mutate(age_group = factor(Age),
         sex = factor(recode(Gender, "M" = "Male", "F" = "Female")),
         population = as.numeric(gsub(",", "", Pop), na.rm = TRUE))

cchs_data <- linelist %>% 
  filter(GEO_PRV == 59)  # filter for BC

# Select the desired columns
cchs_data <- cchs_data %>%
  select(
    ADM_RNO,
    GEO_PRV,
    GEODVBHA,
    GEODGHR4,
    DHH_SEX,
    DHHGMS,
    DHHDGLVG,
    DHHGAGE,
    SDC_015,
    SDC_035,
    SDCDVIMM,
    SDCDGCGT,
    GEN_020,
    GEN_025,
    GEN_030,
    GENDVSWL,
    SUI_005,
    SUI_010,
    HWTDGISW,
    HWTDGCOR,
    HWTDGBCC,
    FVCDVTOT,
    FVCDVGDT,
    SMK_005,
    ALCDVTTM,
    ALWDVDLY,
    DRMDVLA,
    DRMDVLAY,
    PAADVAC2,
    PAADVACV,
    PAADVWHO,
    PAYDVPAG,
    FSCDVAFS,
    FSCDVCFS,
    EHG2DVR3,
    INCG035,
    INCDGHH,
    INCDGPER,
    INCDVRCA,
    INCDVRPR,
    INCDVRRS,
    DHH_OWN,
    LBFG10,
    LBFDVWSS,
    LBFDVPFT,
    GENDVHDI,
    GENDVMHI,
    CCC_095,
    CCC_085,
    CCC_015,
    CCC_030,
    CCC_195,
    CCC_200,
    CCC_065,
    WTS_M)

# Create new age categories 

cchs_data <- cchs_data %>%
  mutate(age_group = case_when(
    DHHGAGE %in% c(1, 2, 3) ~ "12-19",
    DHHGAGE %in% c(4, 5, 6, 7) ~ "20-39",
    DHHGAGE %in% c(8, 9, 10,11) ~ "40-59",
    DHHGAGE %in% c(12, 13, 14, 15) ~ "60-79",
    TRUE ~ "80+"
  )) %>% 
  mutate(age_group = factor(age_group)) %>% 

# recode sex variable 
  
  mutate(sex = case_when(
    DHH_SEX == 1 ~ "Male",
    DHH_SEX == 2 ~ "Female"
  )) %>% 
  mutate(sex = factor(sex))

# Create a survey design

survey_design <- cchs_data %>% 
  as_survey_design(ids = 1, # no cluster ids
                   weights = WTS_M) # weight variable

## define x-axis limits and labels ---------------------------------------------

## (update these numbers to be the values for your graph)

max_prop <- 20      # choose the highest proportion you want to show 
step <- 5           # choose the space you want beween labels 

## this part defines vector using the above numbers with axis breaks

breaks <- c(
  seq(max_prop/100 * -1, 0 - step/100, step/100), 
  0, 
  seq(0 + step / 100, max_prop/100, step/100)
)

## this part defines vector using the above numbers with axis limits

limits <- c(max_prop/100 * -1, max_prop/100)

## this part defines vector using the above numbers with axis labels

labels <-  c(
  seq(max_prop, step, -step), 
  0, 
  seq(step, max_prop, step)
)


## create plots individually  --------------------------------------------------

## plot the source population 

source_population <- population %>%
  group_by(age_group, sex) %>% 

  ## add the counts for each group together 

  summarise(population = sum(population)) %>% 
    
  ## remove the grouping so can calculate overall proportion
    
  ungroup() %>% 
  mutate(proportion = population / sum(population)) %>% 
    
  ## plot pyramid 
    
  age_pyramid(
    age_group = "age_group", 
    split_by = "sex", 
    count = "proportion", 
    proportion = TRUE) +
    
  ## only show the y axis label (otherwise repeated in all three plots)
    
  labs(title = "Source population", 
       y = "", 
       x = "Age group (years)") + 
    
  ## make the x axis the same for all plots 
    
  scale_y_continuous(breaks = breaks, 
                     limits = limits, 
                     labels = labels)


## plot the unweighted sample population 

sample_population <- age_pyramid(
  cchs_data, 
  age_group = "age_group", 
  split_by = "sex",
  proportion = TRUE) + 
  
  ## only show the x axis label (otherwise repeated in all three plots)
  
  labs(title = "Unweighted sample population", 
       y = "Proportion (%)", 
       x = "") + 
  
  ## make the x axis the same for all plots 
  
  scale_y_continuous(breaks = breaks, 
                     limits = limits, 
                     labels = labels)


## plot the weighted sample population

weighted_population <- age_pyramid(
  survey_design,
  age_group = "age_group",
  split_by = "sex",
  proportion = TRUE
) +

## only show the x axis label (otherwise repeated in all three plots)
  
labs(title = "Weighted sample population", 
     y = "", 
     x = "")  + 
  ## make the x axis the same for all plots 
  
  scale_y_continuous(breaks = breaks, 
                     limits = limits, 
                     labels = labels)

## combine all three plots  ----------------------------------------------------

# Combine three plots next to each other using +

(source_population + sample_population + weighted_population) +
  
  # Only show one legend and define theme
  
  plot_layout(guides = "collect") &
  
  # Adjust the theme settings
  
  theme(
    legend.position = "bottom",       # Move legend to the bottom
    legend.title = element_blank(),  # Remove the legend title
    text = element_text(size = 14),   # Change text size
    plot.title = element_text(size = 12, hjust = 0.5),  # Center the plot titles
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)  # Center x-axis text
  )

