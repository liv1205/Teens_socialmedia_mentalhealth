
# Libraries and setting seed
set.seed(290904)

library(tidyverse)
library(here)
library(readr)
library(dplyr)
library(readxl)
install.packages("janitor")
library(janitor)
install.packages("data.table")
library(data.table)
library(car)
install.packages('pscl')
library(pscl)


PENSE2024 <- fread("data/pense_2024.csv",
                   sep = ",",
                   encoding = "Latin-1") %>% #----
  select('IDADE_AGREG',
         'B01026A2',
         'B01014',
         'B01016',
         'B01008B',
         'B02017A',
         'B02018B',
         'B12004',
         'B12009',
         'B03010C',
         'B07002',
         'B07004',
         'B07006',
         'B07007B',
         'B01001A',
         'B05002A',
         'B07013',
         'B12005',
         'B12003',
         'B01002',
         'B0600702',
         'B09016A2',
         'B11007',
         'B11002',
         'B13001',
         'B03006B'
  ) %>%
  clean_names() %>%
  distinct() %>%
  rename(
    "age" = 'idade_agreg', # 2 = 13 to 15 and 3 = 16,17 
    "dreams" = 'b01026a2', # Reconsider
    'ch_gender' = 'b01001a', #dummy
    'race' = 'b01002', # will be specified later
    "smartphone" = 'b01014', # dummy
    "homeint" = "b01016", # dummy but a reguirement (reconsider)
    "adult_edu" = 'b01008b', # likert like data
    "family_time" = 'b02017a', # likert like data
    "distraction" = 'b02018b', # likert like data
    "screentime" = 'b03010c', # likert like data
    'alcohol_use' = 'b05002a', #dummy
    'self_harm' = 'b12009', # dummy
    "adt_closeness" = 'b07002', # likert like data
    'exercise' = 'b03006b', # likert like data
    "adt_trust" = 'b07004', # likert like data
    'drug_usage' = 'b0600702', # dummy
    'sadness' = 'b12005', # dummy
    'anxiety' = 'b12004', # dummy 
    "sch_friends" = 'b07006', # likert like data
    "sch_bully" = 'b07007b', # likert like data
    "socialmedia_bully" = 'b07013', # likert like data
    "friends" = 'b12003', # likert like data
    "sa" = 'b09016a2', # dummy 
    "body_image" = 'b11007', #likert like data
    "dietetic_beha" = 'b11002', # likert like data
    "health" = 'b13001' # dummy
  ) %>%
  mutate(across(
    everything(),
    ~ ifelse(.x %in% c(-1,-2,-9,10,90:100), NA, .x)
  )) %>%
  mutate(
    alcohol_use       = if_else(alcohol_use == 2, 0, as.numeric(alcohol_use)),
    anxiety           = if_else(anxiety %in% c(5,4,3), 1, 0),
    socialmedia_bully = if_else(socialmedia_bully == 2, 0, as.numeric(socialmedia_bully)),
    sa                = if_else(sa == 2, 0, as.numeric(sa)),
    ch_gender         = if_else(ch_gender == 2, 0, as.numeric(ch_gender)),
    smartphone        = if_else(smartphone == 2, 0, as.numeric(smartphone)),
    health            = if_else(health == 2, 0, as.numeric(health)),
    self_harm         = if_else(self_harm == 2, 0, as.numeric(self_harm)),
    friends           = if_else(friends %in% c(4,3), 0, 1), # inverted higher means fewer friends
    adt_closeness     = if_else(adt_closeness %in% c(5,4,3), 1, 0), # higher means worse
    family_time       = 6 - as.numeric(family_time), # higher means worse
    body_image        = if_else(body_image %in% c(5,4,3), 1, 0), #  higher (worse)
    exercise = 9 - as.numeric(exercise),
    adult_edu = 9- as.numeric(adult_edu),
    distraction = 6 - as.numeric(distraction)
  ) %>%
  filter(
    homeint == 1 & # filtering bellow 13 and above 18
      !age %in% c(1,-9,4) &
      !is.na(age) &
      !is.na(race)
  )%>%
  mutate(
    race_white     = if_else(race == 1, 1, 0),  # Branca
    race_black     = if_else(race == 2, 1, 0),  # Preta
    race_asian     = if_else(race == 3, 1, 0),  # Amarela
    # mixed ommited
    race_indigenous = if_else(race == 5, 1, 0)  # Indígena
  ) %>%
  mutate(
    screentime        = as.integer(screentime),   # higher = more screen time (worse)
    distraction       = as.integer(distraction),  # higher = more distraction (worse)
    adult_edu         = as.integer(adult_edu),    # higher = less education (worse)
    family_time       = as.integer(family_time),
    exercise          = as.integer(exercise),     # higher = less exercise (worse)
    adt_closeness     = as.integer(adt_closeness),
    adt_trust         = as.integer(adt_trust),
    sch_friends       = as.integer(sch_friends),
    sch_bully         = as.integer(sch_bully),
    socialmedia_bully = as.integer(socialmedia_bully),
    friends           = as.integer(friends),
    body_image        = as.integer(body_image),
    dietetic_beha     = as.integer(dietetic_beha)
  ) %>%
  mutate(
    screentime_cat = case_when(
      screentime <= 2 ~ 1,  # low: up to 2h/day
      screentime <= 5 ~ 2,  # moderate: 2h to 5h/day
      screentime <= 9 ~ 3,  # high: more than 5h/day
      TRUE ~ NA_real_       # excludes value 10 (don't know/don't remember)
    ),
    screentime_cat = factor(screentime_cat,
                            levels = c(1, 2, 3),
                            labels = c("Low", "Moderate", "High"))
  ) %>%
  select(-race) #-----

summary(PENSE2024)
#--------------

model_anxiety <- glm(
    anxiety ~ screentime_cat + ch_gender +
      distraction + family_time + adt_closeness + adt_trust +
      sch_friends + sch_bully + socialmedia_bully + sa +
      alcohol_use + drug_usage + exercise + health +
      adult_edu + age +
      race_white + race_black + race_asian + race_indigenous,
    data   = PENSE2024,
    family = binomial(link = "logit")
  )

summary(model_anxiety)  


model_relationships <- glm(
  friends ~ screentime_cat + ch_gender +
    distraction + family_time + adt_closeness + adt_trust +
    sch_friends + sch_bully + socialmedia_bully +
    alcohol_use + drug_usage + exercise +
    adult_edu + age +
    race_white + race_black + race_asian + race_indigenous,
  data   = PENSE2024,
  family = binomial(link = "logit")
)

summary(model_relationships)


model_selfimage <- glm(
  body_image ~ screentime_cat + ch_gender +
    distraction + family_time + dietetic_beha +
    sch_bully + socialmedia_bully + sa +
    alcohol_use + drug_usage + exercise + health +
    adult_edu + age +
    race_white + race_black + race_asian + race_indigenous,
  data   = PENSE2024,
  family = binomial(link = "logit")
)
summary(model_selfimage)




vif(model_anxiety)
vif(model_relationships)
vif(model_selfimage)


model_family <- glm(
  adt_closeness ~ screentime_cat +
    distraction + family_time +
    sch_friends + sch_bully + socialmedia_bully +
    friends + alcohol_use + drug_usage + exercise +
    adult_edu + ch_gender + age +
    race_white + race_black + race_asian + race_indigenous,
  data   = PENSE2024,
  family = binomial(link = "logit")
)

summary(model_family)
vif(model_family)


model_selfharm <- glm(
  self_harm ~ screentime_cat +
    anxiety + sadness +
    distraction + family_time + adt_closeness + adt_trust +
    sch_bully + socialmedia_bully + sa +
    alcohol_use + drug_usage + exercise +
    adult_edu + ch_gender + age +
    race_white + race_black + race_asian + race_indigenous,
  data   = PENSE2024,
  family = binomial(link = "logit")
)
summary(model_selfharm)
vif(model_selfharm)


table(PENSE2024$self_harm)
