
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



PENSE2015 <- fread(
  "data/pense_20152.csv",
  sep = ";",
  encoding = "Latin-1") %>% #-----
  select(
    'VB01003',
    'VB01025',
    'VB01014',
    'VB01016',
    'VB01008A',
    'VB02017A',
    'VB02018A',
    'VB07002',
    'VB07004',
    'VB07006',
    'VB12003',
    'VB09016',
    'VB13001',
    'VB11001',
    'VB11002',
    'VB11003',
  ) %>%
  clean_names() %>%
  distinct() %>%
  rename(
    'age_ba' = 'vb01003', # age before adjustment
    'dreams' = 'vb01025',
    'smartphone' = 'vb01014',
    'homeint' = 'vb01016',
    'adult_educ' = 'vb01008a',
    'family_time' = 'vb02017a',
    'distraction' = 'vb02018a',
    'adt_closeness' = 'vb07002',
    'adt_trust' = 'vb07004',
    'sch_friends' = 'vb07006',
    'friends' = 'vb12003',
    'sa' = 'vb09016',
    'health' = 'vb13001',
    'body_image' = 'vb11001',
    'dietetic_beha' = 'vb11002',
    'ed' = 'vb11003'
  ) %>%
  filter(
    !age_ba %in% c(11,12,18,19) & # filtering bellow 13 and above 18
      homeint == 1 
  ) %>%
  mutate(
    age = case_when(
      age_ba %in% c(13:15) ~ 2,
      age_ba %in% c(16,17) ~ 3
    )
  ) %>%
  select(-age_ba) %>%
  mutate(Rs_year = "2015")#----
  

# METHODOLOGICAL NOTE: HARMONIZATION OF DISTRACTED BEHAVIOR VARIABLES (2015-2024)
#
# Variable: distraction (Proxy for Distracted / Mindless Eating)
#
# Wording Shift:
# 2015: "watching TV or studying" -> 2019/2024: "watching TV or using a cell phone"
#
# Rationale:
# Both formulations capture the identical underlying latent construct: the presence 
# of external stimuli disrupting cognitive attention during meals (Mindless Eating). 
# The shift by IBGE reflects instrument calibration to capture screen-time displacement 
# rather than a structural break in the behavioral metric. 
#
# Decision:
# Kept and harmonized into a single proxy to preserve the 3-wave longitudinal series.


PENSE2019 <- fread("data/pense_2019.csv",
                   sep = ",",
                   encoding = "Latin-1") %>% #-----
  select('B01003',
         'B01026A2',
         'B01014',
         'B01016',
         'B01008B',
         'B02017A',
         'B02018B',
         'B07002',
         'B07004',
         'B07006',
         'B07007A',
         'B07013',
         'B12003',
         'B09016A2',
         'B11001',
         'B11002',
         'B13001',
         'B11003'
         ) %>%
  clean_names() %>%
  distinct() %>%
  rename(
    'age' = 'b01003',
    'dreams' = 'b01026a2',
    'smartphone' = 'b01014',
    'homeint' = 'b01016',
    'adult_edu' = 'b01008b',
    'family_time' = 'b02017a',
    'distraction' = 'b02018b',
    'adt_closeness' = 'b07002',
    'adt_trust' = 'b07004',
    'sch_friends' = 'b07006',
    'sch_bully' = 'b07007a',
    'socialmedia_bully' = 'b07013',
    'friends' = 'b12003',
    'sa' = 'b09016a2',
    'body_image' = 'b11001',
    'dietetic_beha' = 'b11002' ,
    'health' = 'b13001',
    'ed' = 'b11003'
  ) %>%
  filter(
    !age %in% c(1,9,4) &  # filtering bellow 13 and above 18
    homeint == 1 
      ) 
  mutate(Rs_year = "2019") #-----
    

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
         'B03010C',
         'B07002',
         'B07004',
         'B07006',
         'B07007B',
         'B07013',
         'B12003',
         'B09016A2',
         'B11001A',
         'B11002',
         'B13001'
  ) %>%
  clean_names() %>%
  distinct() %>%
  rename(
    "age" = 'idade_agreg',
    "dreams" = 'b01026a2',
    "smartphone" = 'b01014',
    "homeint" = "b01016",
    "adult_edu" = 'b01008b',
    "family_time" = 'b02017a',
    "distraction" = 'b02018b',
    "screentime" = 'b03010c',
    "adt_closeness" = 'b07002',
    "adt_trust" = 'b07004',
    "sch_friends" = 'b07006',
    "sch_bully" = 'b07007b',
    "socialmedia_bully" = 'b07013',
    "friends" = 'b12003',
    "sa" = 'b09016a2',
    "body_image" = 'b11001a',
    "dietetic_beha" = 'b11002',
    "health" = 'b13001'
  ) %>%
  filter(
    homeint == 1 & # filtering bellow 13 and above 18
      !age %in% c(1,-9,4) 
  )%>%
  mutate(Rs_year = "2024") #-----







