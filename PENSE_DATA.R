
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



PENSE2019 <- fread("data/pense_2019.csv",
                   sep = ",",
                   encoding = "Latin-1") %>%
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
    'quality_time' = 'b02018b',
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
  )









