
library(tidyverse)
library(here)
library(readr)


# 2020 is missing!
Years_RS <- c(2015:2019, 2021:2024)

cols_to_get <-  c("year",
                  "resp_idade",
                  "resp_escolaridade",
                  "e6_d",
                  "sexo_kids",
                  "idade_kids",
                  "raca_kids",
                  "n1_g",
                  "n1_g1",
                  "t10_a",
                  "t10_b",
                  "t10_c", 
                  "t12_a",
                  "t12_b",
                  "t12_d",
                  "t12_e",
                  "origin_year")

TIC_ALLTIME <- Years_RS %>%
  set_names() %>%
  map_df(~ {
    read_delim(here("data",paste0("microdata_",.x, ".csv")),
               delim = ";",
               na = c("", " ", "97", "98", "99"),
               col_types = cols(.default = "d"),
               guess_max = 50000) %>%
      rename_with(tolower) %>%
      mutate(Year = .x)
  }, .id = "origin_year")


TIC_ALLTIMESel <- TIC_ALLTIME %>%
  select(any_of(cols_to_get))

head(TIC_ALLTIME)

view(TIC_ALLTIME)
view(TIC_ALLTIMESel)
# dealing with the changes in column names






