
# libraries - Setting it up ====

library(tidyverse)
library(here)
library(readr)
library(dplyr)
library(magrittr)

#-----------------------------------

# ---- Collecting data ----
Years_RS <- c(2015,2018,2024)

cols_to_get <-  c("resp_idade",
                  "resp_escolaridade",
                  "e6_d",
                  "sexo_kids",
                  "idade_kids",
                  "raca_kids",
                  "n2_g",
                  "n2_g1",
                  "t10_a",
                  "t10_b",
                  "t10_c", 
                  "t12_a",
                  "t12_b",
                  "t12_d",
                  "t12_e",
                  "origin_year",
                  "peso",
                  "faixa_etaria",
                  "social_media_use")

TIC_ALLTIME <- Years_RS %>%
  set_names(Years_RS) %>%
  map_df(~ {
    read_delim(here("data",paste0("microdata_",.x, ".csv")),
               delim = ";",
               escape_double = FALSE,
               trim_ws = TRUE,
               na = c("", " ", "98", "99"),
               guess_max = 50000) %>%
      rename_with(tolower) %>%
      mutate(Years_RS = .x)
  }, .id = "origin_year")



  #-----------------------------------





# ---- Organizing data ----
TIC_ALLTIMESel <- TIC_ALLTIME %>%
  mutate(social_media_use = coalesce(n2_g,n2_g1)) %>%
  mutate(idade_aju = case_when(
    idade_kids %in% 9:10 ~ 1,
    idade_kids %in% 11:12 ~ 2,
    idade_kids %in% 13:14 ~ 3,
    idade_kids %in% 15:17 ~ 4,
    TRUE ~ as.numeric(faixa_etaria)
  )) %>%
  mutate(age_kids = coalesce(idade_aju, as.numeric(faixa_etaria)))%>%
  select(origin_year, age_kids, peso, any_of(cols_to_get)) %>%
  rename(
    "age_adult"         = "resp_idade",
    "adult_educ"        = "resp_escolaridade",
    "supervised_usage"  = "e6_d",
    "kids_gender"       = "sexo_kids",
    "race_kids"         = "raca_kids",
    "selfharm_index"    = "t10_a",
    "suicide_index"     = "t10_b",
    "skinny_index"      = "t10_c",
    "sleepless_index"   = "t12_a",
    "dependency_index"  = "t12_b",
    "qualitytime_index" = "t12_d",  
    "addiction_index"   = "t12_e",
    "weight"            = "peso"
  ) %>%
  select(-n2_g, -n2_g1, -faixa_etaria, -idade_kids)


view(TIC_ALLTIMESel)


#--------------------------------------------





#Creating graphs just so I learn how my database looks like ====

library(ggplot2)
library(scales)
install.packages("showtext")
library(showtext)

# setting default font

font_add_google("Roboto", "roboto") # Roboto é super limpa e moderna
showtext_auto() # Ativa o uso das fontes nos gráficos

# plotting graph 1 

TIC_ALLTIMESel %>%
  filter(TIC_ALLTIMESel$selfharm_index %in% c(0,1,97)) %>%
  ggplot(aes(x = factor(origin_year), fill = factor(selfharm_index),weight = weight)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c", "97" = "#bdc3c7"),
                    labels = c("0" = "No", "1" = "Yes", "97" = "Don't know")) +
  labs(title = "Self-Harm Content Access Evolution",
       subtitle = "Percentage of children reporting access to harmful content (Weighted)",
       x = NULL, y = "%",
       fill = "Answer") +
  theme_minimal(base_family = "roboto")+
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
    axis.title.y = element_text(size = 9, color = "#7f8c8d"),
    axis.text = element_text(size = 11, color = "#2c3e50"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )




# =================

