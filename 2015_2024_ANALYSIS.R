
# libraries - Setting it up ====

set.seed(290904)

library(tidyverse)
library(here)
library(readr)
library(dplyr)
library(magrittr)
install.packages("ordinal")
library(ordinal)
library(forcats)

# ---- Collecting data ----
Years_RS <- c(2015,2018,2024)

cols_to_get <-  c("resp_idade",
                  "resp_escolaridade",
                  "e6_d",
                  "sexo_kids",
                  "idade_kids",
                  "raca_kids",
                  "n2_g",
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
                  "t8",
                  "social_media_use",
                  "resp_sexo"
                  )

TIC_ALLTIME <- Years_RS %>%
  set_names(Years_RS) %>%
  map_df(~ {
    read_delim(here("data",paste0("microdata_",.x, ".csv")),
               delim = ";",
               escape_double = FALSE,
               trim_ws = TRUE,
               na = c("", " ", "99"),
               guess_max = 50000) %>%
      rename_with(tolower) %>%
      mutate(Years_RS = .x)
  }, .id = "origin_year")

TIC_ALLTIME$n2_g <- fct_rev(as.factor(TIC_ALLTIME$n2_g))

view(TIC_ALLTIME)






# ---- Organizing data ----
TIC_ALLTIMESel <- TIC_ALLTIME %>%
  mutate(idade_aju = case_when(
    idade_kids %in% 9:10 ~ 1,
    idade_kids %in% 11:12 ~ 2,
    idade_kids %in% 13:14 ~ 3,
    idade_kids %in% 15:17 ~ 4,
    TRUE ~ as.numeric(faixa_etaria)
  )) %>%
  filter(!n2_g %in% c(99,100), 
         !is.na(n2_g), 
         idade_aju %in% c(3,4)) %>%
  mutate(age_kids = coalesce(idade_aju, as.numeric(faixa_etaria)))%>%
  select(origin_year, age_kids, peso, any_of(cols_to_get)) %>%
  rename(
    "age_adult"         = "resp_idade",
    "supervised_usage"  = "e6_d",
    "kids_gender"       = "sexo_kids",
    "race_kids"         = "raca_kids",
    "selfharm_index"    = "t10_a",
    "suicide_index"     = "t10_b",
    "imagedistortion_index"      = "t10_c",
    "sleepless_index"   = "t12_a",
    "dependency_index"  = "t12_b",
    "qualitytime_index" = "t12_d",  
    "addiction_index"   = "t12_e",
    "weight"            = "peso",
    "sa_danger"         = "t8",
    "social_media_use"  = "n2_g",
    "adult_gender"      = "resp_sexo"
  ) %>%
  mutate(adult_educ = case_when(
    resp_escolaridade %in% c(1, 2, 3) ~ 1, # Low
    resp_escolaridade %in% c(4, 5, 6) ~ 2, # Medium
    resp_escolaridade >= 7 ~ 3,            # High
    TRUE ~ NA_real_
  )) %>%
  mutate(social_media_simple = case_when(
    social_media_use %in% c(5, 4, 3) ~ 1, # High
    social_media_use %in% c(1, 2) ~ 2,  # Low
    TRUE ~ NA_real_
  )) %>%
  mutate(danger_exposure = rowSums(across(c(selfharm_index,
                                            imagedistortion_index,
                                            sa_danger)), na.rm = TRUE)
         ) %>%
  filter(!danger_exposure %in% c(90:400)) %>%
  select(-faixa_etaria, -idade_kids, -resp_escolaridade)




view(TIC_ALLTIMESel)






###########################################################################

#Creating graphs just so I learn how my database looks like ====

library(ggplot2)
library(scales)
install.packages("showtext")
library(showtext)
install.packages("ggthemes")
library(ggthemes)

# setting default font

font_add_google("Roboto", "roboto") # Roboto é super limpa e moderna
showtext_auto() # Ativa o uso das fontes nos gráficos

# plotting graph 1 -> Self Harm

TIC_ALLTIMESel %>%
  filter(TIC_ALLTIMESel$selfharm_index %in% c(0,1,97) & TIC_ALLTIMESel$age_kids %in% c(3,4)) %>%
  ggplot(aes(x = factor(origin_year), fill = factor(selfharm_index),weight = weight)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("0" = "#0A3351", "1" = "#AE8363", "97" = "#697677"),
                    labels = c("0" = "Não", "1" = "Sim", "97" = "Não sei")) +
  labs(title = "Evolução do acesso a conteúdo nocivo",
       subtitle = "Porcentagem de crianças que acessaram formas de se machucar (Amostra de 13 a 17 anos)",
       x = NULL, y = "%",
       fill = "Resposta ") +
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


#---- plotting "skinny_index" for boys and girls ----

TIC_ALLTIMESel %>%
  filter(TIC_ALLTIMESel$skinny_index %in% c(0,1,97) & 
           TIC_ALLTIMESel$age_kids %in% c(3,4) &
           TIC_ALLTIMESel$kids_gender == 1) %>% # Masculine
  ggplot(aes(x = factor(origin_year), fill = factor(skinny_index),weight = weight)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("0" = "#BEE3DB", "1" = "#A7C7E7", "97" = "#CFCFCF"),
                    labels = c("0" = "Não", "1" = "Sim", "97" = "Não sei")) +
  labs(title = "Evolução da Auto imagem ",
       subtitle = "Porcentagem de crianças que pesquisaram maneiras de perder peso (Meninos de 13 a 17 anos)",
       x = NULL, y = "%",
       fill = "Resposta ") +
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


TIC_ALLTIMESel %>%
  filter(TIC_ALLTIMESel$skinny_index %in% c(0,1,97) & 
           TIC_ALLTIMESel$age_kids %in% c(3,4) &
           TIC_ALLTIMESel$kids_gender == 2   # Feminine  
           ) %>%
  ggplot(aes(x = factor(origin_year), fill = factor(skinny_index),weight = weight)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("0" = "#D8C3E5", "1" = "#F4C2C2", "97" = "#CFCFCF"),
                    labels = c("0" = "Não", "1" = "Sim", "97" = "Não sei")) +
  labs(title = "Evolução da Auto imagem ",
       subtitle = "Porcentagem de crianças que pesquisaram maneiras de perder peso (Meninas de 13 a 17 anos)",
       x = NULL, y = "%",
       fill = "Resposta") +
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



# Plotting "Skinny_index" -> Together: by gender ----

TIC_ALLTIMESel %>%
  filter(skinny_index %in% c(0,1,97),
         age_kids %in% c(3,4)) %>% # age 13-17
  ggplot(aes(x = factor(origin_year),
             fill = factor(skinny_index),
             weight = weight)) +
  geom_bar(position = "fill") +
  facet_wrap(~kids_gender, nrow = 1,
             labeller = labeller(kids_gender = c("1" = "Meninos", "2" = "Meninas"))) + 
  # 1 - boys/meninos and 2 - girls/meninas
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("0" = "#0A3351", "1" = "#AE8363", "97" = "#697677"),
                    labels = c("0" = "Não" , "1" = "Sim", "97" = "Não sei")) + # 0 - NO; 1- YES; 97 - Don't Know
  labs(title = "Indicadores de distorção de imagem e comportamento dietético", #Index of image distortion
       subtitle = "Porcentagem de adolescentes (13–17 anos) que pesquisaram maneiras de perder peso",
       # percentage of kids who looked up online how to get skinny
       x = "Ano", y = "%",
       fill = "Resposta") +
  theme_minimal(base_family = "roboto") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
    axis.title.y = element_text(size = 11, color = "#7f8c8d"),
    axis.text = element_text(size = 11, color = "#2c3e50"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank()
  )


  # Plotting usage throughout the years


ggplot(TIC_ALLTIMESel %>%
         filter(social_media_use %in% c(5,1,3,4,2),
                age_kids %in% c(3,4)),
       aes(x = origin_year,
           y = ..prop..,
           color = factor(social_media_use),
           group = social_media_use)) +
  geom_line(stat = "count", size = 1.2) +
  geom_point(stat = "count", size = 3) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_color_manual(values = c("3"="#0A3351", "1"="#AE8363", "5"="#697677", "4" = "#555C4C","2" = "#E1DFD3"),
                     labels = c("1" = "Várias vezes ao dia", #many times a day
                                "2" = "Pelo menos uma vez por dia", #at least once a day
                                "3" = "Pelo menos uma vez na semana", # At least once a week
                                "4" = "Pelo menos uma vez por mês", # At least once a month
                                "5" = "Menos de uma vez por mês" # less than once a month
                                )) +
  labs(title = "Evolução do Uso de redes sociais", #social media usage evolution
       subtitle = "O quanto crianças afirmam ter usado redes sociais no curto prazo",
       x = "Ano", y = "%",
       color = "Resposta") +
  theme_minimal(base_family = "roboto") +
  theme(
    theme_economist()
  )



# First regression ----

model_v1 <- clm(as.factor(social_media_use) ~ as.factor(age_kids) + 
                  as.factor(adult_educ) + 
                  as.factor(kids_gender) + 
                  as.factor(origin_year),
                  data = TIC_ALLTIMESel,
                  link = "logit"
                )
summary(model_v1)

Exposure_danger_v1 <- clm(as.factor(danger_exposure) ~ as.factor(social_media_simple) + 
                  as.factor(age_kids) + 
                  as.factor(adult_educ) + 
                  as.factor(kids_gender) + 
                  as.factor(origin_year),
                data = TIC_ALLTIMESel,
                link = "logit"
)
summary(Exposure_danger_v1)


#########################################################################
# ----- Parental Distance -----

parental_distance <- TIC_ALLTIME %>% #----
  select(origin_year, peso, area, cod_regiao, renda_familiar, resp_sexo, any_of(cols_to_get)) %>%
  rename(
    "age_adult"         = "resp_idade",
    "supervised_usage"  = "e6_d",
    "kids_gender"       = "sexo_kids",
    "race_kids"         = "raca_kids",
    "selfharm_index"    = "t10_a",
    "suicide_index"     = "t10_b",
    "imagedistortion_index"      = "t10_c",
    "sleepless_index"   = "t12_a",
    "dependency_index"  = "t12_b",
    "qualitytime_index" = "t12_d",  
    "addiction_index"   = "t12_e",
    "weight"            = "peso",
    "sa_danger"         = "t8",
    "social_media_use"  = "n2_g",
    "urb"               = "area",
    "adult_gender"      = "resp_sexo"
  ) %>%
  mutate(area = case_when(
    cod_regiao == 1 ~ "Norte",
    cod_regiao == 2 ~ "Nordeste",
    cod_regiao == 3 ~ "Sudeste",
    cod_regiao == 4 ~ "Sul",
    cod_regiao == 5 ~ "Centro Oeste"
  ))%>%
  mutate(urbano = case_when(
    urb == 1 ~ "Urbana",
    urb == 2 ~ "Rural"
  )) %>%
  filter(selfharm_index %in% c(97,98,99 , 0:5))
view(parental_distance)

remotes::install_github("ipeaGIT/geobr")
library(geobr)

ggplot(
  read_region(year = 2020) %>%
    left_join(
      parental_distance %>%
        group_by(area) %>%
        summarise(prop = mean(selfharm_index %in% c(97,98)), .groups = "drop"),
      by = c("name_region" = "area")
    )
) +
  geom_sf(aes(fill = prop)) +
  scale_fill_gradient(low = "#7f8c8d", high = "#2c3e50", labels = percent_format()) +
  labs(title = "Desconhecimento Parental: Automutilação Online",
       subtitle = "Proporção de 'Não sabe' entre responsáveis, por macrorregião",
       fill = "% de respostas") +
  theme_void() #----



parental_distance %>%
  filter(selfharm_index %in% c(97, 98)) %>%
  mutate(genero_resp = case_when(
    as.numeric(adult_gender) == 1 ~ "Masculino",
    as.numeric(adult_gender) == 2 ~ "Feminino",
    TRUE ~ "Não informado"
  )) %>%
  count(genero_resp) %>%
  mutate(proporcao = n / sum(n) * 100) %>%
  print()



    parental_distance %>%
  filter(selfharm_index %in% c(97, 98)) %>%
  summarise(
    media_idade = mean(age_adult, na.rm = TRUE),
    minimo = min(age_adult, na.rm = TRUE),
    maximo = max(age_adult, na.rm = TRUE),
    n_validos = sum(!is.na(age_adult)) # Conta quantos sobraram
  )





read_region(year = 2020) %>%
  left_join(
    parental_distance %>%
      group_by(area, urbano) %>% #----
      summarise(prop = mean(selfharm_index %in% c(97,98)), .groups = "drop"),
    by = c("name_region" = "area")
  ) %>%
  ggplot() +
  geom_sf(aes(fill = prop)) +
  facet_wrap(~urbano) + 
  scale_fill_gradient(low = "#BEE3DB", high = "#2c3e50", labels = percent_format()) +
  labs(title = "Desconhecimento Parental: Automutilação Online",
       subtitle = "Proporção de 'Não sabe' entre responsáveis, por macrorregião",
       fill = "% de respostas") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 20)),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )
#----



parental_distance %>%
  filter(selfharm_index %in% c(97, 98), 
         renda_familiar %in% c(1:8), 
         !is.na(renda_familiar)) %>% #----
  mutate(renda_familiar_label = factor(case_when( 
    renda_familiar == 1 ~ "Até R$ 788,00",
    renda_familiar == 2 ~ "R$ 788,01 – R$ 1.576,00",
    renda_familiar == 3 ~ "R$ 1.576,01 – R$ 2.364,00",
    renda_familiar == 4 ~ "R$ 2.364,01 – R$ 3.940,00",
    renda_familiar == 5 ~ "R$ 3.940,01 – R$ 7.880,00",
    renda_familiar == 6 ~ "R$ 7.880,01 – R$ 15.760,00",
    renda_familiar == 7 ~ "R$ 15.760,01 – R$ 23.640,00",
    renda_familiar == 8 ~ "Mais de R$ 23.640,00"
  ), levels = rev(c( 
    "Até R$ 788,00", "R$ 788,01 – R$ 1.576,00", "R$ 1.576,01 – R$ 2.364,00",
    "R$ 2.364,01 – R$ 3.940,00", "R$ 3.940,01 – R$ 7.880,00", 
    "R$ 7.880,01 – R$ 15.760,00", "R$ 15.760,01 – R$ 23.640,00", "Mais de R$ 23.640,00"
  )))) %>%
  ggplot(aes(x = renda_familiar_label)) +
  geom_bar(fill = "#2c3e50") +
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.2, color = "black", size = 4) +
  coord_flip() + 
  labs(
    title = "Desconhecimento Parental sobre Automutilação por Renda",
    subtitle = "Frequência de respostas 'Não sei/Não respondeu' dos responsáveis",
    x = "Faixa de Renda Familiar",
    y = "Número de Respostas"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))# ----


parental_distance %>%
  # Filtramos e convertemos no ar
  filter(selfharm_index %in% c(97, 98)) %>%
  mutate(age_adult = as.numeric(as.character(age_adult))) %>%
  filter(!is.na(age_adult)) %>%
  # Plot direto
  ggplot(aes(x = age_adult)) +
  geom_histogram(binwidth = 2, fill = "#2c3e50", color = "white") +
  geom_vline(aes(xintercept = mean(age_adult)), color = "red", linetype = "dashed") +
  labs(
    title = "Distribuição de Idade: Responsáveis que 'Não Sabem'",
    x = "Idade do Responsável (Anos)",
    y = "Quantidade (Contagem Absoluta)"
  ) +
  theme_minimal()



# Analysis of parental internet usage


  






