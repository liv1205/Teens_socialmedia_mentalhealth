
# TIC KDS ONLINE 2025 - QUESTIONNAIRE WITH THE CHILDREN

install.packages("here")
library(here)


# Importing the raw data - Source NIC.br 

install.packages("tidyverse")
library(tidyverse)
library(readr)
Data <- read_delim(here("dados", "microdata_2025.csv"),
                   delim = ";",
                   na = c("", " "))

# To verify if the data has been imported correctly
str(Data)

# filtering the data > 13

Data_v1 <- Data[Data$IDADE_KIDS >= 13,]


# Quick code here just to see what's the most used social media by the teens.

labels <- c(
  "1" = "Many times a day",
  "2" = "Everyday or almost everyday",
  "3" = "Less than once a week",
  "4" = "At least once a month",
  "5" = "Never or almost never",
  "97" = "Don't know",
  "98" = "Did not Answer",
  "99" = "Does not Apply"
)

DATA_NOM <- data.frame(lapply(Data_v1 %>%
                                rename("FreqFacebook" = "O6_A",
                                       "FreqInstagram" = "O6_B",
                                       "FreqTwitter" = "O6_C",
                                       "FreqTiktok" = "O6_D",
                                       "FreqWhatsapp" =  "O6_E",
                                       "FreqYoutube" = "O6_F",
                                       "FreqDiscord" = "O6_G"), trimws))

DATA_NOM$FreqFacebook <- labels[as.character(as.numeric(DATA_NOM$FreqFacebook))]
DATA_NOM$FreqInstagram <- labels[as.character(as.numeric(DATA_NOM$FreqInstagram))]
DATA_NOM$FreqTwitter <- labels[as.character(as.numeric(DATA_NOM$FreqTwitter))]
DATA_NOM$FreqTiktok <- labels[as.character(as.numeric(DATA_NOM$FreqTiktok))]
DATA_NOM$FreqWhatsapp <- labels[as.character(as.numeric(DATA_NOM$FreqWhatsapp))]
DATA_NOM$FreqYoutube <- labels[as.character(as.numeric(DATA_NOM$FreqYoutube))]
DATA_NOM$FreqDiscord <- labels[as.character(as.numeric(DATA_NOM$FreqDiscord))]


Probs <- list(
  Facebook  = round(prop.table(table(DATA_NOM$FreqFacebook)) * 100, 2),
  Instagram = round(prop.table(table(DATA_NOM$FreqInstagram)) * 100, 2),
  Twitter   = round(prop.table(table(DATA_NOM$FreqTwitter)) * 100, 2),
  TikTok    = round(prop.table(table(DATA_NOM$FreqTiktok)) * 100, 2),
  WhatsApp  = round(prop.table(table(DATA_NOM$FreqWhatsapp)) * 100, 2),
  YouTube   = round(prop.table(table(DATA_NOM$FreqYoutube)) * 100, 2),
  Discord   = round(prop.table(table(DATA_NOM$FreqDiscord)) * 100, 2)
)

Probs_table <- do.call(rbind, Probs)
view(Probs_table)

#
