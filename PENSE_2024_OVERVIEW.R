# Decided to take infos from the recent uploaded report from PeNSE 2024
# Recreated one of the graphs with my palette just to put in my project.

library(ggplot2)
library(tidyr)
library(dplyr)

dados_pense <- data.frame(
  Categoria = c("Satisfeito", "Indiferente", "Insatisfeito"),
  `2015` = c(70.2, 10.7, 19.1),
  `2019` = c(66.5, 10.5, 22.2),
  `2024` = c(58.0, 14.0, 27.2),
  check.names = FALSE
)

dados_longo <- dados_pense %>%
  pivot_longer(
    cols = c(`2015`, `2019`, `2024`),
    names_to = "Ano", 
    values_to = "Percentual"
  )

my_palette <- c("2015" = "#AE8361", "2019" = "#555C4C", "2024" = "#0A3351")

ggplot(dados_longo, aes(x = Categoria, y = Percentual, fill = Ano)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(Percentual, "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.5, fontface = "bold", color = "#6E727B") +
  scale_fill_manual(values = my_palette) +
  labs(title = "Evolução do Sentimento em Relação ao Próprio Corpo",
       subtitle = "Escolares de 13 a 17 anos (PeNSE 2015-2024)",
       x = NULL, y = "Percentual (%)",
       fill = "Edição da Pesquisa") +
  theme_minimal() +
  theme(
    text = element_text(color = "#6E727B"),
    plot.title = element_text(face = "bold", size = 14, color = "#0A3351"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
  ) +
  ylim(0, 80)
