library(tidyverse)
install.packages("factoextra")
library(factoextra)
install.packages("cluster")
library(cluster)
install.packages("remotes")
remotes::install_github("kassambara/factoextra")



# ── Select and prepare variables for clustering ────────────────────────────────
# Using only the behavioral/attitudinal variables — excluding identifiers and dummies
vars_cluster <- PENSE2024 %>%
  select(
    screentime_cat,       # screen time (categorized as 1/2/3)
    distraction,          # distraction during meals
    family_time,          # time with family
    adt_closeness,        # closeness to adults
    adt_trust,            # trust in adults
    sch_friends,          # school friendships
    sch_bully,            # school bullying
    socialmedia_bully,    # cyberbullying
    friends,              # friendships quality
    body_image,           # body image perception
    dietetic_beha,        # dietetic behavior
    exercise,             # physical activity
    adult_edu             # parental education
  ) %>%
  mutate(screentime_cat = as.integer(screentime_cat)) %>%
  drop_na() %>%           # k-means cannot handle NAs
  scale() %>%             # standardize: mean = 0, sd = 1 (required for k-means)
  t()                     # transpose: cluster variables, not observations

# ── Step 1: Find optimal number of clusters (elbow method) ────────────────────
set.seed(42)

fviz_nbclust(
  vars_cluster,
  kmeans,
  method = "wss",         # within-cluster sum of squares
  k.max  = 8
) +
  labs(
    title    = "Método do cotovelo — número ideal de clusters",
    subtitle = "Agrupamento de variáveis comportamentais · PeNSE 2024",
    x        = "Número de clusters",
    y        = "Soma dos quadrados intra-cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ── Step 2: Run k-means with chosen k ─────────────────────────────────────────
# Replace k = 3 with the elbow result after inspecting the plot above
k <- 3

set.seed(42)
km_result <- kmeans(vars_cluster, centers = k, nstart = 25)

# ── Step 3: Attach cluster labels to variable names ───────────────────────────
cluster_df <- tibble(
  variable = rownames(vars_cluster),
  cluster  = factor(km_result$cluster)
)

print(cluster_df)

# ── Step 4: Visualize clusters in reduced dimensions (PCA) ────────────────────
fviz_cluster(
  km_result,
  data        = vars_cluster,
  geom        = "text",           # show variable names instead of points
  repel       = TRUE,             # avoid label overlap
  ellipse     = TRUE,
  ellipse.type = "convex"
) +
  scale_color_manual(values = c("#0A3351", "#AE8361", "#555C4C")) +
  scale_fill_manual(values  = c("#0A3351", "#AE8361", "#555C4C")) +
  labs(
    title    = "Agrupamento de variáveis comportamentais",
    subtitle = "K-means com redução PCA · PeNSE 2024",
    caption  = "Fonte: PeNSE/IBGE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold"),
    plot.caption = element_text(color = "grey55", size = 9)
  )
