# Summary barplot figures
# Jessica Bolin 
# This does not make figs for realm, province or location
# Note, these are ugly, will do them up nicely in Illustrator

# Dependencies ------------------------------------------------------------

library(tidyverse)

dat <- read_rds("out/cleaned_climref_summarystats_NOLOCATIONINFO.rds")
df <- dat[[1]]
species <- dat[[2]] %>% as.tibble()
depth <- dat[[3]] %>% as.tibble()


# Barplots ----------------------------------------------------------------

bar_df <- df %>%
  select(
    Scale,
    EEZ,
    Time,
    Type_data,
    Stressor,
    Final_refugia_type
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  count(Variable, Category)

ggplot(bar_df, aes(x = Category, y = n)) +
  geom_col() +
  facet_wrap(~ Variable, scales = "free_x") +
 # theme_minimal() +
  labs(
    x = NULL,
    y = "Number of studies"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  ggsave("out/summarystats_facet.png")


# Depth -------------------------------------------------------------------

depth_long <- depth_counts %>%
  pivot_longer(
    cols = everything(),
    names_to = "depth",
    values_to = "n"
  )

ggplot(depth_long, aes(x = depth, y = n)) +
  geom_col() +  labs(
    x = NULL,
    y = "Number of studies"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) + 
  ggsave("out/summarystats_depth.png")


# Species -----------------------------------------------------------------

species_long <- species %>%
  pivot_longer(
    cols = everything(),
    names_to = "Species",
    values_to = "n"
  )

ggplot(species_long, aes(x = Species, y = n)) +
  geom_col(width = 1) +
  coord_polar(start = 0) +
  theme_minimal() +
  labs(
    title = "Taxonomic groups represented",
    y = "Number of studies",
    x = NULL
  ) +
  theme(
    axis.text.x = element_text(size = 10)
  ) + 
  ggsave("out/summarystats_species.png")


