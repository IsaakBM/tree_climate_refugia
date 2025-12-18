library(sf)
library(terra)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

source("R/utils_helpers.R")
source("R/load_packages.R")

meow <- st_read("data-raw/boundaries/meow/meow_ecos_expl_clipped_expl.shp")
meow <- st_make_valid(meow)
names(meow)
unique(meow$PROVINCE)
length(unique(meow$PROVINCE))

meow %>% 
  st_drop_geometry() %>%
  summarise(
    n_features   = n(),
    n_realm      = n_distinct(REALM),
    n_province   = n_distinct(PROVINCE),
    n_ecoregion  = n_distinct(ECOREGION)
  )

meow %>% 
  st_drop_geometry() %>%
  group_by(ECOREGION) %>%
  summarise(n_prov = n_distinct(PROVINCE), .groups = "drop") %>%
  count(n_prov)

# Dissolve to one polygon per ECOREGION
meow_ecoreg <- meow %>%
  st_make_valid() %>%                    # optional, but helps avoid union errors
  group_by(ECO_CODE, ECOREGION, PROV_CODE, PROVINCE, RLM_CODE, REALM) %>%
  summarise(.groups = "drop")

# test <- meow %>%
#   mutate(province_key = str_squish(str_to_lower(PROVINCE))) %>%
#   distinct(province_key, PROVINCE, .keep_all = TRUE)

# Wrap dateline to avoid world-spanning polygon edges when projecting/plotting
test <- sf::st_wrap_dateline(
  meow_ecoreg,
  options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
  quiet = TRUE
)
# test <- st_make_valid(test)
# length(unique(test$province_key))
# plot(st_geometry(test))

df <- read_csv("data-raw/reference_data/Climref_dataset_final.csv")
length(unique(df$Province))

land <- get_world_latlon()  # land polygons in lon/lat (WGS84)

p1 <- ggplot() + 
  geom_sf(
    data = test,
    fill = "red",
    color = "red",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) + 
  geom_sf(
  data = land,
  fill = "grey20",
  color = "grey30",
  linewidth = 0.2,
  inherit.aes = FALSE
)

# print(p1)
ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v01.pdf",
  plot = p1, dpi = 400, width = 20, height = 10
)
