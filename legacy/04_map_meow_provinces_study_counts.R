# ------------------------------------------------------------------------------
# Script: Map number of climate-refugia studies by MEOW provinces
# Author: Isaac Brito-Morales (ibrito@conservation.org)
# Date: 2026-01-09
#
# Purpose
#   - Read the ClimRef reference dataset (CSV) and count how many studies are
#     associated with each marine province (free-text “Province” field).
#   - Join those counts to a MEOW (Marine Ecoregions of the World) spatial layer.
#   - Produce two exploratory figures:
#       (1) Categorical map of provinces with study counts labeled on top
#       (2) Heatmap (continuous) showing number of studies per province
#
# Notes / assumptions
#   - The ClimRef “Province” column is messy (quotes, slashes, multiple entries),
#     so we standardize text, split multiple provinces, and drop ambiguous values.
#   - The MEOW layer used here is an sf object saved as RDS and contains geometry.
#   - Mapping is done in Robinson projection, with land + an outline for context.
#
# Caveat Emptor
#   - No guarantees that this code is correct. Please double-check outputs.
#   - Please do not distribute without permission.
# ------------------------------------------------------------------------------

source("R/utils_helpers.R")
source("R/load_packages.R")
source("R/earth_outline_robinson.R")

# ------------------------------------------------------------------------------
# 1) Read reference dataset and create a stable paper_id
# ------------------------------------------------------------------------------
df <- read_csv("data-raw/reference_data/Climref_dataset_final_20260109.csv")

dff <- df %>%
  dplyr::mutate(paper_id = dplyr::row_number())

# ------------------------------------------------------------------------------
# 2) Clean and split the free-text Province column, then count studies per province
#
# What happens here:
#   - Keep paper_id + Province
#   - Standardize province text (lowercase, remove quotes/backslashes, squish)
#   - Split multiple entries within a cell (separators: ";" or ",")
#   - Drop empty / NA / "multiple" entries
#   - Count how many studies mention each province
# ------------------------------------------------------------------------------
dff_prov <- dff %>%
  dplyr::select(paper_id, Province) %>%
  dplyr::mutate(
    province = Province |>
      stringr::str_to_lower() |>
      stringr::str_replace_all('"', "") |>
      stringr::str_replace_all("\\\\", "") |>
      stringr::str_squish()
  ) %>%
  tidyr::separate_rows(province, sep = ";|,") %>%
  dplyr::mutate(province = stringr::str_squish(province)) %>%
  dplyr::filter(!is.na(province), province != "", province != "multiple") %>%
  dplyr::arrange(paper_id) %>%
  dplyr::select(paper_id, province) %>%
  dplyr::count(province, name = "n_studies") %>%
  dplyr::arrange(dplyr::desc(n_studies))

# ------------------------------------------------------------------------------
# 3) Standardize province names into a join key
#
# Why:
#   - The MEOW layer has province names in a different format than the reference
#     dataset. This "province_key" tries to make matching more robust.
# ------------------------------------------------------------------------------
dff_std <- dff_prov %>%
  dplyr::mutate(province_key = stringr::str_squish(stringr::str_to_lower(province))) %>%
  dplyr::group_by(province_key) %>%
  dplyr::summarise(n_studies = sum(n_studies), .groups = "drop")

# ------------------------------------------------------------------------------
# 4) Read MEOW spatial layer (sf) and prep join keys
#
# File:
#   outputs/boundaries/meow_v01.RDS
# Expected fields:
#   - ECOREGION, PROVINCE, geometry
# ------------------------------------------------------------------------------
stDF <- readRDS("outputs/boundaries/meow_v01.RDS") %>%
  dplyr::select(ECOREGION, PROVINCE) %>%
  dplyr::mutate(
    ECOREGION = stringr::str_squish(stringr::str_to_lower(ECOREGION)),
    PROVINCE  = stringr::str_squish(stringr::str_to_lower(PROVINCE))
  )

# ------------------------------------------------------------------------------
# 5) Join study counts to MEOW provinces
#
# Result:
#   - prov_sf2: sf layer with n_studies per province (0 if missing)
# ------------------------------------------------------------------------------
prov_sf2 <- stDF %>%
  dplyr::mutate(province_key = stringr::str_squish(stringr::str_to_lower(PROVINCE))) %>%
  dplyr::left_join(dff_std, by = "province_key") %>%
  dplyr::mutate(n_studies = tidyr::replace_na(n_studies, 0))

# ------------------------------------------------------------------------------
# 6) Collapse geometry to one row per province for plotting
#
# Why:
#   - MEOW often has multiple polygons per province (multiple ecoregions).
#   - For the plots we want a province-level map, so we summarise by PROVINCE.
#
# Note:
#   - We use max(n_studies) because after the join, all features within a province
#     should already carry the same n_studies value. max is a safe reducer.
# ------------------------------------------------------------------------------
prov_lab <- prov_sf2 %>%
  dplyr::group_by(PROVINCE) %>%
  dplyr::summarise(
    n_studies = max(n_studies, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 7) Label placement points
#
# Why:
#   - For option 1 we want to annotate each province with its study count.
#   - st_point_on_surface() gives a point guaranteed to fall inside each polygon.
# ------------------------------------------------------------------------------
lab_pts <- prov_lab %>%
  dplyr::mutate(geometry = sf::st_point_on_surface(geometry))

# ------------------------------------------------------------------------------
# 8) Basemap layers + projection helpers
# ------------------------------------------------------------------------------
land <- get_world_latlon()           # land polygons in lon/lat (WGS84)
earth_outline <- earth_outline_robinson()

# ------------------------------------------------------------------------------
# 9) Plot option 1: categorical provinces + numeric labels (n_studies)
# ------------------------------------------------------------------------------
theme_map_with_guides <- function() {
  list(
    ggplot2::theme_void() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        
        axis.text  = ggplot2::element_text(color = "grey30", size = 9),
        axis.title = ggplot2::element_blank(),
        
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 10),
        legend.text  = ggplot2::element_text(size = 12),
        
        legend.box = "horizontal",
        legend.direction = "horizontal",
        
        legend.spacing.x = grid::unit(8, "pt"),
        legend.spacing.y = grid::unit(6, "pt"),
        
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 120, l = 10)
      ),
    
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "top",
        ncol = 6,
        byrow = TRUE,
        keywidth = grid::unit(12, "pt"),
        keyheight = grid::unit(10, "pt")
      )
    )
  )
}

p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = prov_lab,
    ggplot2::aes(fill = PROVINCE),
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf(
    data = land,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf_text(
    data = lab_pts,
    ggplot2::aes(label = n_studies),
    size = 5,
    color = "black"
  ) +
  ggplot2::geom_sf(
    data = earth_outline,
    color = "grey50",
    linewidth = 1.0,
    inherit.aes = FALSE
  ) +
  ggplot2::coord_sf(
    crs = robin,
    default_crs = sf::st_crs(4326),
    expand = FALSE
  ) +
  theme_map_with_guides()

ggplot2::ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v02.pdf",
  plot = p1, dpi = 400, width = 21, height = 15
)

# ------------------------------------------------------------------------------
# 10) Plot option 2: heatmap (continuous fill = n_studies), no numeric labels
# ------------------------------------------------------------------------------
theme_map_with_guides_v02 <- function() {
  list(
    ggplot2::theme_void() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        
        axis.text  = ggplot2::element_text(color = "grey30", size = 9),
        axis.title = ggplot2::element_blank(),
        
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text  = ggplot2::element_text(size = 14),
        
        legend.box = "vertical",
        legend.direction = "vertical",
        
        legend.spacing.y = grid::unit(8, "pt"),
        
        plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
      ),
    
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        barheight = grid::unit(200, "pt"),
        barwidth  = grid::unit(20, "pt"),
        ticks = TRUE
      )
    )
  )
}

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = prov_lab,
    ggplot2::aes(fill = n_studies),
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_gradientn(
    colours = c("grey", RColorBrewer::brewer.pal(9, "YlOrRd")),
    limits  = c(0, 15),
    breaks  = seq(0, 15, by = 3),
    oob     = scales::squish,
    na.value = "grey90",
    name = "Number\nof studies"
  ) +
  ggplot2::geom_sf(
    data = land,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_sf(
    data = earth_outline,
    color = "grey50",
    linewidth = 1.0,
    inherit.aes = FALSE
  ) +
  ggplot2::coord_sf(
    crs = robin,
    default_crs = sf::st_crs(4326),
    expand = FALSE
  ) +
  theme_map_with_guides_v02()

ggplot2::ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v03.pdf",
  plot = p2, dpi = 400, width = 25, height = 15
)

# ------------------------------------------------------------------------------
# 11) Handling studies without provincial attribution (global scale)
# ------------------------------------------------------------------------------
# Theme for option 3 plot
# Same visual language as option 2, but we avoid panel.background shading
# because that paints the whole plotting panel. Instead, we fill the ocean
# ONLY inside the Earth outline (see p3 below), which is the clean cue for
# "global studies in high seas" (n = 5).

theme_map_with_guides_v03 <- function() {
  list(
    theme_void() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        
        panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        axis.text  = element_text(color = "grey30", size = 9),
        axis.title = element_blank(),
        
        legend.position = "right",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text  = element_text(size = 14),
        
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = unit(8, "pt"),
        
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        
        plot.caption = element_text(
          size = 14,
          color = "grey20",
          hjust = 1,
          margin = margin(t = 10)
        )
      ),
    guides(
      fill = guide_colorbar(
        title.position = "top",
        barheight = unit(200, "pt"),
        barwidth  = unit(20, "pt"),
        ticks = TRUE
      )
    )
  )
}


# Option 3 plot
# Same as option 2, but with a pale ocean fill constrained to the Earth outline
# to represent global (non-province) studies (n = 5).


# --- turn the earth_outline LINESTRING into a fillable POLYGON ---
coords <- sf::st_coordinates(earth_outline)
# keep only XY
xy <- coords[, c("X","Y"), drop = FALSE]
# force closure (polygon needs first point == last point)
if (!all(xy[1, ] == xy[nrow(xy), ])) {
  xy <- rbind(xy, xy[1, ])
}
earth_poly <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_polygon(list(xy)), crs = sf::st_crs(earth_outline))
)
# quick sanity checks
sf::st_geometry_type(earth_poly)
plot(sf::st_geometry(earth_poly), col = "grey95", border = "grey20")


p3 <- ggplot() +
  # Pale ocean background constrained to the Earth shape (high seas cue)
  geom_sf(
    data = earth_poly, 
    fill = "#d9d9d9", 
    color = "black") +
  # Provinces (continuous fill = number of studies)
  geom_sf(
    data = prov_lab,
    aes(fill = n_studies),
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_gradientn(
    colours  = c("white", RColorBrewer::brewer.pal(9, "Blues")), # BuGn # Blues
    limits   = c(0, 14),
    breaks   = seq(0, 14, by = 2),
    oob      = scales::squish,
    na.value = "white",
    name     = "Number\nof studies"
  ) +
  # Land
  geom_sf(
    data = land,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  coord_sf(
    crs = robin,
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  labs(
    caption = "Grey ocean areas indicate high seas coverage, representing global studies not assigned to provinces (n = 5)"
  ) +
  theme_map_with_guides_v03()

ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v04c_global_studies.pdf",
  plot = p3, dpi = 400, width = 25, height = 15
)
