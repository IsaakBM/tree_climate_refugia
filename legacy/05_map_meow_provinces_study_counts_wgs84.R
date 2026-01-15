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
# Rationale:
# - land is used as the basemap land layer (in lon/lat by default).
# - earth_outline is used in Robinson plots as a clean outline/frame context.
land <- get_world_latlon()            # land polygons in lon/lat (WGS84)
earth_outline <- earth_outline_robinson()


# ==============================================================================
# NEW SECTIONS NEEDED FOR THE WGS84 VERSION (Option 3)
# ==============================================================================

# ------------------------------------------------------------------------------
# 9) Theme for WGS84 plotting (v04)
# ------------------------------------------------------------------------------
# Rationale:
# - WGS84 maps can look like they "float" without a visible map frame, so we add
#   a panel border.
# - We set a light grey panel background to provide spatial context and make the
#   ocean cue read cleanly, but keep the outer plot background white for export.
# - Subtle graticules help interpret lon/lat (EPSG:4326).
theme_map_with_guides_v04 <- function() {
  list(
    ggplot2::theme_void() +
      ggplot2::theme(
        # Grey map background (inside map frame)
        panel.background = ggplot2::element_rect(
          fill  = "grey92",
          color = NA
        ),
        
        # White outer background (page)
        plot.background = ggplot2::element_rect(
          fill  = "white",
          color = NA
        ),
        
        # Graticules (useful for WGS84)
        panel.grid.major = ggplot2::element_line(
          color = "grey80",
          linewidth = 0.3
        ),
        panel.grid.minor = ggplot2::element_blank(),
        
        # Map frame
        panel.border = ggplot2::element_rect(
          fill  = NA,
          color = "grey40",
          linewidth = 0.6
        ),
        
        axis.text  = ggplot2::element_text(color = "grey30", size = 9),
        axis.title = ggplot2::element_blank(),
        
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text  = ggplot2::element_text(size = 14),
        
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = grid::unit(8, "pt"),
        
        plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10),
        
        plot.caption = ggplot2::element_text(
          size = 14,
          color = "grey20",
          hjust = 1,
          margin = ggplot2::margin(t = 10)
        )
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


# ------------------------------------------------------------------------------
# 10) WGS84 setup: harmonize CRSs for plotting + robust "ocean" layer
# ------------------------------------------------------------------------------
# Rationale:
# - prov_lab is already WGS84 (Geodetic CRS: WGS 84), but we transform explicitly
#   so downstream code is unambiguous.
# - For the "global studies" cue we do NOT shade the entire panel background.
#   Instead we create an explicit ocean polygon:
#        ocean = world bbox (Earth extent) minus land
#   This keeps the cue constrained to the Earth and avoids dateline artifacts
#   that can appear when polygonizing an outline LINESTRING.
prov_lab_ll <- sf::st_transform(prov_lab, 4326)
land_ll     <- sf::st_transform(land, 4326)

# Build Earth extent polygon (world box) in WGS84
earth_ll <- sf::st_as_sfc(
  sf::st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = sf::st_crs(4326))
) |> sf::st_sf(geometry = _)

# Ocean-only polygon = Earth extent minus land (robust for global maps)
land_u   <- sf::st_union(sf::st_make_valid(land_ll))
ocean_ll <- sf::st_difference(
  sf::st_make_valid(earth_ll),
  sf::st_make_valid(sf::st_sf(geometry = land_u))
)


# ------------------------------------------------------------------------------
# 11) Option 3 plot (WGS84): continuous heatmap + ocean cue for global studies
# ------------------------------------------------------------------------------
# Rationale:
# - Layer order:
#   1) ocean grey first (background cue for global/high seas studies)
#   2) provinces with continuous fill (n_studies)
#   3) land on top (dark, for contrast)
# - WGS84 coord_sf gives the lon/lat view requested.
p3_wgs84 <- ggplot2::ggplot() +
  # Pale ocean background constrained to Earth (ocean only)
  ggplot2::geom_sf(data = ocean_ll, fill = "#d9d9d9", color = NA) +
  
  # Provinces (continuous fill = number of studies)
  ggplot2::geom_sf(
    data = prov_lab_ll,
    ggplot2::aes(fill = n_studies),
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_gradientn(
    colours  = c("white", RColorBrewer::brewer.pal(9, "Blues")),
    limits   = c(0, 14),
    breaks   = seq(0, 14, by = 2),
    oob      = scales::squish,
    na.value = "white",
    name     = "Number\nof studies"
  ) +
  
  # Land
  ggplot2::geom_sf(
    data = land_ll,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  
  # WGS84 view (lon/lat)
  ggplot2::coord_sf(crs = sf::st_crs(4326), expand = FALSE) +
  
  ggplot2::labs(
    caption = "Grey ocean areas indicate high seas coverage, representing global studies not assigned to provinces (n = 5)"
  ) +
  theme_map_with_guides_v04()

ggplot2::ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v04c_global_studies_WGS84.pdf",
  plot = p3_wgs84,
  dpi = 400,
  width = 25,
  height = 15
)