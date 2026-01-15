source("R/plot_meow_global_study_heatmap_wgs84.R")
source("R/plot_meow_global_study_heatmap_robinson.R")
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



plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "Blues")
plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "BuGn")
plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "YlOrRd")

plot_meow_global_study_heatmap_robinson(prov_lab, palette = "Blues")
plot_meow_global_study_heatmap_robinson(prov_lab, palette = "BuGn")
plot_meow_global_study_heatmap_robinson(prov_lab, palette = "YlOrRd")
