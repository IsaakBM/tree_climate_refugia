source("R/plot_meow_global_study_heatmap_wgs84.R")
source("R/plot_meow_global_study_heatmap_robinson.R")

# ------------------------------------------------------------------------------
# 0) Minimum fix: normalize province keys so CSV matches MEOW
# ------------------------------------------------------------------------------
# Fixes:
# - "&" vs "and" (e.g., "red sea & gulf of aden")
# - accents (fernández -> fernandez)
# - en-dash/em-dash -> "-" (amsterdam–st paul)
# - keep everything lowercase + squished
norm_prov_key <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("&", "and") |>
    stringr::str_replace_all("–|—", "-") |>
    stringi::stri_trans_general("Latin-ASCII")
}

# ------------------------------------------------------------------------------
# 1) Read reference dataset and create a stable paper_id
# ------------------------------------------------------------------------------
df <- readr::read_csv("data-raw/reference_data/Climref_dataset_final_20260115_v2.csv")

dff <- df %>%
  dplyr::mutate(paper_id = dplyr::row_number())

# ------------------------------------------------------------------------------
# 2) Clean + split Province column, then count studies per province
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
  
  # ---------------------------------------------------------------------------
# MIN FIX: recode the two remaining mismatches reported by the team
# - MEOW combines: "marshall" + "gilbert & ellis islands" -> one province name
# - MEOW uses space (not dash): "tristan gough"
# ---------------------------------------------------------------------------
dplyr::mutate(
  province = dplyr::case_when(
    province %in% c("marshall", "gilbert & ellis islands", "gilbert and ellis islands") ~
      "marshall, gilbert and ellis islands",
    province %in% c("tristan–gough", "tristan-gough") ~
      "tristan gough",
    TRUE ~ province
  )
) %>%
  
  dplyr::arrange(paper_id) %>%
  dplyr::select(paper_id, province) %>%
  dplyr::count(province, name = "n_studies") %>%
  dplyr::arrange(dplyr::desc(n_studies))

# ------------------------------------------------------------------------------
# 3) Standardize province names into a join key (MIN FIX applied here)
# ------------------------------------------------------------------------------
dff_std <- dff_prov %>%
  dplyr::mutate(province_key = norm_prov_key(province)) %>%
  dplyr::group_by(province_key) %>%
  dplyr::summarise(n_studies = sum(n_studies), .groups = "drop")

# ------------------------------------------------------------------------------
# 4) Read MEOW spatial layer (sf) and prep join keys (MIN FIX applied here)
# ------------------------------------------------------------------------------
stDF <- readRDS("outputs/boundaries/meow_v01.RDS") %>%
  dplyr::select(ECOREGION, PROVINCE) %>%
  dplyr::mutate(
    ECOREGION = norm_prov_key(ECOREGION),
    PROVINCE  = norm_prov_key(PROVINCE)
  )

# ------------------------------------------------------------------------------
# 5) Join study counts to MEOW provinces
# ------------------------------------------------------------------------------
prov_sf2 <- stDF %>%
  dplyr::mutate(province_key = PROVINCE) %>%  # already normalized above
  dplyr::left_join(dff_std, by = "province_key") %>%
  dplyr::mutate(n_studies = tidyr::replace_na(n_studies, 0))

# ------------------------------------------------------------------------------
# 6) Collapse geometry to one row per province for plotting
# ------------------------------------------------------------------------------
prov_lab <- prov_sf2 %>%
  dplyr::group_by(PROVINCE) %>%
  dplyr::summarise(
    n_studies = max(n_studies, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 7) Make plots (WGS84 + Robinson) for a few palettes
# ------------------------------------------------------------------------------
plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "Blues")
plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "BuGn")
plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "GnBu")
plot_meow_global_study_heatmap_wgs84(prov_lab, palette = "YlOrRd")

plot_meow_global_study_heatmap_robinson(prov_lab, palette = "Blues")
plot_meow_global_study_heatmap_robinson(prov_lab, palette = "BuGn")
plot_meow_global_study_heatmap_robinson(prov_lab, palette = "GnBu")
plot_meow_global_study_heatmap_robinson(prov_lab, palette = "YlOrRd")