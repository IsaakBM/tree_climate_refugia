source("R/utils_helpers.R")
source("R/load_packages.R")

df <- read_csv("data-raw/reference_data/Climref_dataset_final.csv")
dff <- df %>% 
  dplyr::mutate(paper_id = row_number())

dff_realm <- dff %>%
  dplyr::select(paper_id, Realm) %>%
  dplyr::mutate(
    realm = Realm |>
      stringr::str_to_lower() |>
      stringr::str_replace_all('"', "") |>
      stringr::str_replace_all("\\\\", "") |>
      stringr::str_squish()
  ) %>%
  tidyr::separate_rows(realm, sep = ";|,") %>%
  dplyr::mutate(realm = stringr::str_squish(realm)) %>%
  dplyr::filter(!is.na(realm), realm != "", realm != "multiple") %>% 
  dplyr::arrange(paper_id) %>% 
  dplyr::select(paper_id, realm) %>% 
  dplyr::count(realm, name = "n_studies") %>%
  dplyr::arrange(dplyr::desc(n_studies))

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

# paper_geo_long <- dplyr:::bind_rows(
#   dff_realm   %>% transmute(paper_id, level = "realm", value = realm),
#   dff_prov %>% transmute(paper_id, level = "province", value = province)
# ) %>%
#   arrange(paper_id, level, value)

nrow(dff_prov[dff_prov$province == "gulf of guinea",])
sum(dff_prov$province == "gulf of guinea")

sort(unique(dff_realm$realm))
sort(unique(dff_prov$province))


stDF <- readRDS("outputs/boundaries/meow_v01.RDS") %>% 
  dplyr::select(ECOREGION, REALM, PROVINCE) %>% 
  dplyr::mutate(ECOREGION = str_squish(str_to_lower(ECOREGION))) %>% 
  dplyr::mutate(REALM = str_squish(str_to_lower(REALM))) %>% 
  dplyr::mutate(PROVINCE = str_squish(str_to_lower(PROVINCE)))

sort(unique(stDF$PROVINCE))
sort(unique(stDF$REALM))


asdf <- dff_prov %>%   # this is your table with paper_id + province
  mutate(province_key = str_squish(str_to_lower(province))) %>%
  count(province_key, name = "n_studies")

prov_sf2 <- stDF %>%   # your sf with ECOREGION, REALM, PROVINCE, geometry
  mutate(province_key = str_squish(str_to_lower(PROVINCE))) %>%
  left_join(asdf, by = "province_key") %>%
  mutate(n_studies = tidyr::replace_na(n_studies, 0))

