source("R/utils_helpers.R")
source("R/load_packages.R")

df <- read_csv("data-raw/reference_data/Climref_dataset_final.csv")
dff <- df %>% 
  dplyr::mutate(paper_id = row_number())

dff_realm <- dff %>%
  dplyr::select(paper_id, Realm) %>%
  dplyr::mutate(realm = str_squish(str_to_lower(Realm))) %>%
  tidyr::separate_rows(realm, sep = ";|,") %>%
  dplyr::mutate(realm = str_squish(realm)) %>%
  dplyr::filter(!is.na(realm), realm != "", realm != "multiple") %>% 
  dplyr::arrange(paper_id) %>% 
  dplyr::select(paper_id, realm, Realm)

dff_prov <- dff %>%
  dplyr::select(paper_id, Province) %>%
  dplyr::mutate(province = str_squish(str_to_lower(Province))) %>%
  tidyr::separate_rows(province, sep = ";|,") %>%
  dplyr::mutate(province = str_squish(province)) %>%
  dplyr::filter(!is.na(province), province != "", province != "multiple") %>% 
  dplyr::arrange(paper_id) %>% 
  dplyr::select(paper_id, province, Province)

paper_geo_long <- dplyr:::bind_rows(
  dff_realm   %>% transmute(paper_id, level = "realm", value = realm),
  dff_prov %>% transmute(paper_id, level = "province", value = province)
) %>%
  arrange(paper_id, level, value)

nrow(dff_prov[dff_prov$province == "gulf of guinea",])
sum(dff_prov$province == "gulf of guinea")
