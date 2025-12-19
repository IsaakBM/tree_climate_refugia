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

land <- get_world_latlon()  # land polygons in lon/lat (WGS84)
p1 <- ggplot() + 
  geom_sf(
    data = prov_sf2,
    aes(fill = PROVINCE),
    color = "black",
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
ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v02.pdf",
  plot = p1, dpi = 400, width = 20, height = 10
)




prov_sf2_lab <- prov_sf2 %>%
  mutate(label_geom = st_point_on_surface(geometry)) %>%
  st_set_geometry("label_geom")
p1 <- ggplot() + 
  geom_sf(
    data = prov_sf2,
    aes(fill = REALM),
    color = "black",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) + 
  geom_sf(
    data = land,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  geom_sf_text(
    data = prov_sf2_lab,
    aes(label = n_studies),
    size = 2.5,
    color = "black"
  )
ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v02.pdf",
  plot = p1, dpi = 400, width = 20, height = 10
)
