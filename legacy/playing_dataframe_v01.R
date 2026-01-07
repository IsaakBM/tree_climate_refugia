source("R/utils_helpers.R")
source("R/load_packages.R")


# data manipulation -------------------------------------------------------

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
sort(unique(dff_realm$realm))
# [1] "arctic"                               "central indo-pacific"                
# [3] "cold temperate northeast pacific"     "cold temperate northwest atlantic"   
# [5] "eastern indo-pacific"                 "global"                              
# [7] "hawaii"                               "mediterranean sea"                   
# [9] "south china sea"                      "southern ocean"                      
# [11] "temperate australasia"                "temperate northern atlantic"         
# [13] "temperate northern pacific"           "temperate south america"             
# [15] "temperate southern africa"            "tropical atlantic"                   
# [17] "tropical eastern pacific"             "tropical northwestern atlantic"      
# [19] "tropical southwestern atlantic"       "tropical southwestern pacific"       
# [21] "warm temperate northeast pacific"     "warm temperate northwest atlantic"   
# [23] "warm temperate southeastern pacific"  "warm temperate southwestern atlantic"
# [25] "western indian ocean"                 "western indo-pacific"    

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
# sort(unique(dff_prov$province))
# [1] "agulhas"                              "amsterdam–st paul"                   
# [3] "andaman"                              "arctic"                              
# [5] "bay of bengal"                        "benguela"                            
# [7] "black sea"                            "central indian ocean islands"        
# [9] "central polynesia"                    "cold temperate northeast pacific"    
# [11] "cold temperate northwest atlantic"    "cold temperate northwest pacific"    
# [13] "continental high antarctic"           "east central australian shelf"       
# [15] "easter island"                        "eastern coral triangle"              
# [17] "galapagos"                            "gilbert & ellis islands"             
# [19] "global"                               "gulf of guinea"                      
# [21] "hawaii"                               "java transitional"                   
# [23] "juan fernández & desventuradas"       "lord howe & norfolk islands"         
# [25] "lusitanian"                           "magellanic"                          
# [27] "marquesas"                            "marshall"                            
# [29] "mediterranean sea"                    "north brazil shelf"                  
# [31] "northeast australian shelf"           "northern european seas"              
# [33] "northern new zealand"                 "northwest australian shelf"          
# [35] "red sea & gulf of aden"               "sahul shelf"                         
# [37] "scotia sea"                           "somali/arabian"                      
# [39] "south china sea"                      "south kuroshio"                      
# [41] "southeast australian shelf"           "southeast polynesia"                 
# [43] "southern new zealand"                 "southwest australian shelf"          
# [45] "st. helena & ascension islands"       "subantarctic islands"                
# [47] "subantarctic new zealand"             "sunda shelf"                         
# [49] "tristan–gough"                        "tropical east pacific"               
# [51] "tropical northwestern atlantic"       "tropical northwestern pacific"       
# [53] "tropical southwestern atlantic"       "tropical southwestern pacific"       
# [55] "warm temperate northeast pacific"     "warm temperate northwest atlantic"   
# [57] "warm temperate northwest pacific"     "warm temperate southeastern pacific" 
# [59] "warm temperate southwestern atlantic" "west & south indian shelf"           
# [61] "west african transition"              "west central australian shelf"       
# [63] "western coral triangle"               "western indian ocean"   



# paper_geo_long <- dplyr:::bind_rows(
#   dff_realm   %>% transmute(paper_id, level = "realm", value = realm),
#   dff_prov %>% transmute(paper_id, level = "province", value = province)
# ) %>%
#   arrange(paper_id, level, value)
# nrow(dff_prov[dff_prov$province == "gulf of guinea",])
# sum(dff_prov$province == "gulf of guinea")
# 
# sort(unique(dff_realm$realm))
# sort(unique(dff_prov$province))

stDF <- readRDS("outputs/boundaries/meow_v01.RDS") %>% 
  dplyr::select(ECOREGION, REALM, PROVINCE) %>% 
  dplyr::mutate(ECOREGION = str_squish(str_to_lower(ECOREGION))) %>% 
  dplyr::mutate(REALM = str_squish(str_to_lower(REALM))) %>% 
  dplyr::mutate(PROVINCE = str_squish(str_to_lower(PROVINCE)))

# sort(unique(stDF$PROVINCE))
# [1] "agulhas"                              "amsterdam-st paul"                   
# [3] "andaman"                              "arctic"                              
# [5] "bay of bengal"                        "benguela"                            
# [7] "black sea"                            "central indian ocean islands"        
# [9] "central polynesia"                    "cold temperate northeast pacific"    
# [11] "cold temperate northwest atlantic"    "cold temperate northwest pacific"    
# [13] "continental high antarctic"           "east central australian shelf"       
# [15] "easter island"                        "eastern coral triangle"              
# [17] "galapagos"                            "gulf of guinea"                      
# [19] "hawaii"                               "java transitional"                   
# [21] "juan fernandez and desventuradas"     "lord howe and norfolk islands"       
# [23] "lusitanian"                           "magellanic"                          
# [25] "marquesas"                            "marshall, gilbert and ellis islands" 
# [27] "mediterranean sea"                    "north brazil shelf"                  
# [29] "northeast australian shelf"           "northern european seas"              
# [31] "northern new zealand"                 "northwest australian shelf"          
# [33] "red sea and gulf of aden"             "sahul shelf"                         
# [35] "scotia sea"                           "somali/arabian"                      
# [37] "south china sea"                      "south kuroshio"                      
# [39] "southeast australian shelf"           "southeast polynesia"                 
# [41] "southern new zealand"                 "southwest australian shelf"          
# [43] "st. helena and ascension islands"     "subantarctic islands"                
# [45] "subantarctic new zealand"             "sunda shelf"                         
# [47] "tristan gough"                        "tropical east pacific"               
# [49] "tropical northwestern atlantic"       "tropical northwestern pacific"       
# [51] "tropical southwestern atlantic"       "tropical southwestern pacific"       
# [53] "warm temperate northeast pacific"     "warm temperate northwest atlantic"   
# [55] "warm temperate northwest pacific"     "warm temperate southeastern pacific" 
# [57] "warm temperate southwestern atlantic" "west african transition"             
# [59] "west and south indian shelf"          "west central australian shelf"       
# [61] "western coral triangle"               "western indian ocean"   

# sort(unique(stDF$REALM))
# [1] "arctic"                      "central indo-pacific"        "eastern indo-pacific"       
# [4] "southern ocean"              "temperate australasia"       "temperate northern atlantic"
# [7] "temperate northern pacific"  "temperate south america"     "temperate southern africa"  
# [10] "tropical atlantic"           "tropical eastern pacific"    "western indo-pacific"  

sort(unique(stDF$ECOREGION))
# [1] "adriatic sea"                                   "aegean sea"                                    
# [3] "agulhas bank"                                   "alboran sea"                                   
# [5] "aleutian islands"                               "amazonia"                                      
# [7] "amsterdam-st paul"                              "amundsen/bellingshausen sea"                   
# [9] "andaman and nicobar islands"                    "andaman sea coral coast"                       
# [11] "angolan"                                        "antarctic peninsula"                           
# [13] "arabian (persian) gulf"                         "arafura sea"                                   
# [15] "araucanian"                                     "arnhem coast to gulf of carpenteria"           
# [17] "auckland island"                                "azores canaries madeira"                       
# [19] "baffin bay - davis strait"                      "bahamian"                                      
# [21] "baltic sea"                                     "banda sea"                                     
# [23] "bassian"                                        "beaufort sea - continental coast and shelf"    
# [25] "beaufort-amundsen-viscount melville-queen maud" "bermuda"                                       
# [27] "bight of sofala/swamp coast"                    "bismarck sea"                                  
# [29] "black sea"                                      "bonaparte coast"                               
# [31] "bounty and antipodes islands"                   "bouvet island"                                 
# [33] "campbell island"                                "cape howe"                                     
# [35] "cape verde"                                     "cargados carajos/tromelin island"              
# [37] "carolinian"                                     "celtic seas"                                   
# [39] "central and southern great barrier reef"        "central chile"                                 
# [41] "central kuroshio current"                       "central new zealand"                           
# [43] "central peru"                                   "central somali coast"                          
# [45] "chagos"                                         "channels and fjords of southern chile"         
# [47] "chatham island"                                 "chiapas-nicaragua"                             
# [49] "chiloense"                                      "chukchi sea"                                   
# [51] "clipperton"                                     "cocos islands"                                 
# [53] "cocos-keeling/christmas island"                 "coral sea"                                     
# [55] "cortezian"                                      "crozet islands"                                
# [57] "delagoa"                                        "east african coral coast"                      
# [59] "east antarctic dronning maud land"              "east antarctic enderby land"                   
# [61] "east antarctic wilkes land"                     "east caroline islands"                         
# [63] "east china sea"                                 "east greenland shelf"                          
# [65] "east siberian sea"                              "easter island"                                 
# [67] "eastern bering sea"                             "eastern brazil"                                
# [69] "eastern caribbean"                              "eastern galapagos islands"                     
# [71] "eastern india"                                  "eastern philippines"                           
# [73] "exmouth to broome"                              "faroe plateau"                                 
# [75] "fernando de naronha and atoll das rocas"        "fiji islands"                                  
# [77] "floridian"                                      "gilbert/ellis islands"                         
# [79] "great australian bight"                         "greater antilles"                              
# [81] "guayaquil"                                      "guianan"                                       
# [83] "gulf of aden"                                   "gulf of alaska"                                
# [85] "gulf of guinea central"                         "gulf of guinea islands"                        
# [87] "gulf of guinea south"                           "gulf of guinea upwelling"                      
# [89] "gulf of guinea west"                            "gulf of maine/bay of fundy"                    
# [91] "gulf of oman"                                   "gulf of papua"                                 
# [93] "gulf of st. lawrence - eastern scotian shelf"   "gulf of thailand"                              
# [95] "gulf of tonkin"                                 "halmahera"                                     
# [97] "hawaii"                                         "heard and macdonald islands"                   
# [99] "high arctic archipelago"                        "houtman"                                       
# [101] "hudson complex"                                 "humboldtian"                                   
# [103] "ionian sea"                                     "juan fernandez and desventuradas"              
# [105] "kamchatka shelf and coast"                      "kara sea"                                      
# [107] "kerguelen islands"                              "kermadec island"                               
# [109] "lancaster sound"                                "laptev sea"                                    
# [111] "leeuwin"                                        "lesser sunda"                                  
# [113] "levantine sea"                                  "line islands"                                  
# [115] "lord howe and norfolk islands"                  "macquarie island"                              
# [117] "magdalena transition"                           "malacca strait"                                
# [119] "maldives"                                       "malvinas/falklands"                            
# [121] "manning-hawkesbury"                             "mariana islands"                               
# [123] "marquesas"                                      "marshall islands"                              
# [125] "mascarene islands"                              "mexican tropical pacific"                      
# [127] "namaqua"                                        "namib"                                         
# [129] "natal"                                          "new caledonia"                                 
# [131] "nicoya"                                         "ningaloo"                                      
# [133] "north american pacific fijordland"              "north and east barents sea"                    
# [135] "north and east iceland"                         "north greenland"                               
# [137] "north patagonian gulfs"                         "north sea"                                     
# [139] "northeast sulawesi"                             "northeastern brazil"                           
# [141] "northeastern honshu"                            "northeastern new zealand"                      
# [143] "northern and central red sea"                   "northern bay of bengal"                        
# [145] "northern california"                            "northern galapagos islands"                    
# [147] "northern grand banks - southern labrador"       "northern gulf of mexico"                       
# [149] "northern labrador"                              "northern monsoon current coast"                
# [151] "northern norway and finnmark"                   "ogasawara islands"                             
# [153] "oregon, washington, vancouver coast and shelf"  "oyashio current"                               
# [155] "palawan/north borneo"                           "panama bight"                                  
# [157] "papua"                                          "patagonian shelf"                              
# [159] "peter the first island"                         "phoenix/tokelau/northern cook islands"         
# [161] "prince edward islands"                          "puget trough/georgia basin"                    
# [163] "rapa-pitcairn"                                  "revillagigedos"                                
# [165] "rio de la plata"                                "rio grande"                                    
# [167] "ross sea"                                       "saharan upwelling"                             
# [169] "sahelian upwelling"                             "samoa islands"                                 
# [171] "sao pedro and sao paulo islands"                "scotian shelf"                                 
# [173] "sea of japan/east sea"                          "sea of okhotsk"                                
# [175] "seychelles"                                     "shark bay"                                     
# [177] "snares island"                                  "society islands"                               
# [179] "solomon archipelago"                            "solomon sea"                                   
# [181] "south and west iceland"                         "south australian gulfs"                        
# [183] "south china sea oceanic islands"                "south european atlantic shelf"                 
# [185] "south georgia"                                  "south india and sri lanka"                     
# [187] "south kuroshio"                                 "south new zealand"                             
# [189] "south orkney islands"                           "south sandwich islands"                        
# [191] "south shetland islands"                         "southeast madagascar"                          
# [193] "southeast papua new guinea"                     "southeastern brazil"                           
# [195] "southern california bight"                      "southern caribbean"                            
# [197] "southern china"                                 "southern cook/austral islands"                 
# [199] "southern grand banks - south newfoundland"      "southern gulf of mexico"                       
# [201] "southern java"                                  "southern norway"                               
# [203] "southern red sea"                               "southern vietnam"                              
# [205] "southwestern caribbean"                         "st. helena and ascension islands"              
# [207] "sulawesi sea/makassar strait"                   "sunda shelf/java sea"                          
# [209] "three kings-north cape"                         "tonga islands"                                 
# [211] "torres strait northern great barrier reef"      "trindade and martin vaz islands"               
# [213] "tristan gough"                                  "tuamotus"                                      
# [215] "tunisian plateau/gulf of sidra"                 "tweed-moreton"                                 
# [217] "uruguay-buenos aires shelf"                     "vanuatu"                                       
# [219] "virginian"                                      "weddell sea"                                   
# [221] "west caroline islands"                          "west greenland shelf"                          
# [223] "western and northern madagascar"                "western arabian sea"                           
# [225] "western bassian"                                "western caribbean"                             
# [227] "western galapagos islands"                      "western india"                                 
# [229] "western mediterranean"                          "western sumatra"                               
# [231] "white sea"                                      "yellow sea"



# testing plots -----------------------------------------------------------


asdf <- dff_prov %>%
  mutate(province_key = str_squish(str_to_lower(province))) %>%
  group_by(province_key) %>%
  summarise(n_studies = sum(n_studies), .groups = "drop")

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




# testing dissolve by province --------------------------------------------

theme_map_with_guides <- function() {
  list(
    theme_void() +
      theme(
        panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        axis.text  = element_text(color = "grey30", size = 9),
        axis.title = element_blank(),
        
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 12),
        
        legend.box = "horizontal",
        legend.direction = "horizontal",
        
        legend.spacing.x = unit(8, "pt"),
        legend.spacing.y = unit(6, "pt"),
        
        plot.margin = margin(t = 10, r = 10, b = 120, l = 10)
      ),
    
    guides(
      fill = guide_legend(
        title.position = "top",
        ncol = 6,          # ⬅️ fewer columns = taller legend
        byrow = TRUE,
        keywidth = unit(12, "pt"),
        keyheight = unit(10, "pt")
      )
    )
  )
}

prov_lab <- prov_sf2 %>%
  dplyr::group_by(PROVINCE) %>%
  dplyr::summarise(
    n_studies = max(n_studies, na.rm = TRUE),
    .groups = "drop"
  )
lab_pts <- prov_lab %>%
  mutate(geometry = st_point_on_surface(geometry))
earth_outline <- earth_outline_robinson()

p1 <- ggplot() +
  geom_sf(
    data = prov_lab,
    aes(fill = PROVINCE),
    color = "black",
    linewidth = 0.2
  ) +
  geom_sf(
    data = land,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2
  ) +
  geom_sf_text(
    data = lab_pts,
    aes(label = n_studies),
    size = 5,
    color = "black"
  ) +
  # Earth outline
  geom_sf(
    data = earth_outline,
    color = "grey50",
    linewidth = 1.0,
    inherit.aes = FALSE
  ) +
  # Robinson projection; default_crs ensures lon/lat tiles are projected correctly
  coord_sf(
    crs = robin,
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  theme_map_with_guides()

ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v02.pdf",
  plot = p1, dpi = 400, width = 21, height = 15
)



theme_map_with_guides_v02 <- function() {
  list(
    theme_void() +
      theme(
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
        
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
      ),
    
    guides(
      fill = guide_colorbar(
        title.position = "top",
        barheight = unit(200, "pt"),  # taller bar
        barwidth  = unit(20, "pt"),   # thicker bar
        ticks = TRUE
      )
    )
  )
}




p2 <- ggplot() +
  # Provinces, continuous fill
  geom_sf(
    data = prov_lab,
    aes(fill = n_studies),
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_gradientn(
    colours = c("grey", RColorBrewer::brewer.pal(9, "YlOrRd")),
    limits  = c(0, 14),
    breaks  = seq(0, 14, by = 2),
    oob     = scales::squish,
    na.value = "grey90",
    name = "Number\nof studies"
  ) +
  # Land
  geom_sf(
    data = land,
    fill = "grey20",
    color = "grey30",
    linewidth = 0.2,
    inherit.aes = FALSE
  ) +
  # Labels
  # geom_sf_text(
  #   data = lab_pts,
  #   aes(label = n_studies),
  #   size = 5,
  #   color = "black",
  #   inherit.aes = FALSE
  # ) +
  # Earth outline
  geom_sf(
    data = earth_outline,
    color = "grey50",
    linewidth = 1.0,
    inherit.aes = FALSE
  ) +
  coord_sf(
    crs = robin,
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  theme_map_with_guides_v02()

ggsave(
  filename = "outputs/figures/exploratory/meow_ecoreg_v03.pdf",
  plot = p2, dpi = 400, width = 25, height = 15
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
