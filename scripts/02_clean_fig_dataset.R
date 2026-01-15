# Clean  PROVOSIONAL dataset ready for making summary figures
# Jessica Bolin Dec '25


# Dependencies ------------------------------------------------------------

library(tidyverse)

#dat <- read.csv("data-raw/Provisional_climref_dataset_for_figures.csv")
#dat <- read.csv("data-raw/reference_data/Climref_dataset_final_20260109.csv")
dat <- read.csv("data-raw/reference_data/Climref_dataset_final_20260115.csv")

# Update to final dataset pending Mikaela


# Cleaning ----------------------------------------------------------------

head(dat)
# Remove empty cols
#dat2 <- dat[, -grep("X.", colnames(dat))]
#dat2$X <- NULL

# Remove uncessary cols
dat$Inclusion <- NULL
dat$Title <- NULL

head(dat)
dat$Authors
# Remove empty rows
dat2 <- dat[1:grep("Nur ", dat$Authors),] #Nur et al is final paper in list

# Change author col to first name only
dat2$ID <- str_extract(dat2$Authors, "^[^[:space:];,\\.]+")
dat2$Authors <- NULL

# Change depth vals to codes 
head(dat2)

dat4 <- dat2 %>%
  mutate(
    Depth_code = Depth %>%
      str_replace_all("\n", " ") %>%
      str_replace_all("\"", "") %>%
      str_replace_all("Shallow \\(0-50m\\)", "SHAL") %>%
      str_replace_all("Epipelagic \\(0-200 m\\)", "EPI") %>%
      str_replace_all("Mesopelagic \\(200–1,000 m\\)", "MESO") %>%
      str_replace_all("Bathyabyssopelagic \\(>1,000 m\\)", "BATHY") %>%
      str_replace_all("Seafloor", "SEAFLOOR") %>%
      str_replace_all("All water column, ", "") %>% 
      str_replace_all(", All water column", "") 
  )
dat4$Depth <- NULL
head(dat4)

# Change variables to factors

dat4$Scale <- as.factor(dat4$Scale)
dat4$EEZ <- as.factor(dat4$EEZ)
dat4$Time <- as.factor(dat4$Time)
dat4$Type_data <- as.factor(dat4$Type_data)
dat4$Stressor <- as.factor(dat4$Stressor)
dat4$Final_refugia_type <- as.factor(dat4$Final_refugia_type)

# Code up final_refugia_type
levels(dat4$Final_refugia_type) <- c("COMBO", "HAB-SUIT", "LOW-EXP", "RESIL")

head(dat4)

# Remove realm, location and province -------------------------------------

#Propose two datasets, remove realm and location from this one for ease and have that in separate dataset 

dat4$Realm <- NULL
dat4$Province <- NULL
dat4$Location_Country <- NULL
# Will do these in another script.
head(dat4)

# Separate species and depth codes into table -----------------------------

# Some species have multiple fields. Separate.

dat4$Species
head(dat4)

species_counts <- dat4 %>%
  separate_rows(Species, sep = ",\\s*") %>%   # split comma-separated species into rows
  count(Species) %>%   # count occurrences per species
  pivot_wider(    names_from  = Species,   # turn species into separate columns
    values_from = n,
    values_fill = 0  )

# Same for depth
depth_counts <- dat4 %>%
  separate_rows(Depth_code, sep = ",\\s*") %>%   # split comma-separated species into rows
  count(Depth_code) %>%   # count occurrences per species
  pivot_wider(    names_from  = Depth_code,   # turn species into separate columns
                  values_from = n,
                  values_fill = 0  )

species_counts
depth_counts
dat4

# Save as list ------------------------------------------------------------

listy <- list()
listy[[1]] <- dat4
listy[[2]] <- species_counts
listy[[3]] <- depth_counts
listy
write_rds(listy, "out/cleaned_climref_summarystats_NOLOCATIONINFO.rds")


# Summary plots -----------------------------------------------------------

barplot(dat4$Scale %>% table)
barplot(dat4$EEZ %>% table)
barplot(dat4$Time %>% table)
barplot(dat4$Type_data %>% table)
barplot(dat4$Stressor %>% table)
barplot(dat4$Final_refugia_type %>% table)

df <- dat4$Species %>% table %>% as.tibble()
species_counts2 <- species_counts %>%  as.data.frame()

species_counts2_long <- species_counts2 |>
  pivot_longer(
    cols = everything(),
    names_to = "species_group",
    values_to = "count"
  )

species_counts2_long

ggplot(species_counts2_long, aes(x = species_group, y = count)) +
  geom_col(width = 1) +
  coord_polar(start = 0) +
  theme_minimal() +
  labs(
    title = "Taxonomic groups represented",
    y = "Number of studies",
    x = NULL
  ) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )
