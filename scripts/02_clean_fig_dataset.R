# Clean  PROVOSIONAL dataset ready for making summary figures
# Jessica Bolin Dec '25


# Dependencies ------------------------------------------------------------

library(tidyverse)

dat <- read.csv("data-raw/Provisional_climref_dataset_for_figures.csv")
# Update to final dataset pending Mikaela


# Cleaning ----------------------------------------------------------------

# Remove empty cols
dat2 <- dat[, -grep("X.", colnames(dat))]
dat2$X <- NULL

# Remove uncessary cols
dat2$Inclusion <- NULL
dat2$Title <- NULL

# Remove empty rows
dat3 <- dat2[1:grep("Man,", dat2$Authors),] #Man et al is final paper in list

# Change author col to first name only
dat3$ID <- str_extract(dat3$Authors, "^[^[:space:];,\\.]+")
dat3$Authors <- NULL

# Change depth vals to codes 

dat4 <- dat3 %>%
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

# Change variables to factors

dat4$Scale <- as.factor(dat4$Scale)
dat4$EEZ <- as.factor(dat4$EEZ)
dat4$Time <- as.factor(dat4$Time)
dat4$Type_data <- as.factor(dat4$Type_data)
dat4$Stressor <- as.factor(dat4$Stressor)
dat4$Final_refugia_type <- as.factor(dat4$Final_refugia_type)

# Code up final_refugia_type
levels(dat4$Final_refugia_type) <- c("COMBO", "HAB-SUIT", "LOW-EXP", "RESIL")


# Remove realm, location and province -------------------------------------

#Propose two datasets, remove realm and location from this one for ease and have that in separate dataset 

dat4$Realm <- NULL
dat4$Province <- NULL
dat4$Location_Country <- NULL
# Will do these in another script.


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
write_rds(listy, "out/cleaned_climref_summarystats_NOLOCATIONINFO.rds")


# Summary plots -----------------------------------------------------------

barplot(dat4$Scale %>% table)
barplot(dat4$EEZ %>% table)
barplot(dat4$Time %>% table)
barplot(dat4$Type_data %>% table)
barplot(dat4$Stressor %>% table)
barplot(dat4$Final_refugia_type %>% table)

df <- dat4$Species %>% table %>% as.tibble()

ggplot(df, aes(x = ., y = n)) +
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
