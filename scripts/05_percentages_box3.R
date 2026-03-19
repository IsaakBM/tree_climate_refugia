# Percentages that go into Box 3 for Marina
# Jessica Bolin


# Dependencies ------------------------------------------------------------


library(tidyverse)

dat <- read_rds("outputs/box3/out/final_cleaned_climref_summarystats_NOLOCATIONINFO.rds")
df <- dat[[1]]
species <- dat[[2]] %>% as.tibble()
depth <- dat[[3]] %>% as.tibble()


# Species -----------------------------------------------------------------

species_long <- species %>%
  pivot_longer(
    cols = everything(),
    names_to = "Species",
    values_to = "n"
  ) %>% 
  mutate(percent_label = n / sum(n)* 100) 

species_long$percent_label%>% sum #check 

write.csv(species_long, "outputs/box3/final_species.csv")


# Depth -------------------------------------------------------------------


depth_long <- depth %>%
  pivot_longer(
    cols = everything(),
    names_to = "depth classes",
    values_to = "n"
  ) %>% 
  mutate(percent_label = n / sum(n) * 100) 

depth_long
depth_long$percent_label%>% sum #check 
write.csv(depth_long, "outputs/box3/final_depth.csv")


# All other panels ---------------------------------------------------------------------

bar_df <- df %>%
  select(
    Scale,
    EEZ,
    Time,
    Type_data,
    Stressor,
    Final_refugia_type
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  count(Variable, Category)
bar_df$Variable %>% unique

# EEZ
eez <- bar_df %>% filter(Variable == "EEZ")  %>% 
  mutate(percent_label = n / sum(n) * 100) 
write.csv(eez, "outputs/box3/final_eez.csv")


# Final refugia type 
type <- bar_df %>% filter(Variable == "Final_refugia_type")  %>% 
  mutate(percent_label = n / sum(n) * 100) 
write.csv(type, "outputs/box3/final_type.csv")

# Scale
scale <- bar_df %>% filter(Variable == "Scale")  %>% 
  mutate(percent_label = n / sum(n) * 100) 
write.csv(scale, "outputs/box3/final_scale.csv")

# Stressor
stressor <- bar_df %>% filter(Variable == "Stressor")  %>% 
  mutate(percent_label = n / sum(n) * 100) 
write.csv(stressor, "outputs/box3/final_stressor.csv")

# Time
time <- bar_df %>% filter(Variable == "Time")  %>% 
  mutate(percent_label = n / sum(n) * 100) 
write.csv(time, "outputs/box3/final_time.csv")

# Type_data
typedata <- bar_df %>% filter(Variable == "Type_data")  %>% 
  mutate(percent_label = n / sum(n) * 100) 
write.csv(typedata, "outputs/box3/final_typedata.csv")

