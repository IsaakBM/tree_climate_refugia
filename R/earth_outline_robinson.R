# ============================
# Build Robinson “earth outline”
# Author: Isaac Brito-Morales
# Email: ibrito@conservation.org
# ============================

source("R/utils_helpers.R")
source("R/load_packages.R")

earth_outline_robinson <- function(crs_out = robin, 
                                   lon_step = 0.5, 
                                   lat_step = 0.5) {
  
  # Construct a densified lon/lat ring around global extent
  lon <- seq(-180, 180, by = lon_step)
  lat <- seq(-90,  90,  by = lat_step)
  
  top    <- cbind(lon,  90)
  bottom <- cbind(rev(lon), -90)
  left   <- cbind(rep(-180, length(lat)), lat)
  right  <- cbind(rep( 180, length(lat)), rev(lat))
  
  ring <- rbind(top, right, bottom, left, top)
  
  earth_outline <- sf::st_sfc(
    sf::st_linestring(ring),
    crs = 4326
  ) |>
    sf::st_transform(crs_out)
  
  return(earth_outline)
}