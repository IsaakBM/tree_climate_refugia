library(terra)
library(sf)
library(ggplot2)
library(dplyr)

# rs   : SpatRaster (logical, TRUE = area of interest)
# proj : "latlon" or "moll"
# fill_col : color for TRUE cells

plot_low25_map <- function(rs, proj = c("latlon", "moll"), fill_col = "steelblue2") {
  proj <- match.arg(proj)
  
  if (proj == "latlon") {
    land <- get_world_latlon()
    
    df <- as.data.frame(rs, xy = TRUE, na.rm = FALSE)
    colnames(df) <- c("lon", "lat", "val")
    df <- df |>
      mutate(val = ifelse(val, "Low25", NA)) |>
      filter(!is.na(val))
    
    p <- ggplot() +
      geom_raster(
        data = df,
        aes(x = lon, y = lat, fill = val)
      ) +
      geom_sf(
        data  = land,
        fill  = "grey20",
        color = "grey30",
        linewidth = 0.2
      ) +
      coord_sf(
        xlim = c(-180, 180),
        ylim = c(-90, 90),
        expand = FALSE
      ) +
      scale_fill_manual(
        values = c("Low25" = fill_col),
        na.value = NA
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid       = element_blank(),
        axis.text        = element_text(color = "grey70"),
        legend.position  = "none",
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6)
      )
    
  } else if (proj == "moll") {
    
    land <- get_world_latlon() |>
      st_transform(crs = moll)
    
    rs_moll <- terra::project(rs, moll, method = "near")
    
    df <- as.data.frame(rs_moll, xy = TRUE, na.rm = FALSE)
    colnames(df) <- c("x", "y", "val")
    
    df <- df |>
      mutate(val = ifelse(val, "Low25", NA)) |>
      filter(!is.na(val))
    
    p <- ggplot() +
      geom_raster(
        data = df,
        aes(x = x, y = y, fill = val)
      ) +
      geom_sf(
        data  = land,
        fill  = "grey20",
        color = "grey30",
        linewidth = 0.2
      ) +
      coord_sf(crs = moll, expand = FALSE) +
      scale_fill_manual(
        values = c("Low25" = fill_col),
        na.value = NA
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.position  = "none",
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6)
      )
  }
  
  return(p)
}