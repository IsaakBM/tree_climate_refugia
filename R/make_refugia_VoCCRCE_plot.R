# Mollweide CRS
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

# ---------------------------------------------------------
# Helper: ellipse from land bbox in Mollweide
# ---------------------------------------------------------
moll_earth_border <- function(land_sf,
                                             scale_x = 1.02,
                                             scale_y = 1.035,  # <- slightly taller
                                             n = 720) {
  bb <- st_bbox(land_sf)
  
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2
  
  rx <- (bb["xmax"] - bb["xmin"]) / 2 * scale_x
  ry <- (bb["ymax"] - bb["ymin"]) / 2 * scale_y
  
  t <- seq(0, 2 * pi, length.out = n)
  
  coords <- cbind(
    x = cx + rx * cos(t),
    y = cy + ry * sin(t)
  )
  
  coords <- rbind(coords, coords[1, ])  # close ring
  
  st_sfc(st_polygon(list(coords)), crs = st_crs(land_sf))
}

# ---------------------------------------------------------
# Main plotting function
# ---------------------------------------------------------
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
    
    # land mask in Mollweide
    land <- get_world_latlon() |>
      st_transform(crs = moll)
    
    # raster projected to Mollweide
    rs_moll <- terra::project(rs, moll, method = "near")
    
    # ellipse based on land bbox
    earth_border <- moll_earth_border(
      land,
      scale_x = 1.02,
      scale_y = 1.035
    )
    
    # clip raster to ellipse so no pixels go outside
    earth_border_vect <- terra::vect(earth_border)
    rs_moll_clip      <- terra::mask(rs_moll, earth_border_vect)
    
    df <- as.data.frame(rs_moll_clip, xy = TRUE, na.rm = FALSE)
    colnames(df) <- c("x", "y", "val")
    
    df <- df |>
      mutate(val = ifelse(val, "Low25", NA)) |>
      filter(!is.na(val))
    
    p <- ggplot() +
      # 1) Raster (already clipped)
      geom_raster(
        data = df,
        aes(x = x, y = y, fill = val)
      ) +
      # 2) Land
      geom_sf(
        data  = land,
        fill  = "grey20",
        color = "grey30",
        linewidth = 0.2
      ) +
      # 3) Coord system
      coord_sf(crs = moll, expand = FALSE) +
      scale_fill_manual(
        values = c("Low25" = fill_col),
        na.value = NA
      ) +
      # 4) Ellipse on top
      geom_sf(
        data  = earth_border,
        fill  = NA,
        color = "black",
        linewidth = 0.6
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        legend.position  = "none",
        panel.border     = element_blank()
      )
  }
  
  return(p)
}