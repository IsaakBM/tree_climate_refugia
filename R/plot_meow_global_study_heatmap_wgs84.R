# ------------------------------------------------------------------------------
# Script: plot_meow_global_study_heatmap_wgs84.R
# Author: Isaac Brito-Morales (ibrito@conservation.org)
# Date: 2026-01-15
#
# Purpose
#   - Standalone plotting utility to generate a WGS84 (EPSG:4326) heatmap of
#     climate-refugia study counts by MEOW provinces, including a pale ocean cue
#     for global (non-province) studies.
#   - Palette choice is an explicit argument (RColorBrewer) and is encoded in
#     the output filename.
#
# Dependencies (project)
#   - R/utils_helpers.R    (expects get_world_latlon() to be defined here)
#   - R/load_packages.R    (your preferred package loading pattern)
#
# Required input object (created upstream)
#   - prov_lab : sf object with province geometry and n_studies
#
# Output
#   - PDF written to outputs/figures/exploratory/ by default
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 01) Project sources (so get_world_latlon() is available)
# ------------------------------------------------------------------------------
source("R/utils_helpers.R")
source("R/load_packages.R")

# Optional: keep pattern consistent with other scripts
# (Not used for WGS84 plotting, so safe to omit)
# source("R/earth_outline_robinson.R")


# ------------------------------------------------------------------------------
# 02) Theme (internal default)
# ------------------------------------------------------------------------------
default_theme_map_wgs84 <- function() {
  list(
    ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "grey92", color = NA),
        plot.background  = ggplot2::element_rect(fill = "white",  color = NA),
        
        panel.grid.major = ggplot2::element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = ggplot2::element_blank(),
        
        panel.border = ggplot2::element_rect(fill = NA, color = "grey40", linewidth = 0.6),
        
        axis.text  = ggplot2::element_text(color = "grey30", size = 9),
        axis.title = ggplot2::element_blank(),
        
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text  = ggplot2::element_text(size = 14),
        
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.spacing.y = grid::unit(8, "pt"),
        
        plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10),
        
        plot.caption = ggplot2::element_text(
          size = 14,
          color = "grey20",
          hjust = 1,
          margin = ggplot2::margin(t = 10)
        )
      ),
    
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        barheight = grid::unit(200, "pt"),
        barwidth  = grid::unit(20, "pt"),
        ticks = TRUE
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 03) Main plotting function
# ------------------------------------------------------------------------------
plot_meow_global_study_heatmap_wgs84 <- function(
    prov_lab,
    palette = "Blues",
    out_dir = "outputs/figures/exploratory",
    out_prefix = "meow_ecoreg_global_studies_wgs84",
    width = 25,
    height = 15,
    dpi = 400,
    theme_fn = default_theme_map_wgs84
) {
  
  # ---------------------------------------------------------------------------
  # 03a) Basic checks
  # ---------------------------------------------------------------------------
  if (missing(prov_lab)) {
    stop("[plot_meow_global_study_heatmap_wgs84] You must supply `prov_lab` (sf with n_studies).")
  }
  if (!inherits(prov_lab, "sf")) {
    stop("[plot_meow_global_study_heatmap_wgs84] `prov_lab` must be an sf object.")
  }
  if (!"n_studies" %in% names(prov_lab)) {
    stop("[plot_meow_global_study_heatmap_wgs84] `prov_lab` must contain a column named `n_studies`.")
  }
  
  # ---------------------------------------------------------------------------
  # 03b) Validate palette
  # ---------------------------------------------------------------------------
  pal_info <- RColorBrewer::brewer.pal.info
  
  if (!palette %in% rownames(pal_info)) {
    stop("[plot_meow_global_study_heatmap_wgs84] Unknown RColorBrewer palette: '", palette, "'.")
  }
  if (pal_info[palette, "maxcolors"] < 9) {
    stop("[plot_meow_global_study_heatmap_wgs84] Palette '", palette, "' supports only ",
         pal_info[palette, "maxcolors"], " colours (9 required).")
  }
  
  # ---------------------------------------------------------------------------
  # 03c) Build basemap land (WGS84) from project helper
  # ---------------------------------------------------------------------------
  # Rationale:
  # - You asked that land <- get_world_latlon() is included.
  # - We build it inside the function so the plot is reproducible and standalone.
  land <- get_world_latlon()
  land_ll <- sf::st_transform(land, 4326)
  
  # ---------------------------------------------------------------------------
  # 03d) Harmonize CRS to WGS84 and build ocean polygon (Earth bbox minus land)
  # ---------------------------------------------------------------------------
  prov_lab_ll <- sf::st_transform(prov_lab, 4326)
  
  earth_ll <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = sf::st_crs(4326))
  ) |> sf::st_sf(geometry = _)
  
  land_u <- sf::st_union(sf::st_make_valid(land_ll))
  ocean_ll <- sf::st_difference(
    sf::st_make_valid(earth_ll),
    sf::st_make_valid(sf::st_sf(geometry = land_u))
  )
  
  # ---------------------------------------------------------------------------
  # 03e) Output path encodes palette choice
  # ---------------------------------------------------------------------------
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  palette_tag <- paste0("pal-", tolower(palette))
  out_file <- file.path(out_dir, paste0(out_prefix, "_", palette_tag, ".pdf"))
  
  # ---------------------------------------------------------------------------
  # 03f) Build plot
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = earth_ll, fill = "#d9d9d9", color = NA) +
    ggplot2::geom_sf(
      data = prov_lab_ll,
      ggplot2::aes(fill = n_studies),
      color = "black",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_gradientn(
      colours  = c("white", RColorBrewer::brewer.pal(9, palette)),
      limits   = c(0, 14),
      breaks   = seq(0, 14, by = 2),
      oob      = scales::squish,
      na.value = "white",
      name     = "Number\nof studies"
    ) +
    ggplot2::geom_sf(
      data = land_ll,
      fill = "grey20",
      color = "grey30",
      linewidth = 0.2,
      inherit.aes = FALSE
    ) +
    ggplot2::coord_sf(crs = sf::st_crs(4326), expand = FALSE) +
    ggplot2::labs(
      caption = "Grey ocean areas indicates studies in the high seas (n = 20)"
    ) +
    theme_fn()
  
  # ---------------------------------------------------------------------------
  # 03g) Save
  # ---------------------------------------------------------------------------
  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    dpi = dpi,
    width = width,
    height = height
  )
  
  message("[plot_meow_global_study_heatmap_wgs84] Saved: ", out_file)
  invisible(p)
}