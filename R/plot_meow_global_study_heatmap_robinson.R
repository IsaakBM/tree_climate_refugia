# ------------------------------------------------------------------------------
# Script: plot_meow_global_study_heatmap_robinson.R
# Author: Isaac Brito-Morales (ibrito@conservation.org)
# Date: 2026-01-15
#
# Purpose
#   - Standalone plotting utility to generate a Robinson-projected heatmap of
#     climate-refugia study counts by MEOW provinces.
#   - Provide a robust "global/high seas" cue by filling the Earth outline as a
#     polygon (earth_poly) and plotting it FIRST (grey fill + black outline).
#   - Palette choice is an explicit argument (RColorBrewer) and is encoded in the
#     output filename.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 01) Project sources
# ------------------------------------------------------------------------------
source("R/utils_helpers.R")
source("R/load_packages.R")
source("R/earth_outline_robinson.R")


# ------------------------------------------------------------------------------
# 02) Theme (internal default, tuned for Robinson maps too)
# ------------------------------------------------------------------------------
default_theme_map_robinson <- function() {
  list(
    ggplot2::theme_void() +
      ggplot2::theme(
        # panel background must stay white (outside the globe)
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA),
        
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        
        panel.border = ggplot2::element_blank(),
        
        axis.text  = ggplot2::element_blank(),
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
# 03) Main plotting function (Robinson)
# ------------------------------------------------------------------------------
plot_meow_global_study_heatmap_robinson <- function(
    prov_lab,
    palette = "Blues",
    out_dir = "outputs/figures/exploratory",
    out_prefix = "meow_ecoreg_global_studies_robinson",
    width = 25,
    height = 15,
    dpi = 400,
    theme_fn = default_theme_map_robinson
) {
  
  # ---------------------------------------------------------------------------
  # 03a) Basic checks
  # ---------------------------------------------------------------------------
  if (missing(prov_lab)) {
    stop("[plot_meow_global_study_heatmap_robinson] You must supply `prov_lab` (sf with n_studies).")
  }
  if (!inherits(prov_lab, "sf")) {
    stop("[plot_meow_global_study_heatmap_robinson] `prov_lab` must be an sf object.")
  }
  if (!"n_studies" %in% names(prov_lab)) {
    stop("[plot_meow_global_study_heatmap_robinson] `prov_lab` must contain a column named `n_studies`.")
  }
  
  # ---------------------------------------------------------------------------
  # 03b) Validate palette
  # ---------------------------------------------------------------------------
  pal_info <- RColorBrewer::brewer.pal.info
  
  if (!palette %in% rownames(pal_info)) {
    stop("[plot_meow_global_study_heatmap_robinson] Unknown RColorBrewer palette: '", palette, "'.")
  }
  if (pal_info[palette, "maxcolors"] < 9) {
    stop("[plot_meow_global_study_heatmap_robinson] Palette '", palette, "' supports only ",
         pal_info[palette, "maxcolors"], " colours (9 required).")
  }
  
  # ---------------------------------------------------------------------------
  # 03c) Basemap layers + Robinson helpers
  # ---------------------------------------------------------------------------
  land <- get_world_latlon()
  land_ll <- sf::st_transform(land, 4326)
  
  earth_outline <- earth_outline_robinson()
  
  # --- turn the earth_outline LINESTRING into a fillable POLYGON ---
  coords <- sf::st_coordinates(earth_outline)
  
  # keep only XY (defensive: drop Z/M if present)
  xy <- coords[, 1:2, drop = FALSE]
  
  # force closure (polygon needs first point == last point)
  if (!all(xy[1, ] == xy[nrow(xy), ])) {
    xy <- rbind(xy, xy[1, ])
  }
  
  earth_poly <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(xy)), crs = sf::st_crs(earth_outline))
  )
  
  # optional safety (prevents weird slivers if any self-intersection exists)
  earth_poly <- sf::st_make_valid(earth_poly)
  
  # Provinces in WGS84 (coord_sf will reproject to Robinson)
  prov_lab_ll <- sf::st_transform(prov_lab, 4326)
  
  # ---------------------------------------------------------------------------
  # 03d) Output path encodes palette choice
  # ---------------------------------------------------------------------------
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  palette_tag <- paste0("pal-", tolower(palette))
  out_file <- file.path(out_dir, paste0(out_prefix, "_", palette_tag, ".pdf"))
  
  # ---------------------------------------------------------------------------
  # 03e) Build plot (Robinson)
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot() +
    
    # Earth polygon FIRST: grey fill + black outline (high seas cue + frame)
    ggplot2::geom_sf(
      data = earth_poly,
      fill = "#d9d9d9",
      color = "black"
    ) +
    
    # Provinces heatmap
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
    
    # Land on top
    ggplot2::geom_sf(
      data = land_ll,
      fill = "grey20",
      color = "grey30",
      linewidth = 0.2,
      inherit.aes = FALSE
    ) +
    
    # Robinson projection
    ggplot2::coord_sf(
      crs = robin,
      default_crs = sf::st_crs(4326),
      expand = FALSE
    ) +
    
    ggplot2::labs(
      caption = "Grey ocean areas indicates studies in the high seas (n = 20)"
    ) +
    theme_fn()
  
  # ---------------------------------------------------------------------------
  # 03f) Save
  # ---------------------------------------------------------------------------
  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    dpi = dpi,
    width = width,
    height = height
  )
  
  message("[plot_meow_global_study_heatmap_robinson] Saved: ", out_file)
  invisible(p)
}