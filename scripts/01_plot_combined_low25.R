# Load package setup and helper functions
source("R/load_packages.R")       # loads required R libraries via renv and package manager
source("R/utils_helpers.R")       # utility functions for projection and basemap handling
source("R/make_refugia_VoCCRCE_plot.R")  # custom plotting function for map visualization

# Load VoCC and RCE rasters for SSP245, Epipelagic layer (0–200 m)
rs_vocc <- rast("data-raw/VoCC/ssp245/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp245_2050-2100.tif")
rs_rce  <- rast("data-raw/RCE/ssp245/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp245_2015-2020.tif")

# Compute 25th percentile threshold for each metric
# This identifies areas with lowest climate velocity and lowest climate exposure
q_vocc <- terra::quantile(values(rs_vocc), 0.25, na.rm = TRUE)
q_rce  <- terra::quantile(values(rs_rce),  0.25, na.rm = TRUE)

# Binary rasters: TRUE where raster values are <= 25th percentile
lowest25_vocc <- rs_vocc <= q_vocc
lowest25_rce  <- rs_rce  <= q_rce

# Mask original rasters to retain only low 25 percent cells
rs_vocc_low <- terra::mask(rs_vocc, lowest25_vocc, maskvalues = 0)
rs_rce_low  <- mask(rs_rce,  lowest25_rce,  maskvalues = 0)

# Combine the low-VoCC and low-RCE areas
# intersection (both low)
both_low <- lowest25_vocc & lowest25_rce 
# union (either low) — alternative option
# both_low <- lowest25_vocc | lowest25_rce 

# Generate maps in two projections:
p_deg  <- plot_low25_map(both_low, proj = "latlon")   # geographic projection
p_moll <- plot_low25_map(both_low, proj = "moll")     # Mollweide projection (global)

# Output settings
params <- list(
  out_pdf   = "outputs/figures/exploratory/vocc_rce_low25_refugia_moll_v02.pdf",  # output filename
  # out_png   = "outputs/figures/exploratory/vocc_rce_low25_refugia_moll.png", # optional PNG
  dpi       = 600,
  width_in  = 20,
  height_in = 10
)

# Export the Mollweide version to file (high-resolution)
ggsave(
  filename = params$out_pdf,
  plot     = p_moll,
  width    = params$width_in,
  height   = params$height_in,
  dpi      = params$dpi
)
