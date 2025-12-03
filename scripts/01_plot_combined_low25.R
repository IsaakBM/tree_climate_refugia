source("R/load_packages.R")
source("R/utils_helpers.R")
source("R/make_refugia_VoCCRCE_plot.R")

library(terra)
rs_vocc <- rast("data-raw/VoCC/ssp126/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp126_2050-2100.tif")
rs_rce <- rast("data-raw/RCE/ssp126/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp126_2015-2020.tif")

q_vocc <- terra::quantile(values(rs_vocc), 0.25, na.rm = TRUE)
q_rce  <- terra::quantile(values(rs_rce),  0.25, na.rm = TRUE)

lowest25_vocc <- rs_vocc <= q_vocc
lowest25_rce  <- rs_rce  <= q_rce

rs_vocc_low <- terra::mask(rs_vocc, lowest25_vocc, maskvalues = 0)
rs_rce_low  <- mask(rs_rce,  lowest25_rce,  maskvalues = 0)


both_low <- lowest25_vocc & lowest25_rce # intersection
both_low <- lowest25_vocc | lowest25_rce # addition

plot(both_low)
plot(lowest25_vocc)


# degrees version
p_deg  <- plot_low25_map(both_low, proj = "latlon")
# mollweide version
p_moll <- plot_low25_map(both_low, proj = "moll")


params <- list(
  out_pdf   = "outputs/figures/exploratory/vocc_rce_low25_refugia_moll.pdf",
  # out_png   = "outputs/figures/exploratory/vocc_rce_low25_refugia_moll.png",
  dpi       = 600,
  width_in  = 20,
  height_in = 10
)


ggsave(
  filename = params$out_pdf,
  plot     = p_moll,
  width    = params$width_in,
  height   = params$height_in,
  dpi      = params$dpi
)



