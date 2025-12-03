# Marine climate refugia review 

This repository contains the R code and data processing workflows that underpin the analyses for our manuscript:

> *Identifying marine climate refugia to advance climate-smart conservation*
Sanz-Martín, M., Olguín-Jacobson, C., García Molinos, J., Hidalgo, M., Alabia, I., 
Brito-Morales, I., Bolin, J.A., Gissi, E., Quiles-Pons, C., Micheli, F., Provost, M.M., 
Arafeh-Dalmau, N., in review at Trends in Ecology & Evolution.

The scripts in this repository perform the extraction and spatial mapping of candidate climate refugia based on 
global epipelagic climate velocity (VoCC) and relative climate exposure (RCE) derived from CMIP6 ensemble models. 
These analyses support the conceptual and empirical framework developed in the manuscript.

📂 Repository Structure
tree_climate_refugia/
├── data-raw/                   # Raw raster inputs (VoCC and RCE)
│   ├── VoCC/
│   ├── RCE/
├── outputs/
│   ├── figures/
│   │   ├── exploratory/        # Working drafts
│   │   ├── final/              # Paper-ready figures
├── R/
│   ├── load_packages.R         # Package loader with renv support
│   ├── utils_helpers.R         # Basemap, projections and helper functions
│   ├── make_refugia_VoCCRCE_plot.R # Main map generation function
├── scripts/
│   ├── 01_plot_combined_low25.R    # Reproducible workflow script
├── renv/                       # Local isolated R package environment
├── renv.lock                   # Frozen dependency versions
├── LICENSE
└── README.md
