# Marine climate refugia review

This repository contains the R code and data processing workflows that underpin the analyses for our manuscript:

> *Identifying marine climate refugia to advance climate-smart conservation*\
> Marina Sanz-Martín, Carolina Olguín-Jacobson, Jessica A. Bolin, Carla Quiles-Pons, Isaac Brito-Morales, Jorge García Molinos, Manuel Hidalgo, Irene D. Alabia, Elena Gissi, Mikaela M. Provost, Fiorenza Micheli\**,* Nur Arafeh-Dalmau\*. In review at Trends in Ecology & Evolution.*\
> *\*Joint senior authors\
> Corresponding author: Marina Sanz-Martín. marina.sanz(at)ieo(dot)csic(dot)es

# 📂 Repository Structure

```         
tree_climate_refugia/
├── data-raw/                   # Raw inputs 
│   ├── VoCC/
│   ├── RCE/
│   ├── boundaries/
│   ├── reference_data/
├── legacy/                     # Depreacted scripts kept for posterity
├── outputs/                    # Figures and .RDS files used for figures
├── R/
│   ├── load_packages.R         # Package loader with renv support
│   ├── utils_helpers.R         # Basemap, projections and helper functions
│   ├── make_refugia_VoCCRCE_plot.R # Main map generation function
├── scripts/
│   ├── 01_plot_combined_low25.R  # Reproducible workflow script
│   ├── 02_clean_fig_dataset.R    # Clean Box 3 figure dataset
│   ├── 03_summarystat_barplots_nolocation.R    # Generate barplots for Box 3 figure
│   ├── 04_build_meow_study_counts_and_plot_maps.R    # Box 3 map
│   ├── 05_percentages_box3.R    # Generate % for Box 3 stats
├── renv/                       # Local isolated R package environment
├── renv.lock                   # Frozen dependency versions
├── LICENSE
└── README.md
```
