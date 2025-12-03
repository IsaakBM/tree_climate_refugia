# ============================
# Centralized Package Loader
# Author: Isaac Brito-Morales
# Email: ibrito@conservation.org
# ============================

# Full list of required packages
libs <- c(
  "terra", "sf", "ggplot2", "RColorBrewer", "patchwork", "dplyr",
  "rnaturalearth", "rnaturalearthdata", "future.apply",
  "tidyr", "transformr", "stringr", "readr", "data.table",
  "doParallel", "foreach", "lwgeom", "purrr", "viridisLite", "scales"
)

# Try to use renv if available
if (requireNamespace("renv", quietly = TRUE)) {
  # Check if a renv project is already active
  p <- tryCatch(renv::project(), error = function(e) NULL)
  
  # If no project, initialize renv in this directory
  if (is.null(p)) {
    message("Initializing renv project in this directory...")
    renv::init()
  }
}

# Install missing packages depending on what is available
if (requireNamespace("renv", quietly = TRUE) && !is.null(tryCatch(renv::project(), error = function(e) NULL))) {
  # Using renv
  missing <- libs[!(libs %in% rownames(installed.packages()))]
  if (length(missing)) {
    message("Installing missing packages with renv: ", paste(missing, collapse = ", "))
    renv::install(missing)
    renv::snapshot()
  }
} else if (requireNamespace("pak", quietly = TRUE)) {
  # Fallback to pak
  missing <- libs[!(libs %in% rownames(installed.packages()))]
  if (length(missing)) {
    message("Installing missing packages with pak: ", paste(missing, collapse = ", "))
    pak::pak(missing, ask = FALSE)
  }
} else {
  # Last resort: base install.packages
  missing <- libs[!(libs %in% rownames(installed.packages()))]
  if (length(missing)) {
    message("Installing missing packages with install.packages(): ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}

# Load all packages
invisible(lapply(libs, library, character.only = TRUE))

message("All required packages loaded successfully.")