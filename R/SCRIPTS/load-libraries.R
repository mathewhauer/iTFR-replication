###------LIBRARY SETUP-----
## @knitr load-libraries



if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}


# Libraries
pkgs <- c(
  "magick",        # magick
  "tidyverse",     # Tidyverse
  "data.table",    # Data Management/Manipulation
  "readxl",        # Microsoft Excel Files
  "scales",        # Number formatting
  "cowplot",       # Plot Grids
  "tmap",          # Cartography
  "tmaptools",     # Cartographic tools
  "tigris",        # US shapefiles
  "censusapi",     # Census Data
  "kableExtra",    # Pretty Tables
  "pdftools",      # Load pdfs
  "R.utils",       # Utilities
  "tidycensus",    # Census Data
  "HMDHFDplus",    # Human Mortality Database
  "ggrepel",       # Formating labels
  "ggpubr",        # Publication ready plots
  "getPass",       # Entering Passwords
  "raster",        # Rasters
  "sf",            # Shapefiles
  "rstan",         # STAN
  "RColorBrewer",  # Colors
  "ipumsr",        # IPUMS
  "openxlsx",      # openxlsx
  "grid",
  "gridExtra",
  "numform",
  "RJSONIO",       # jsonlite
  "ggforce"
)

# Install missing packages
# Will only run if at least one package is missing

if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# load the packages
p_load(pkgs, character.only = TRUE)
rm(pkgs)