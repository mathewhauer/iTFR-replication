## @knitr make-africa-map

packages <- function(x){
  
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[,1])
  
  if (length(intersect(x, installed_packages)) == 0){
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}


packages(tidyverse)
packages(ggrepel)
packages(grid)
packages(gridExtra)
packages(HMDHFDplus)
packages(scales)
packages(dplyr)
packages(openxlsx) 
packages(tmap)
packages(tmaptools)
packages(tigris)
packages(censusapi)
packages(tidycensus)
packages(cowplot)
packages(magick)
packages(pdftools)
packages(readxl)
packages(ggpubr)
packages(wpp2017)
packages(raster)
packages(rgdal)
packages(sf)

# q5 data comes from the UN's World Population Prospects
countryq5 <- read.xlsx("../R/DATA-RAW/AFRICA/WPP2017_MORT_F01_2_Q5_BOTH_SEXES.xlsx", sheet=1) %>% dplyr::select(X3, X18) %>% na.omit %>%
  rename(Country = X3, q5_2015 = X18) %>%
  mutate(q5_2015 = as.numeric(q5_2015)/1000)
borders <- read_sf("../R/DATA-RAW/AFRICA/TM_WORLD_BORDERS-0.3/Africa.shp") %>%
  mutate(NAME = case_when(
    NAME == "Libyan Arab Jamahiriya" ~"Libya",
    NAME == "Cape Verde" ~ "Cabo Verde",
    NAME == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    TRUE ~ as.character(NAME))) %>%
  append_data(., countryq5, key.shp = "NAME", key.data = "Country")

# The WorldPop data is too large to host on GitHub. To fully replicate, you'll need to download the files from WorldPop
# and populate the folder yourself.
# Note: The African 1 sq. km raster data can be downloaded from WorldPop at: http://www.worldpop.org.uk/data/summary/?id=337 

# #----ITFR AFRICA----
# # Reading the gridded population data from WorldPop for the year 2015
# f0004_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A0004_F_2015_adj_v5.tif")
# m0004_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A0004_M_2015_adj_v5.tif")
# f1519_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A1519_F_2015_adj_v5.tif")
# f2024_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A2024_F_2015_adj_v5.tif")
# f2529_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A2529_F_2015_adj_v5.tif")
# f3034_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A3034_F_2015_adj_v5.tif")
# f3539_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A3539_F_2015_adj_v5.tif")
# f4044_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A4044_F_2015_adj_v5.tif")
# f4549_2015 <- raster("../R/DATA-RAW/Worldpop/AFR_PPP_A4549_F_2015_adj_v5.tif")
# q5 <- rasterize(borders, f0004_2015, 'q5_2015')
# 
# # Calculating the xTFR based on the coefficients from above
# iTFR_worldpop <- overlay(f0004_2015,
#                          m0004_2015,
#                          f1519_2015,
#                          f2024_2015,
#                          f2529_2015,
#                          f3034_2015,
#                          f3539_2015,
#                          f4044_2015,
#                          f4549_2015,
#                          q5missing,
#                          fun=function(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10){
#                            C = (r1+r2)
#                            W = (r3+r4+r5+r6+r7+r8+r9)
#                            return(7 * (C/(1-0.8*r10)/W))})
# 
# writeRaster(iTFR_worldpop, filename="DATA-PROCESSED/AFRICA-iTFRplus_worldpop.tif", format="GTiff", overwrite=TRUE)

rasterDF <- raster("../R/DATA-PROCESSED/AFRICA-iTFRplus_worldpop.tif")

# Reading the boundary files for Africa
borders <- read_sf("../R/DATA-RAW/AFRICA/TM_WORLD_BORDERS-0.3/Africa.shp")  %>%
  mutate(NAME = case_when(
    NAME == "Libyan Arab Jamahiriya" ~"Libya",
    NAME == "United Republic of Tanzania" ~ "Tanzania",
    NAME == "Democratic Republic of the Congo" ~ "DR Congo",
    TRUE ~ as.character(NAME)))

# Setting the color ramps
rampcols <- c("#2b8cbe", brewer.pal(8, "YlOrRd"))

# Setting the bounding box of the map to just the outline of Africa
africa_bb <- bb(rasterDF, xlim=c(.14, .82), ylim=c(0.15, .96), relative = TRUE)
# Mapping the gridded TFRS
a <- tm_shape(rasterDF, bbox = africa_bb) +
  tm_raster("AFRICA.iTFRplus_worldpop",
            palette = rampcols,
            breaks = c(0, 2.1, 3, 4, 5, 6, 7, 8, 9, Inf),
            title = "TFR") +
  tm_shape(borders) +
  tm_borders(lwd=2, col="black", alpha = 0.5) +
  tm_text("NAME" , size="AREA", root=5, legend.size.show = FALSE) +
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.text.size = 0.9,
            legend.format = list(digits=1))
a
# save_tmap(a, filename="./MANUSCRIPT/FIGURES/top_africa4.pdf")

