## @knitr plot-tract-xtfr-estimates


# xtfr_alpha <- 10.65
# xtfr_beta <- -12.55
# # v15 <- load_variables(2016, "acs5", cache = TRUE)
# 
# # Creating the function to download all of the data.
# # This downloads and creates iTFR and xTFR estimates.
# gatherdat <- function(state){
#   # print(state)
# placeholder <- get_acs("tract",
#                      variables = c(medincome = "B19013_001",
#                                    m0004 = "B01001_003",
#                                    W0004 = "B01001_027",
#                                    W1517 = "B01001_030",
#                                    W1819 = "B01001_031",
#                                    W20 = "B01001_032",
#                                    W21 = "B01001_033",
#                                    W2224 = "B01001_034",
#                                    W2529 = "B01001_035",
#                                    W3034 = "B01001_036",
#                                    W3539 = "B01001_037",
#                                    W4044 = "B01001_038",
#                                    W4549 = "B01001_039"
#                                    ),
#                      state = paste(state),
#                      output = "wide") %>%
#     dplyr::mutate(Ce = m0004E + W0004E,
#                   Cm = m0004M + W0004M,
#                   W         = (W1517E + W1819E + W20E + W21E + W2224E + W2529E + W3034E + W3539E + W4044E + W4549E), # estimating total number of Women
#                   CWR        = (m0004E + W0004E) / W, # estimating Child-woman ratio
#                   p2534      = (W2529E + W3034E) / W, # estimating the proportion of women aged 25-34
#                   xTFR       = (xtfr_alpha + xtfr_beta * p2534) * CWR, # applying the regression coefficients from the HMD/HFD to US counties)
#                   iTFR       = CWR*7) %>%
#     na.omit 
# 
# return(placeholder)
# }
# 
# # Reading a Census CSV file, located online, to create a list of StateIDS to apply the function on.
# fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
#   mutate(GEOID = paste0(X2, X3)) %>%
#   dplyr::rename(state = X1,
#                 STATEID = X2,
#                 CNTYID = X3,
#                 NAME = X4) %>%
#   filter(!STATEID %in% c("60", "66", "69", "72", "74", "78"))
# stateid = unlist(list(unique(fipslist$state)))
# 
# # Getting all of the tract data from every state and putting it into a single df, "tracts."
# dat <- lapply(stateid, gatherdat)
# tracts <- rbindlist(dat) %>%
#   filter(W>=1000)
# 
# write_csv(tracts, "DATA-PROCESSED/censustracts.csv")

tracts <- read_csv("DATA-PROCESSED/censustracts.csv")

ggplot(data=tracts, aes(x = medincomeE/1000, y = xTFR)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  # coord_cartesian(expand = FALSE) +
  facet_zoom(y = xTFR <=2.5) +
  # ,
  #              zoom.size = 0.5,
  #              shrink = FALSE) +
  # coord_cartesian(ylim=c(0,3)) +
  labs(y = "TFR",
       x = "Median HH Income ($000)")
