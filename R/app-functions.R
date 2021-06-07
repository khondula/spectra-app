
# FUNCTIONS FOR SHINY APP
library(tidyverse)

my_wls <- 400:800
water_df <- read_csv('data/water-approx1nm.csv') %>%
  dplyr::filter(wl %in% my_wls)
wasi_df <- read_csv('data/gege-wasi-spectra.csv') %>% 
  dplyr::filter(nm %in% my_wls)

absorb_water <- water_df$absorb_m1
backs_water <- water_df$backs_m1
absorb_phyto <- wasi_df$phyto_mixA

# water_df <- read_csv("data/water-approx1nm.csv") 
# leet2 <- read_csv("data/lee1998-t2_phy_abs_coeffs.csv")
# abs_water = data.frame(wl = water_df$wl,
#                        abs_water = water_df$absorb_m1)
# backs_water = data.frame(wl = water_df$wl,
#                          backs_water = water_df$backs_m1)
lake_river_sites <- c("BARC", "CRAM", "FLNT", 
                      "LIRO", "PRLA", 
                      "PRPO", "SUGG",
                      "TOMB", "TOOK")

spectra_join <- read_csv('data/spectra_join.csv') %>%
  dplyr::filter(aq_site %in% lake_river_sites)

# calc_absorb <- function(wl, absREF, aSF =  1.3e-2, wlREF = 443){
#   abs_wl <- absREF * exp(-aSF * (wl - wlREF))
#   return(abs_wl)
# }
# 
# calc_scatter <- function(wl, scatterREF, bSF, wlREF = 443){
#   scatter_wl <- scatterREF * (wl/wlREF)^(bSF)
#   return(scatter_wl)
# }
# 
# calc_scatter_spectra <- function(scatterREF, bSF,
#                                  bratio = 0.01, wlREF = 443, 
#                                  spectra_min = 400, spectra_max = 800){
#   wl_range <- spectra_min:spectra_max
#   my_spectra <- wl_range %>% 
#     purrr::map_dbl(~calc_scatter(.x, scatterREF, bSF, wlREF)) %>%
#     as_tibble() %>%
#     mutate(backscatter_m1 = value*bratio) %>%
#     mutate(wavelength = wl_range) %>%
#     mutate(wavelength = as.numeric(wavelength)) %>%
#     mutate(wlREF = wlREF, SF = bSF)
#   return(my_spectra)
# }
# 
# calc_absorb_spectra <- function(absREF,  aSF =  1.3e-2, wlREF = 443, 
#                                 spectra_min = 400, spectra_max = 800){
#   wl_range <- spectra_min:spectra_max
#   my_spectra <- wl_range %>% 
#     purrr::map_dbl(~calc_absorb(.x, absREF, aSF, wlREF)) %>%
#     as_tibble() %>%
#     rename(abs_m1 = value) %>%
#     mutate(wavelength = wl_range) %>%
#     mutate(wavelength = as.numeric(wavelength)) %>%
#     mutate(wlREF = wlREF, SF = aSF)
#   return(my_spectra)
# }


# abs440 = 1
# 
# lee_spectra <- leet2 %>%
#   dplyr::mutate(absorb1 = (a0 + a1*log(abs440))*abs440,
#                 absorb2 = (a0 + a1*log(0.5))*0.5,
#                 absorb3 = (a0 + a1*log(2))*2)
