
#################################################
###                                           ###
### Resample ACI and CDL raster data to same  ###
### classifications                           ###
###                                           ###
#################################################

library(landscapemetrics)
library(stars)
library(tidyverse)

# Make vector of years needed for extraction
yrs <- c('09', 10:20)

# Import raster data into directory
system('mkdir ~/Documents/R-Projects/site_fidelity/rasters/')
for(i in yrs) {
  system(paste('cp ~/Documents/Spatial*Data/Manitoba*Data/landcover/ACI/',
               'aci_20', i, '.tif ', 
               '~/Documents/R-Projects/site_fidelity/rasters/', sep = ''))
  system(paste('cp ~/Documents/Spatial*Data/Minnesota*Data/landcover/CDL/',
               'cdl_20', i, '.tif ', 
               '~/Documents/R-Projects/site_fidelity/rasters/', sep = ''))
  
}

# Read rasters
lc_stars <- read_stars(list.files('rasters/', full.names = T))

# Read landcover classifications and join
lc_classes <- read.csv('input/aci_lc.csv') %>%
  rename('band_aci' = band) %>%
  full_join(read.csv('input/cdl_lc.csv')) %>%
  rename('band_cdl' = band) %>%
  # Filter out any NA classes
  filter(is.na(band_aci) | is.na(band_cdl))

# Rename classes to make sure landscape descriptions match between ACI and CDL
# Renames for CDL
lc_cdl <- read.csv('input/cdl_lc.csv') %>%
  mutate(landcover = ifelse(band == 81, 'Cloud', landcover),
         landcover = ifelse(band == 65, 'Exposed Land and Barren', landcover),
         landcover = ifelse(band %in% c(82, 121:124), 'Urban and Developed', landcover),
         landcover = ifelse(band %in% c(87, 190, 195), 'Wetland', landcover),
         landcover = ifelse(band == 176, 'Grassland', landcover),
         landcover = ifelse(band == 61, 'Fallow', landcover),
         landcover = ifelse(band == 25, 'Grains', landcover),
         landcover = ifelse(band == 30, 'Spelt', landcover),
         landcover = ifelse(band == 22, 'Wheat', landcover),
         landcover = ifelse(band %in% c(31, 34), 'Oilseeds', landcover),
         landcover = ifelse(band %in% c(141:143), 'Forest', landcover),
         landcover = ifelse(band == 59, 'Sod', landcover)) %>%
  rename('band_cdl' = band)
# Renames for ACI
lc_aci <- read.csv('input/aci_lc.csv') %>%
  mutate(landcover = ifelse(band == 85, 'Wetland', landcover),
         landcover = ifelse(band %in% c(110, 122), 'Grassland', landcover),
         landcover = ifelse(band %in% c(130, 131), 'Fallow', landcover),
         landcover = ifelse(band %in% c(132, 134), 'Grains', landcover),
         landcover = ifelse(band %in% c(200, 210, 220, 230), 'Forest', landcover)) %>%
  rename('band_aci' = band)

# Combine to check which bands correspond between classifications
lc_classes <- lc_aci %>%
  full_join(lc_cdl) %>%
  # Remove any NAs with no match
  filter(! is.na(band_aci) & ! is.na(band_cdl)) %>%
  # Unique class for each matching land cover type
  group_by(landcover) %>%
  mutate(new_class = cur_group_id())
# Get reclassification tables
# ACI
reclass_aci <- lc_classes %>%
  select(band_aci, landcover, new_class) %>%
  unique()
# CDL
reclass_cdl <- lc_classes %>%
  select(band_cdl, landcover, new_class) %>%
  unique()

# Reclassify and merge rasters by year and new class
stars_by_yr <- list()
for(i in 1:length(yrs)) {
  
  # Get aci and cdl layers from each year
  aci <- lc_stars[paste0('aci_20', yrs[i], '.tif')]
  cdl <- lc_stars[paste0('cdl_20', yrs[i], '.tif')]
  # Reclassify rasters according to new class system
  for(r in 1:nrow(reclass_aci)) {
    aci[aci == reclass_aci[r ,]$band_aci] <- reclass_aci[r ,]$new_class # Forest
  }
  for(r in 1:nrow(reclass_cdl)) {
    cdl[cdl == reclass_cdl[r ,]$band_cdl] <- reclass_cdl[r ,]$new_class # Forest
  }
  # Make any values outside new class or < 1 == NA
  aci[aci > max(reclass_aci$new_class)] <- NA
  cdl[cdl > max(reclass_cdl$new_class)] <- NA
  aci[aci == 0] <- NA
  cdl[cdl == 0] <- NA
  # Combine rasters
  lc <- st_mosaic(aci, cdl)
  # Add raster to list by year
  stars_by_yr[[i]] <- lc
  names(stars_by_yr)[[i]] <- paste0('lc_', yrs[i])
}

# Save new rasters and classification system
saveRDS(stars_by_yr, 'output/raster_list.rds')
saveRDS(lc_classes, 'output/raster_classifications.rds')


