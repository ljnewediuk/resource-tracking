
#################################################
###                                           ###
### Assign weights to land cover classes for  ###
### contrast-weighted edge density            ###
###                                           ###
#################################################

library(tidyverse)

# Load raster classifications
contrast_weights <- readRDS('output/raster_classifications.rds') %>%
  rename('class' = new_class) %>%
  # Get unique classes and landcover
  select(class, landcover) %>%
  unique() %>%
  # Assign weights
  mutate(weight = ifelse(landcover == 'Cloud', 0, NA),
         # Natural landcovers with low values
         weight = ifelse(landcover == 'Forest', 0.025, weight),
         weight = ifelse(landcover == 'Shrubland', 0.05, weight),
         weight = ifelse(landcover %in% c('Water', 'Wetland'), 0.075, weight),
         weight = ifelse(landcover %in% c('Grassland', 'Sod'), 0.1, weight),
         # Crops middle values
         weight = ifelse(landcover %in% c('Grains', 'Barley', 'Millet', 'Oats',
                                          'Rye', 'Spelt', 'Triticale', 'Wheat',
                                          'Switchgrass', 'Sorghum', 'Winter Wheat',
                                          'Spring Wheat'), 0.3, weight),
         weight = ifelse(landcover %in% c('Camelina', 'Flaxseed', 'Mustard', 
                                          'Safflower', 'Sunflower', 
                                          'Oilseeds'), 0.4, weight),
         weight = ifelse(landcover %in% c('Soybeans', 'Peas', 'Lentils'), 0.5, weight),
         weight = ifelse(landcover == 'Corn', 0.6, weight),
         weight = ifelse(landcover %in% c('Vetch', 'Hops', 'Herbs', 'Tobacco',
                                          'Buckwheat'), 0.7, weight),
         weight = ifelse(landcover %in% c('Tomatoes', 'Potatoes', 'Sugarbeets',
                                          'Other Crops'), 0.8, weight),
         # Urban/exposed habitat high values
         weight = ifelse(landcover %in% c('Fallow', 
                                          'Exposed Land and Barren'), 0.975, weight),
         weight = ifelse(landcover == 'Urban and Developed', 1, weight))

# Save contrasts
saveRDS(contrast_weights, 'output/raster_weights.rds')
