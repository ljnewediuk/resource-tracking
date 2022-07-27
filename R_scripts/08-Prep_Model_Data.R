
library(tidyverse)

# Load home range metrics
hr_metrics <- readRDS('output/hr_raster_metrics.rds') %>%
  group_by(animal_ID, period) %>%
  # Calculate difference in landscape between years
  mutate(cwed_d = abs(cwed - cwed[year == min(year)]),
         pforest_d = abs(pforest - pforest[year == min(year)]),
         shdi_d = abs(shdi - shdi[year == min(year)]),
         year_comp = paste0('20', year - 1, '-', '20', year),
         period = ifelse(period == '122-213', 'pre-calv', 'post-calv')) %>%
  # Filter out the doubles
  filter(! cwed_d == 0) %>%
  # Select variables needed
  select(! c(year, cwed, pforest, shdi))

# Load landscape metrics
lc_metrics <- readRDS('output/lc_raster_metrics.rds') %>%
  # Make year numeric
  mutate(year = as.numeric(year))

# Loop to calculate difference in landscape between years
lc_diffs <- data.frame()
for(i in 2:nrow(lc_metrics)) {
  # Calculate metrics by row
  diffs <- lc_metrics[i ,] %>%
    mutate(cwed_d_l = abs(cwed - lc_metrics[i-1 ,]$cwed),
           pforest_d_l = abs(pforest - lc_metrics[i-1 ,]$pforest),
           shdi_d_l = abs(shdi - lc_metrics[i-1 ,]$shdi),
           year_comp = paste0(year - 1, '-', year)) %>%
    # Select variables needed
    select(cwed_d_l:year_comp)
  # Bind together
  lc_diffs <- rbind(lc_diffs, diffs)
}

# Load social connection/site fidelity data
soc_dat_yr1 <- readRDS('output/soc_fidel_dat_yr1.rds') %>%
  na.omit() %>%
  select(year_comp, animal_ID:comp_period, wgtd_i_strength)
soc_dat_yr2 <- readRDS('output/soc_fidel_dat_yr2.rds') %>%
  na.omit() %>%
  select(animal_ID, period, wgtd_i_strength, year_comp)

# Combine social/fidelity data with landscape metrics
lc_soc_dat <- soc_dat_yr1 %>% 
  # Social connections in first year and second year of comparison
  rename('yr1_sc' = wgtd_i_strength) %>%
  left_join(soc_dat_yr2) %>%
  rename('yr2_sc' = wgtd_i_strength) %>%
  left_join(hr_metrics) %>% left_join(lc_diffs) %>%
  # Calculate tracking ability
  mutate(cwed_tr = cwed_d_l - cwed_d,
         pforest_tr = pforest_d_l - pforest_d,
         shdi_tr = shdi_d_l - shdi_d,
         risk = ifelse(period == 'pre-calv', 'low', 'high')) %>%
  # Select variables needed
  select(year_comp, animal_ID, period, fidelity, yr1_sc, yr2_sc, cwed_d_l:shdi_tr)

# Load familiarity data
familiarity <- readRDS('derived_data/familiarity.rds')

# Combine social and familiarity data
soc_fam_dat <- lc_soc_dat %>%
  left_join(familiarity)

# Load NDVI data
NDVI_dat <- readRDS('output/NDVI_dat.rds') %>%
  group_by(animal_ID) %>%
  filter(year == max(year)) %>%
  mutate(year_comp = paste(year - 1, year, sep = '-'),
         NDVI_tr = hr_NDVI - lc_NDVI,
         period = ifelse(period == '122-213', 'pre-calv', 'post-calv')) %>%
  select(year_comp, animal_ID, period, lc_NDVI, hr_NDVI, NDVI_tr)

# Load cort data
cort_dat <- readRDS('derived_data/cort_dat.rds')

# Load social associations data
assoc_dat <- readRDS('derived_data/associations.rds') %>%
  select(animal_ID, loc_strength) %>%
  rename('social_assoc' = loc_strength)

# Combine NDVI and cort data
dat <- soc_fam_dat %>%
  left_join(NDVI_dat) %>%
  left_join(cort_dat) %>%
  left_join(assoc_dat) %>%
  mutate(risk = ifelse(period == 'pre-calv', 'low', 'high'))

# Save data
saveRDS(dat, 'derived_data/model_dat.rds')


