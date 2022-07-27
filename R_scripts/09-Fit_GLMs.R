
library(tidyverse)

# Call function to fit models
source('R_functions/Model_brms.R')

# Load data for model
dat <- readRDS('derived_data/model_dat.rds') %>%
  # Subset calving period
  filter(risk == 'low')  %>%
  # Rename NDVI landscape column
  rename('NDVI_d_l' = lc_NDVI) 

# Make list of all environmental variables
env_vars <- c('NDVI', 'shdi', 'pforest', 'cwed')

# Fit models and save

# Social connections on tracking
for(i in env_vars) {
  saveRDS(fit_mod(data = dat, env_var = i, f = brmsformula(TR ~ SC + E)),
          paste0('models/glm_', i, 'tr_sc.rds'))
}

# Familiarity on tracking
for(i in env_vars) {
  saveRDS(fit_mod(data = dat, env_var = i, f = brmsformula(TR ~ FL + SC)),
          paste0('models/glm_', i, 'tr_fl_sc.rds'))
}

# Social associations on tracking
for(i in env_vars) {
  saveRDS(fit_mod(data = dat, env_var = i, f = brmsformula(TR ~ SA + SC)),
          paste0('models/glm_', i, 'tr_sa_sc.rds'))
}

# Social connections on social associations
saveRDS(fit_mod(data = dat, f = brmsformula(SA ~ SC)), 'models/glm_sa_sc.rds')

# Social connections on risk
saveRDS(fit_mod(data = dat, f = brmsformula(R ~ SC)), 'models/glm_r_sc.rds')

# Social associations on risk
saveRDS(fit_mod(data = dat, f = brmsformula(R ~ SA + SC)), 'models/glm_r_sa.rds')


