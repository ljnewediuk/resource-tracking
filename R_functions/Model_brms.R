
#################################################
###                                           ###
### Function to fit brms models               ###
###                                           ###
### Returns MCMC glm and a data.frame of      ###
### variables included in the model for       ###
### predicting.                               ###
###                                           ###
#################################################

library(brms)
library(tidyverse)

# Arguments:
#   data: df containing model variables variable
#   env_var: Landscape variable to track between years; defaults to NDVI
#   f: brms formula specifying the model

# Use cmdstanr as a backend
options('brms.backend' = 'cmdstanr')

# Define function
fit_mod <- function(data, env_var = 'NDVI', f) {
  
  model_dat <- data %>%
    # Select variables for priors and model
    select(c(TR = paste0(env_var, '_tr'), E = paste0(env_var, '_d_l'), 
             SC = yr2_sc, SF = fidelity, R = mean_f_cort, FL = `post-calv_fam`,
             SA = social_assoc)) %>%
    # Scale variables
    mutate(across(.cols = everything(), scale))

  
  # Get the default priors, given the formula and data
  default_priors <- get_prior(f, data = model_dat)
  
  # Fit the model
  m <- brm(
    formula = f,
    data = model_dat,
    prior = default_priors
  )
  
  # Put scaled df and model in list
  m_results <- list(m, model_dat)
  names(m_results) <- c('model', 'scaled_vars')
  
  # Return list
  return(m_results)
  
}

