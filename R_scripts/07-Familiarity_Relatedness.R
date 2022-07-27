
# Load packages
library(tidyverse)
library(vegan)

# Load data
hr_overlaps <- readRDS('output/hr_overlaps.rds')

# Get ß similarity of social connections between years

b_sims <- data.frame()

for(i in unique(hr_overlaps$animal_ID)) {
  
  p_comp <- data.frame(animal_ID = i)
  
  for(j in c('pre-calv', 'post-calv')) {
    
    comp <- hr_overlaps %>%
      filter(animal_ID == i & period == j)
    
    years <- unique(comp$year)
    IDs <- unique(comp$comp_ID)
    
    if(length(years) < 2) next
      
    y_comp <- matrix(
      nrow = length(years), 
      ncol = length(IDs),
      dimnames = list(years, IDs)
    )
    
    # For comparing similarity based on strength of social connection
    # If just comparing presence/absence of a connection between years,
    # ifelse(IDs %in% unique(comp[comp$year == min(years) ,]$comp_ID), 1, 0)
    y_comp[1 ,] <- ifelse(IDs %in% 
                            unique(comp[comp$year == min(years) ,]$comp_ID), 
                          comp[comp$year == min(years), ]$mean, 0)
    y_comp[2 ,] <- ifelse(IDs  %in% 
                            unique(comp[comp$year == max(years) ,]$comp_ID), 
                          comp[comp$year == max(years), ]$mean, 0)
    
    # Calculate similarity index (see Koleff et al. 2003)
    sim_ind <- as.numeric(betadiver(y_comp, method = 'sor'))
    
    # Instead get the sum of differences in overlap between years
    sim_ind <- sum(abs(y_comp[2 ,] - y_comp[1 ,]))
    
    p_comp <- cbind(p_comp, sim_ind)
    
    colnames(p_comp)[ncol(p_comp)] <- paste0(j, '_fam')
    
  }
  
  if(ncol(p_comp) < 3) next
  b_sims <- rbind(b_sims, p_comp)
  
}

saveRDS(b_sims, 'derived_data/familiarity.rds')
