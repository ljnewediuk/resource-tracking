
# Load packages
library(tidyverse)
library(ctmm)

# Load AKDE models to calculate overlaps
akde_models <- readRDS('output/akde_models.rds')

# ** Note: Do we need to standardize graph strength/overlap by year to account for
#       different numbers of collared animals?

# Get overlaps from models
overlap_CIs <- overlap(akde_models, level = 0.95, debias = T)
# Subset to upper triangles only
ut <- upper.tri(overlap_CIs[,, 1], diag = FALSE)

# Make data frame to compare 
overlaps <- data.frame(
  # Get initial models
  initial = rep(rownames(overlap_CIs), ncol(overlap_CIs))[c(ut)],
  # Get comparisons
  comparison = rep(colnames(overlap_CIs), each = nrow(overlap_CIs))[c(ut)],
  # Calculate mean and confidence intervals of overlap
  lower = overlap_CIs[,,1][ut],
  mean = overlap_CIs[,,2][ut],
  upper = overlap_CIs[,,3][ut]) %>%
  # Get animal ID, year, and period for both comparisons
  mutate(animal_ID = substr(gsub("(.*_){1}(\\d+)_.+", "\\1", initial), 1,
                            nchar(gsub("(.*_){1}(\\d+)_.+", "\\1", initial)) - 1),
         year = gsub("(.*_){2}(\\d+)_.+", "\\2", initial),
         period = gsub('.*-([0-9]+).*','\\1', initial),
         comp_ID = substr(gsub("(.*_){1}(\\d+)_.+", "\\1", comparison), 1,
                          nchar(gsub("(.*_){1}(\\d+)_.+", "\\1", comparison)) - 1),
         comp_year = gsub("(.*_){2}(\\d+)_.+", "\\2", comparison),
         comp_period = gsub('.*-([0-9]+).*','\\1', comparison)) %>%
  # Define period as pre- or post-calving and make year numeric
  mutate(period = ifelse(period == 213, 'pre-calv', 'post-calv'),
         comp_period = ifelse(comp_period == 213, 'pre-calv', 'post-calv'),
         year = as.numeric(year))

# Get unique elk collared in each year
unq_collared <- overlaps %>%
  group_by(animal_ID, year) %>%
  summarize(n()) %>%
  ungroup() %>% group_by(year) %>%
  summarize(weights = n())

# Calculate overlaps from same year and period

# Filter from same year and period
overlaps_within_year <- overlaps %>%
  filter(year == comp_year & period == comp_period)

# Get spatial network for each year and period
network_within_year <- data.frame()
for(yr in unique(overlaps_within_year$year)) {
  for(prd in c('pre-calv', 'post-calv')) {
    # Social network
    snetwork <- overlaps_within_year %>%
      filter(year == yr & period == prd) %>%
      select(animal_ID, comp_ID, mean) %>%
      pivot_wider(names_from = animal_ID, values_from = mean) %>%
      column_to_rownames('comp_ID') %>%
      as.matrix()
    # Calculate graph strength
    hr.grph_df <-
      igraph::graph_from_adjacency_matrix(snetwork,
                                          mode = "lower",
                                          diag = F,
                                          weighted = T)
    # Build row
    hr_networks <- data.frame(year = yr, period = prd,
                              i_strength = igraph::graph.strength(hr.grph_df)) %>%
      rownames_to_column('animal_ID')
    # Bind to rest of df
    network_within_year <- rbind(network_within_year, hr_networks)
    
  }
  
}

# Filter only the same individual from the same period and different years
overlaps_across_years <- overlaps %>%
  filter(animal_ID == comp_ID & period == comp_period) %>%
  rename('fidelity' = mean) %>%
  # Select only years compared to a previous year
  select(animal_ID, fidelity, year, comp_year, period, comp_period) %>%
  mutate(year = as.numeric(year),
         comp_year = as.numeric(comp_year))

# Join dfs with fidelity and graph strength in first year 
# (i.e., prospecting for resources using social information in previous year)
soc_fidel_dat_yr1 <- overlaps_across_years %>%
  select(! comp_year) %>%
  left_join(network_within_year) %>%
  # Add weights
  left_join(unq_collared) %>%
  mutate(wgtd_i_strength = i_strength/weights,
         year_comp = paste0(year, '-', year + 1))

# Join dfs with fidelity and graph strength in second year 
# (i.e., conspecific attraction/local enhancement in current year)
soc_fidel_dat_yr2 <- overlaps_across_years %>%
  select(! year) %>%
  rename('year' = comp_year) %>%
  left_join(network_within_year) %>%
  # Add weights
  left_join(unq_collared) %>%
  mutate(wgtd_i_strength = i_strength/weights,
         year_comp = paste0(year - 1, '-', year))

# Save site fidelity/social connection data
saveRDS(soc_fidel_dat_yr1, 'output/soc_fidel_dat_yr1.rds')
saveRDS(soc_fidel_dat_yr2, 'output/soc_fidel_dat_yr2.rds')
saveRDS(overlaps_within_year, 'output/hr_overlaps.rds')


