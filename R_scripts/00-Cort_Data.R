
library(tidyverse)
library(rptR)

# Load fecal data
fecal_dat <- readRDS('input/stress_dat.rds') %>%
  sf::st_drop_geometry() %>%
  select(animal_ID, cort_ng_g) %>%
  rename('f_cort_ng_g' = cort_ng_g) %>%
  unique()

# Add hair cort from capture (raw data)
hair_dat <- data.frame(
  animal_ID = paste0('ER_E_', 15:32),
  h_cort_ng_g = c(0.99, 2.73, 1.72, 1.51, 1.37, 1.64, 1.81, 1.76, 2.31, 1.43, 
                  2.08, 1.92, 1.01, 1.57, 1.35, 1.81, 2.33, 1.59)
)

# Summarize mean ± sd fecal cort by animal
summ_cort <- fecal_dat %>% 
  group_by(animal_ID) %>%
  summarise(mean_f_cort = mean(f_cort_ng_g, na.rm = T),
            se_f_cort = sd(f_cort_ng_g, na.rm = T)/sqrt(n()))

# Add hair cort
cort_dat <- hair_dat %>%
  left_join(summ_cort)

# Plot relationship between fecal and hair cort (expected positive relationship)
ggplot(cort_dat, aes(x = h_cort_ng_g, y = mean_f_cort)) + 
  geom_pointrange(aes(ymin = mean_f_cort - se_f_cort, ymax = mean_f_cort + se_f_cort)) +
  geom_smooth(method = 'lm', colour = 'black')

# Calculate repeatability of fecal cort
rep1 <- suppressMessages(rpt(f_cort_ng_g ~ (1 | animal_ID), 
            grname = 'animal_ID', data = fecal_dat, 
            datatype = 'Gaussian', nboot = 1000, npermut = 0))
# Summarize
summary(rep1)

# Save data
saveRDS(cort_dat, 'derived_data/cort_dat.rds')


