
library(tidyverse)
library(brms)

# Load models
NDVItr_sa_sc <- readRDS('models/glm_NDVItr_sa_sc.rds')$model
cwedtr_sa_sc <- readRDS('models/glm_cwedtr_sa_sc.rds')$model
NDVItr_fl_sc <- readRDS('models/glm_NDVItr_fl_sc.rds')$model
cwedtr_fl_sc <- readRDS('models/glm_cwedtr_fl_sc.rds')$model
r_sc <- readRDS('models/glm_r_sc.rds')$model
r_sa <- readRDS('models/glm_r_sa.rds')$model
sa_sc <- readRDS('models/glm_sa_sc.rds')$model

# Plot effect of social connections and site fidelity on tracking NDVI
# Set up tiff
tiff("figures/S2_NDVItr_sa_sc.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(NDVItr_sa_sc, N = 6)
# dev.off
dev.off()

# Plot effect of social connections and site fidelity on tracking CWED
# Set up tiff
tiff("figures/S2_cwedtr_sa_sc.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(cwedtr_sa_sc, N = 6)
# dev.off
dev.off()

# Plot effect of familiarity on tracking NDVI
# Set up tiff
tiff("figures/S2_NDVItr_fl_sc.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(NDVItr_fl_sc, N = 6)
# dev.off
dev.off()

# Plot effect of familiarity on tracking CWED
# Set up tiff
tiff("figures/S2_cwedtr_fl_sc.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(cwedtr_fl_sc, N = 6)
# dev.off
dev.off()

# Plot effect of social connections and site fidelity on tracking NDVI
# Set up tiff
tiff("figures/S2_NDVItr_sc_sf.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(NDVItr_sc_sf, N = 6)
# dev.off
dev.off()

# Plot effect of social connections on risk perception
# Set up tiff
tiff("figures/S2_r_sc.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(r_sc, N = 6)
# dev.off
dev.off()

# Plot effect of social associations on risk perception
# Set up tiff
tiff("figures/S2_r_sa.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(r_sa, N = 6)
# dev.off
dev.off()

# Plot effect of social connections on social associations
# Set up tiff
tiff("figures/S2_sa_sc.tiff", width = 15, height = 20, units = 'cm', res = 300)
# Plot
plot(sa_sc, N = 6)
# dev.off
dev.off()


