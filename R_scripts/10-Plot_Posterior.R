
library(tidyverse)
library(cowplot)

# Call function to fit plots
source('R_functions/Sample_Posterior_Plots.R')

# Get list of model files for m_name argument
list.files('models/', pattern = '^[g]')

# Load raw data
dat <- readRDS('derived_data/model_dat.rds') %>%
  # Subset calving period
  filter(risk == 'low') 

# 1 - Effect of social connections on resource-tracking
#   - Model: TR ~ SF*SC + E
#     
# No effect of social connections on NDVI tracking
tr_sc_ndvi <- plot_glm(y = 'TR', a = 'SC', b = 'E', m_name = 'glm_NDVItr_sc') + 
  theme(strip.text = element_text(size = 18, colour = 'black'),
        strip.background = element_rect(colour = NA, fill = 'white'),
        plot.tag = element_text(size = 20)) +
  xlab('Shared spatial overlap') +
  ylab("NDVI tracking") +
  labs(tag = '(a)') 
# Add optional facet
  # facet_wrap(~ SC, labeller = 
  #              labeller(SC = c(`-2` = 'Low home range overlap', 
  #                              `2` = 'High home range overlap')))
# No effect of social connections on CWED tracking
tr_sc_cwed <- plot_glm(y = 'TR', a = 'SC', b = 'E', m_name = 'glm_cwedtr_sc') + 
  theme(strip.text = element_blank(),
        plot.tag = element_text(size = 20)) +
  xlab('Shared spatial overlap') +
  ylab("CWED tracking") +
  labs(tag = '(b)')

# Summarize credible intervals
# NDVI tracking
posterior_interval(readRDS('models/glm_NDVItr_sc.rds')$model, prob = 0.89)
# CWED tracking
posterior_interval(readRDS('models/glm_cwedtr_sc.rds')$model, prob = 0.89)

# Plot grid
plot_grid(tr_sc_ndvi, tr_sc_cwed, align = 'h', axis = 'bt', ncol = 2)

# Save
ggsave('figures/tr_sc.tiff', plot = last_plot(), 
       dpi = 300, width = unit(10, 'cm'), height = unit(7, 'cm'), device = 'tiff')

# 2 - Effect of home range overlap/social associations on risk perception
#   - Model: R ~ SC, R ~ SA + SC
#     
# Animals with greater home range overlap perceive less risk, suggesting possible
# benefit of social connection is reducing stress even if it does not improve 
# resource tracking
r_sc <- plot_glm(y = 'R', a = 'SC', m_name = 'glm_r_sc') +
  ylab('Fecal glucocorticoids') +
  xlab('Shared spatial overlap') +
  labs(tag = '(a)') +
  ylim(-4, 4) +
  theme(plot.tag = element_text(size = 20))
# Social associations (SRI) have no effect on risk perception, suggesting diminishing
# returns of associating too closely; density may reduce risk perception, but
# direct association may increase competition for food, increasing GCs
r_sa <- plot_glm(y = 'R', a = 'SA', b = 'SC', m_name = 'glm_r_sa') +
  ylab('') +
  xlab('Social associations') + 
  labs(tag = '(b)') +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.tag = element_text(size = 20))

# Summarize credible intervals
# Home range overlap
posterior_interval(readRDS('models/glm_r_sc.rds')$model, prob = 0.89)
# Social associations
posterior_interval(readRDS('models/glm_r_sa.rds')$model, prob = 0.89)

# Plot grid
plot_grid(r_sc, r_sa, align = 'h', axis = 'bt', ncol = 2)

# Save
ggsave('figures/r_sc_sa.tiff', plot = last_plot(), 
       dpi = 300, width = unit(10, 'cm'), height = unit(7, 'cm'), device = 'tiff')

# 3 - Effect of home range overlap on social associations
#   - Model: SA ~ SC
#     
# Animals with greater home range overlap do not also have more social associations,
# suggesting individuals that overlap spatially do not share social information
plot_glm(y = 'SA', a = 'SC', m_name = 'glm_sa_sc') +
  ylab('Social associations') +
  xlab('Shared spatial overlap')

# Summarize credible intervals
posterior_interval(readRDS('models/glm_sa_sc.rds')$model, prob = 0.89)

# Save
ggsave('figures/sa_sc.tiff', plot = last_plot(), 
       dpi = 300, width = unit(7, 'cm'), height = unit(7, 'cm'), device = 'tiff')

# 4 - Effect of familiarity and social associations on resource-tracking
#     Models: TR ~ FL + SC, TR ~ SA + SC
#     
# Social associations have no effect on tracking NDVI
tr_sa_ndvi <- plot_glm(y = 'TR', a = 'SA', b = 'SC', m_name = 'glm_NDVItr_sa_sc') +
  theme(plot.tag = element_text(size = 20)) +
  ylab('NDVI tracking') +
  xlab('Social associations') +
  ylim(-3.2, 3.4) +
  labs(tag = '(a)')
# However, familiarity improves tracking of NDVI, suggesting even though social
# associations might not help you track resources, they help when you maintain 
# the same social connections between years (lower FL values = greater familiarity)
tr_fl_ndvi <- plot_glm(y = 'TR', a = 'FL', b = 'SC', m_name = 'glm_NDVItr_fl_sc') +
  theme(plot.tag = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab('NDVI tracking') +
  xlab('Familiarity') +
  ylim(-3.2, 3.4) +
  labs(tag = '(b)') +
  scale_x_reverse()

# Plot grid
plot_grid(tr_sa_ndvi, tr_fl_ndvi, align = 'h', axis = 'bt', ncol = 2)

# Summarize credible intervals
# NDVI tracking familiarity
posterior_interval(readRDS('models/glm_NDVItr_fl_sc.rds')$model, prob = 0.89)
# CWED tracking familiarity
posterior_interval(readRDS('models/glm_cwedtr_fl_sc.rds')$model, prob = 0.89)

# NDVI tracking social associations
posterior_interval(readRDS('models/glm_NDVItr_sa_sc.rds')$model, prob = 0.89)
# CWED tracking social associations
posterior_interval(readRDS('models/glm_cwedtr_sa_sc.rds')$model, prob = 0.89)

# Save
ggsave('figures/tr_fl_sa_sc.tiff', plot = last_plot(), 
       dpi = 300, width = unit(11, 'cm'), height = unit(6, 'cm'), device = 'tiff')

