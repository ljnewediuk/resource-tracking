
# Figure out causal structure/variable conditioning for model

# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(rethinking)

# Define DAG
# TR = resource-tracking; SC = HR overlap; SA = social associations;
# E = environmental change; SF = site fidelity; FL = familiarity; R = risk percep.
dag <- dagify(
  TR ~ SC + SA + E + FL,
  FL ~ SC,
  SA ~ SC,
  FL ~ SA,
  SC ~ E,
  R ~ SC + SA,
  outcome = 'TR',
  coords = list(x = c(SC = 0, SA = 0, R = 0, TR = 2, E = 2, FL = 1),
                y = c(SC = 0, SA = 1, R = 2, TR = 2, E = 0, FL = 1))
)


# Plot
# tiff('figures/DAG_plot.tiff', width = 5, height = 4, units = 'in', res = 300)
# drawdag(dag)
ggdag(dag) + theme_void()

# dev.off()

# Required conditioning variables for combined effects of site fidelity and 
# HR overlap on resource tracking
adjustmentSets(dag, exposure = 'SC', outcome = 'TR')
# Required conditioning variables for effect of HR overlap on risk percep.
adjustmentSets(dag, exposure = 'SC', outcome = 'R')
# Required conditioning variables for effect of social associations on risk percep.
adjustmentSets(dag, exposure = 'SA', outcome = 'R')
# Required conditioning variables for effect of HR overlap on social associations
adjustmentSets(dag, exposure = 'SC', outcome = 'SA')
# Required Conditioning variables for effect of familiarity on tracking
adjustmentSets(dag, exposure = 'FL', outcome = 'TR')
# Required Conditioning variables for effect of familiarity on tracking
adjustmentSets(dag, exposure = 'SA', outcome = 'TR')

