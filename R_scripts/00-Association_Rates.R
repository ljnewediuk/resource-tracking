
# The purpose of this supplement is to calculate 5-min/50 m association rates
# between elk in 2019 and 2020 to make sure they follow association patterns
# using home ranges.

# Load packages
library(data.table)
library(tidyverse)
library(sf)
library(spatsoc)
library(asnipe)
library(igraph)

# Get HR association rates data
HR_assoc_2019 <- readRDS('output/soc_fidel_dat_yr1.rds') %>%
  filter(period == 'pre-calv',
         year == 2019)

HR_assoc_2020 <- readRDS('output/soc_fidel_dat_yr2.rds') %>%
  filter(period == 'pre-calv',
         year == 2020)

# Read data and convert to data.table
dat <- readRDS('input/vita_elk_vectronic_feb_2019-march_2021_cleaned.rds') %>%
  # Transform to projected CRS
  st_transform(crs = st_crs(26914))

DT <- dat %>%
  # Get coordinates as column
  mutate(lat = st_coordinates(dat)[, 2],
         long = st_coordinates(dat)[, 1],
         # Cast datetime column to POSIXct
         dat_time = as.POSIXct(dat_time),
         # Calculate the year of the relocation 
         yr = lubridate::year(dat_time),
         day = lubridate::yday(dat_time)) %>%
  # Subset days in correct period
  filter(day %in% c(122, 213))  %>%
  # Drop geometry and convert to data.table
  st_drop_geometry() %>%
  as.data.table()

# Temporal groups
DT <- group_times(DT, datetime = 'dat_time', threshold = '5 minutes')

# Spatial groups
gbiMtrx <- group_pts(DT, threshold = 50, id = 'animal_ID', 
                  coords = c('long', 'lat'), timegroup = 'timegroup') %>%
  get_gbi(group = 'group', id = 'animal_ID')

# Subset DT to only year 2019
subDT <- DT[yr == 2019]

# Generate network
net <- get_network(gbiMtrx,
                   data_format = "GBI",
                   association_index = "SRI")

# Generate graph
g <- graph.adjacency(net, 'lower', 
                     diag = FALSE, weighted = TRUE)

# Metrics for all individuals 
observed <- data.table(
  loc_strength = graph.strength(g),
  animal_ID = names(graph.strength(g))
)

# Join data
associations <- HR_assoc_2019 %>%
  left_join(observed)

# Save association rates data
saveRDS(associations, 'derived_data/associations.rds')

# Plot SRI graph strength vs. home range graph strength
ggplot(associations, aes(x = i_strength, y = loc_strength)) + 
  geom_point() + 
  geom_smooth(method = 'lm', colour = 'black') +
  ylab('SRI graph strength') +
  xlab('Home range overlap graph strength') +
  theme(panel.background = element_rect(fill = 'white'),
        axis.title.x = element_text(size = 20, colour = 'black',  vjust = -4),
        axis.title.y = element_text(size = 20, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        axis.line.x = element_line(colour = 'black', size = .75),
        axis.line.y = element_line(colour = 'black', size = .75),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm')) 

# Save plot for MS figures
ggsave('figures/sri_vs_hr.tiff', plot = last_plot(), 
       dpi = 300, width = unit(7, 'cm'), height = unit(7, 'cm'), device = 'tiff')



