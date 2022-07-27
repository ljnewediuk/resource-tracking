
#################################################
### Function to calculate contrast-weighted   ###
### edge density of raster using              ###
### landscapemetrics functions                ###
###                                           ###
### CWED is the "sum of the lengths (m) of    ###
### each edge segment in the landscape        ###
### multiplied by the corresponding contrast  ###
### weight, divided by the total landscape    ###
### area (m2), multiplied by 10,0000 (to      ###
### convert to hectares).                     ###
###                                           ###
### Returns a contrast-weighted edge density  ###
### value for the landscape                   ###
###                                           ###
#################################################

# Required:
#   landsc: stars/raster object, 
#   c_weights: contrast weights for each class in 
#              landsc, corresponding to the 
#              dissimilarities between classes

lsm_l_cwed <- function(landsc, c_weights) {
    
  # Calculate total area of landscape
  t_area <- lsm_l_ta(landsc)$value
  
  # Re-define new landscapes with only classes i and k
  # Get unique classes in landscape
  classes <- suppressWarnings(get_unique_values(landsc)[1][[1]])
  
  # Calculate weighted edges between all classes
  cw_edges <- data.frame()
  for(cl in 1:length(classes)) {
    # Get comparison class
    i <- classes[cl]
    # Repeat loop for all classes not in the comparison class
    for(k in classes[! classes %in% i]) {
      # New landscape with only two comparison values
      landsc_ik <- landsc
      landsc_ik[! landsc_ik %in% c(i, k)] <- NA
      # Check unique values are only in two classes
      suppressWarnings(get_unique_values(landsc_ik)[1][[1]])
      # Calculate edge length
      edge_ln <- suppressMessages(lsm_c_te(landsc_ik, 
                                           count_boundary = F, directions = 8)  %>%
        left_join(c_weights))
      # Calculate difference in weights
      diss <- abs(edge_ln[1 ,]$weight - edge_ln[2 ,]$weight)
      # Get weighted edge densities
      edges <- edge_ln %>%
        mutate(diss) %>%
        filter(class == i) %>%
        mutate(compar = k,
               CWED = value * diss) %>%
        select(class, compar, CWED)
      # Bind together
      cw_edges <- rbind(cw_edges, edges)
      
    }
  }
  
  # Calculate contrast weighted edge density of landscape 
  # (sum of contrasts/total landscape area)
  cwed <- sum(cw_edges$CWED, na.rm = T)/t_area
  # Make tibble to return
  results <- 
    tibble::tibble(
      level = "landscape",
      class = as.integer(NA),
      id = as.integer(NA),
      metric = "cwed",
      value = cwed)

  return(results)
  
}


