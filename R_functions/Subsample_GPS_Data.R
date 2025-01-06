
#################################################
### Subsamples a sf object of GPS location    ###
### data to a new relocation rate             ###
###                                           ###
### Returns a sf object                       ###
###                                           ###
#################################################

# Required columns:
#   animal_ID, dat_time
# Variables:
#   dat: dataframe object, 
#   subsamp: fix rate to subample to (numeric)
#   subsamp_unit: 'hours' or 'mins', 
#   start_row: row where the resampling should start (if not provided, starts
#              at randomly selected row between 1 and value of subsamp)

# Load required packages
library(tidyverse)
library(sf)

# Function
subsample_gps_data <- function(dat, subsamp, subsamp_unit, start_row = NULL) {
  
  # Create data frame to store individuals
  subsample_dat <- data.frame()
  
  # Round hours (or minutes depending on units)
  dat_round <- dat %>% 
    mutate(round_dat = round(dat_time, units = subsamp_unit))
  
  # Run loop for each individual
  for(j in unique(dat_round$animal_ID)) {
    cat('Animal:', j, '\n', sep=' ')
    # Filter out the individual
    ID_dat <- dat_round %>% 
      filter(animal_ID == j)
    # Remove rows prior to start row
    if(is.null(start_row)) start_row <- sample(1:subsamp, 1)
    ID_dat <- ID_dat[ - c(1:c(start_row-1)) ,]
    start_row <- NULL
    # Initiate progress bar
    prog_bar <- txtProgressBar(min = 1, max = nrow(ID_dat))
    # Resample fixes
    for(i in 1:nrow(ID_dat)) {
      setTxtProgressBar(prog_bar, i)
      # Calculate interval between fix i and next fix
      fix_interval <- as.numeric(difftime(ID_dat[i + 1,]$round_dat, 
                                          ID_dat[i ,]$round_dat,
                                          units = subsamp_unit))
      # If fix interval is.na, break
      if(is.na(fix_interval)) break
      # If the interval is less than the subsample rate...
      if(fix_interval < subsamp) {
        # Repeat removing next row and recalculating difftime...
        repeat {
          ID_dat <- ID_dat[- c(i+1) ,]
          fix_interval <- as.numeric(difftime(ID_dat[i + 1,]$round_dat, 
                                              ID_dat[i ,]$round_dat,
                                              units = subsamp_unit))
          # ...until fix_interval > subsamp (or is.na(fix_interval))
          if(fix_interval >= subsamp | is.na(fix_interval)) break
        }
      }
      # Break loop if is.na(fix_interval) or at end of data frame
      if(is.na(fix_interval) | i >= nrow(ID_dat)) break
      close(prog_bar)
    }
    cat('\n\n')
    # Bind individual data frames
    subsample_dat <- rbind(subsample_dat, ID_dat)
  }
  cat('Done.\n\n')
  
  # Remove 'round_dat' column before returning output
  final_dat <- subsample_dat %>% select(-round_dat)
  
  return(final_dat)
  
}


