
#################################################
###                                           ###
### Resample Vectronic data to relocation     ###
### rate matching lotek data                  ###
###                                           ###
### Checks input folder and runs cleaning     ###
### if necessary files are not present        ###
###                                           ###
#################################################

# List files in input
list.files('input/')

# If the files are missing, resample the data
if(!"vita_elk_vectronic_feb_2019-march_2021_12_hr.rds" %in% 
   list.files('input/')) {
  
  # Source the function to resample the data
  source('R_functions/Subsample_GPS_Data.R')
  
  # Load the cleaned location data
  dat <- readRDS('input/vita_elk_vectronic_feb_2019-march_2021_cleaned.rds')
  
  # Subsample the data to 12 hr relocation rate starting at random row
  subd_dat <- subsample_gps_data(dat = dat, subsamp = 12, subsamp_unit = 'hours')
  
  # Save resampled data in input folder
  saveRDS(subd_dat, 'input/vita_elk_vectronic_feb_2019-march_2021_12_hr.rds')
  
} else {
  cat('Resampled data already present in input folder')
}

