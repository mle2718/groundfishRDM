#This code reads in a catch per trip dta for the rec dashboard and uploads it to Google drive as an Rds


#Load libraries
library(tidyverse)
library(haven)
library(glue)
library(googledrive)
library(here)


here::i_am("Code/pre_sim/rdb_catch_per_trip_to_drive.R")
output_folder<-here("Data", "miscellaneous")

input_file <- here("Data","miscellaneous","rdb_sim_catch_per_trip.dta")

# Read in my .dta file
rdb_catch_per_trip <- read_dta(input_file)

# Save the data version
file_date <- rdb_catch_per_trip$data_version[1]
SimCPTSaveFile<-glue("rdb_catch_per_trip_{file_date}")

# convert character date to a date variable
rdb_catch_per_trip$data_version<-as.Date(rdb_catch_per_trip$data_version)

# Save dataframe as Rds
write_rds(rdb_catch_per_trip, file=file.path(output_folder,glue("{SimCPTSaveFile}.Rds")))


# Connect to Google Drive
# NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
drive_auth(cache = here(".secrets"), email = TRUE)

# Output folder on google drive
miscellaneous_path <-file.path("socialsci","RecreationalDST","2027_management_cycle_data",
                               "groundfishRDM","miscellaneous")

folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)
miscellaneous_path<-folder_info$id


#Put the catch per trip Rds on google drive
drive_upload(
  media = file.path(output_folder,glue("{SimCPTSaveFile}.Rds")),
  path = as_id(miscellaneous_path),
  name = glue("{SimCPTSaveFile}.Rds"),
  overwrite = TRUE
)


