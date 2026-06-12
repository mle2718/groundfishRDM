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








##CLEAN UP THE REST


# DONT THINK WE WANT THIS Output folder on google drive
rdb_gf_processed_path<-file.path("RecDST","recDST data dashboard","input_data","cod_haddock")
folder_info <- drive_get(
  path = rdb_gf_processed_path,
  shared_drive = "RecDST"
)

#THIS I THINK
# Output folder on google drive
miscellaneous_path <-file.path("socialsci","RecreationalDST","2027_management_cycle_data",
                               "groundfishRDM","miscellaneous")

folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)
miscellaneous_path<-folder_info$id



## THIS is for reading things in FROM google drive
# input save files
file_in<-"rdb_sim_catch_per_trip.dta"

#read in the assessment file
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","cod_assessment",assessment_file_in)
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id


# MIN YanG's Output folder on google drive
groundfish_processed_path<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data")
folder_info <- drive_get(
  path = groundfish_processed_path,
  shared_drive = "NMFS NEC READ SSB"
)
groundfish_processed_path<-folder_info$id


# save "$misc_data_cd\rdb_sim_catch_per_trip.dta", replace


write_dta(NAA_long, path=file.path(output_folder,glue("{ProjectedNAASaveFile}.dta")))
write_rds(NAA_long, file=file.path(output_folder,glue("{ProjectedNAASaveFile}.Rds")))

### Example


write_dta(NAA_long, path=file.path(assessment_output_folder,glue("{ProjectedNAASaveFile}.dta")))
write_rds(NAA_long, file=file.path(assessment_output_folder,glue("{ProjectedNAASaveFile}.Rds")))

#Put the historical NAA on google drive
drive_upload(
  media = file.path(assessment_output_folder,glue("{ProjectedNAASaveFile}.Rds")),
  path = as_id(groundfish_processed_path),
  name = glue("{ProjectedNAASaveFile}.Rds"),
  overwrite = TRUE
)

drive_upload(
  media = file.path(assessment_output_folder,glue("{ProjectedNAASaveFile}.dta")),
  path = as_id(groundfish_processed_path),
  name = glue("{ProjectedNAASaveFile}.dta"),
  overwrite = TRUE
)

