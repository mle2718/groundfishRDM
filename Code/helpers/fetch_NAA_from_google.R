# Code to pull stock assessment (historical and projected) off google drive
# you will run this occassionally.

library(tidyverse)
library(glue)
library(googledrive)
library(here)

here::i_am("Code/helpers/fetch_NAA_from_google.R")

data_version<-"2026-05-06"
# Connect to Google Drive
drive_auth(cache = here(".secrets"), email = TRUE)

###################################################################################
# GET COD
###################################################################################
###################################################################################
ProjectedNAASaveFile<-glue("WGOM_Cod_projected_NAA_from_2024Assessment_{data_version}")
HistoricalNAASaveFile<-glue("WGOM_Cod_historical_NAA_from_2024Assessment_{data_version}")

# Get the historical NAA file
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data",glue("{HistoricalNAASaveFile}.Rds"))
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

# change the path to save this to a different place.
#path = here("input_data2",glue("{HistoricalNAASaveFile}.Rds")) saves it to the folder input_data2 inside the repository

# Download
drive_download(
  file = as_id(file_id),
  path = here("input_data2",glue("{HistoricalNAASaveFile}.Rds")),
  overwrite = TRUE
)


# Get the projected NAA file
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data",glue("{ProjectedNAASaveFile}.Rds"))
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

# Download
drive_download(
  file = as_id(file_id),
  path = here("input_data2",glue("{ProjectedNAASaveFile}.Rds")),
  overwrite = TRUE
)


###################################################################################
# GET HADDOCK
###################################################################################
###################################################################################
ProjectedNAASaveFile<-glue("GOM_Haddock_projected_NAA_2024Assessment_{data_version}")
HistoricalNAASaveFile<-glue("GOM_Haddock_historical_NAA_2024Assessment_{data_version}")

# Get the historical NAA file
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data",glue("{HistoricalNAASaveFile}.Rds"))
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

# Download
drive_download(
  file = as_id(file_id),
  path = here("input_data2",glue("{HistoricalNAASaveFile}.Rds")),
  overwrite = TRUE
)





# Get the projected NAA file
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data",glue("{ProjectedNAASaveFile}.Rds"))
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

# Download
drive_download(
  file = as_id(file_id),
  path = here("input_data2",glue("{ProjectedNAASaveFile}.Rds")),
  overwrite = TRUE
)

