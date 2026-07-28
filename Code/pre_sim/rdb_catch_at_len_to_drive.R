#This code reads in a catch at length dta for the recDST data dashboard and uploads it to Google drive as an Rds


#Load libraries
library(tidyverse)
library(haven)
library(glue)
library(googledrive)
library(here)

here::i_am("Code/pre_sim/rdb_catch_at_len_to_drive.R")
source(here("Code", "helpers", "developer_setup.R"))

output_folder<-file.path(gf.data.dir, "miscellaneous")
vintage_string<-list.files(output_folder, pattern=glob2rx("mrip_pull*Rds"))
vintage_string<-gsub("mrip_pull","",vintage_string)
vintage_string<-gsub(".Rds","",vintage_string)
data_vintage<-max(vintage_string)

input_file <- file.path(gf.data.dir,"miscellaneous","rdb_cat_len.dta")

# Read in my .dta file
rdb_catch_at_length <- read_dta(input_file)
rdb_catch_at_length$data_version <- data_vintage

# Save the data version
file_date <- rdb_catch_at_length$data_version[1]
SimCPTSaveFile<-glue("rdb_catch_at_length_{file_date}")

# convert character date to a date variable
rdb_catch_at_length$data_version<-as.Date(rdb_catch_at_length$data_version)

# Save dataframe as Rds
write_rds(rdb_catch_at_length, file=file.path(output_folder,glue("{SimCPTSaveFile}.Rds")))


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


