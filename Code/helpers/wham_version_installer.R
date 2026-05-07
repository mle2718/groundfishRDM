# WHAM_version_helper
# Running these projections requires different versions of WHAM. You could install a new version of wham every time
# you want to do a projection.

# Or you could install these and switch using an explicit call to the library location.
# this is code to help with that second thing.


library(tidyverse)
library(TMB)
library(haven)
library(glue)
library(googledrive)
library(here)
here::i_am("Code/helpers/wham_version_installer.R")

drive_auth(cache = here(".secrets"), email = TRUE)

###########Begin Housekeeping##################################################
#Set paths, input names, and savefile names.

cod_wham_lib <- file.path(Sys.getenv("R_LIBS_USER"), "cod_wham_install")
haddock_wham_lib <- file.path(Sys.getenv("R_LIBS_USER"), "haddock_wham_install")

dir.create(file.path(cod_wham_lib), showWarnings = FALSE)
dir.create(file.path(haddock_wham_lib), showWarnings = FALSE)


# Read in accepted cod model

# this is the google drive location of the cod model
assessment_file_in<-"mod_base_2023_noBLLS.rds"
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","cod_assessment",assessment_file_in)
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

temp_path <- tempfile(fileext = ".rds")

# Download
drive_download(
  file = as_id(file_id),
  path = temp_path,
  overwrite = TRUE
)

# Read in using  into your environment
mod_accepted <- read_rds(temp_path)

###################################################################################
###################################################################################
#Make sure that the version of WHAM that was used to generate the model is installed
###################################################################################
###################################################################################

# take a look at the version of WHAM used to generate the model.
model_wham_commit<-strsplit(mod_accepted$wham_commit,split="@")[[1]][2]
model_wham_commit<-gsub(")", "", model_wham_commit)


remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), lib=cod_wham_lib, auth_token=NULL , upgrade="never")






# read in the haddock assessment
assessment_file_in<-"mod_nola_dcpe_blls2.rds"
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","haddock_assessment",assessment_file_in)
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id


# Create a path for a temporary file
temp_path <- tempfile(fileext = ".rds")

# Download
drive_download(
  file = as_id(file_id),
  path = temp_path,
  overwrite = TRUE
)

# Read in using  into your environment
haddock_accepted <- read_rds(temp_path)
# cleanup
if (file.exists(temp_path)) {
  file.remove(temp_path)
}
###################################################################################
###################################################################################
#Make sure that the version of WHAM that was used to generate the model is installed
###################################################################################
###################################################################################

# take a look at the version of WHAM used to generate the model.
model_wham_commit<-strsplit(haddock_accepted$wham_commit,split="@")[[1]][2]
model_wham_commit<-gsub(")", "", model_wham_commit)


remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), lib=haddock_wham_lib, auth_token=NULL, upgrade="never")

