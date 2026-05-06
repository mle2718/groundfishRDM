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
file_id<-"1A6p4yKLqL8vs0cTGz_3KWCpwi71ltbER"
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








# I have hard-coded the id, just to save some time.  But if you want to search for the file, uncomment the two lines immediately following.
file_id<-"1pPGqMBJXUnFxnc17JlVjetkRKONTxEM-"
# readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","haddock_assessment",assessment_file_in)
# file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

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

