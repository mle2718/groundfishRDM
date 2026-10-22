################################################################################
# Script:       wham_version_installer.R  (helpers)
# Purpose:      Installs the exact WHAM package versions that were used to fit the
#               accepted cod and haddock assessment models, each into its own
#               library location, so projections can switch WHAM versions by
#               pointing at an explicit lib path instead of reinstalling WHAM
#               every time.
# Inputs:       Accepted assessment .rds models on Google Drive:
#               cod: mod_base_2023_noBLLS.rds
#               haddock: mod_nola_dcpe_blls2.rds.
# Outputs:      Two installed WHAM builds under
#               R_LIBS_USER/{cod_wham_install, haddock_wham_install}.
# Dependencies: Requires a cached Drive token (.secrets) and internet access to
#               install WHAM from GitHub.
# Pipeline:     Standalone helper; not called by any wrapper.
################################################################################

library(tidyverse)
library(TMB)
library(haven)
library(glue)
library(googledrive)
library(here)
here::i_am("Code/helpers/wham_version_installer.R")

drive_auth(cache = here(".secrets"), email = TRUE)

################################################################################
################################################################################
# Section A: Housekeeping — per-model WHAM library locations
################################################################################
################################################################################
#Set paths, input names, and savefile names.

cod_wham_lib <- file.path(Sys.getenv("R_LIBS_USER"), "cod_wham_install")
haddock_wham_lib <- file.path(Sys.getenv("R_LIBS_USER"), "haddock_wham_install")

dir.create(file.path(cod_wham_lib), showWarnings = FALSE)
dir.create(file.path(haddock_wham_lib), showWarnings = FALSE)

# If the dir.create() lines fail with a "No such file or directory" error message then you might
#  not have R_LIBS_USER in .libPaths(). Check with:
#
#     Sys.getenv("R_LIBS_USER")
#     .libPaths()
#
#  Add R_LIBS_USER to .libPaths() with:
#
#     dir.create(file.path(Sys.getenv("R_LIBS_USER")), recursive=TRUE)
#         .libPaths(c(.libPaths(), file.path(Sys.getenv("R_LIBS_USER")))
#     # Verify that .libPaths() now has 2 things in it
#     Sys.getenv("R_LIBS_USER")
#     .libPaths()



################################################################################
################################################################################
# Section B: Cod — read the accepted model and install its WHAM version
################################################################################
################################################################################

# Read in accepted cod model

# this is the google drive location of the cod model
assessment_file_in<-"mod_base_2023_noBLLS.rds"
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","cod_assessment",assessment_file_in)
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id
# break if this matches more than 1 file.

stopifnot(length(file_id)==1)
temp_path <- tempfile(fileext = ".rds")

# Download
message("Downloading cod assessment model from Google Drive ...")
drive_download(
  file = as_id(file_id),
  path = temp_path,
  overwrite = TRUE
)

# Read in using  into your environment
mod_accepted <- read_rds(temp_path)

# Make sure the WHAM version used to generate the model is installed.

# The model stores its provenance string as "...@<commit>)"; pull out the commit
# hash (text after "@") and strip the trailing ")".
cod_wham_commit<-strsplit(mod_accepted$wham_commit,split="@")[[1]][2]
cod_wham_commit<-gsub(")", "", cod_wham_commit)



installed_cod_sha <- utils::packageDescription("wham",
                                                   lib.loc = cod_wham_lib,
                                                   fields = "RemoteSha")
skip_count<-0
install_count<-0
# install if it doesn't match.
if (installed_cod_sha!=cod_wham_commit){
  print(glue("Installing cod WHAM version {installed_cod_sha}. \n GitHub build; this can take a while ..."))
  remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), lib=cod_wham_lib, auth_token=NULL, upgrade="never")
  install_count<-install_count+1
} else {
  print(glue("Installed WHAM version {model_wham_commit} in cod directory matches assessment. Skipping ..."))
  skip_count<-skip_count+1
  }






################################################################################
################################################################################
# Section C: Haddock — read the accepted model and install its WHAM version
################################################################################
################################################################################

# read in the haddock assessment
assessment_file_in<-"mod_nola_dcpe_blls2.rds"
readin<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","haddock_assessment",assessment_file_in)
file_id<-drive_get(path = readin, shared_drive = "NMFS NEC READ SSB")$id

# break if this matches more than 1 file.
stopifnot(length(file_id)==1)

# Create a path for a temporary file
temp_path <- tempfile(fileext = ".rds")

# Download
message("Downloading haddock assessment model from Google Drive ...")
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
# Make sure the WHAM version used to generate the model is installed.

# The model stores its provenance string as "...@<commit>)"; pull out the commit
# hash (text after "@") and strip the trailing ")".
haddock_wham_commit<-strsplit(haddock_accepted$wham_commit,split="@")[[1]][2]
haddock_wham_commit<-gsub(")", "", haddock_wham_commit)


installed_haddock_sha <- utils::packageDescription("wham",
                                           lib.loc = haddock_wham_lib,
                                           fields = "RemoteSha")

# install if it doesn't match.
if (installed_haddock_sha!=haddock_wham_commit){
  print(glue("Installing haddock WHAM version {model_wham_commit}. \n GitHub build; this can take a while ..."))
  remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), lib=haddock_wham_lib, auth_token=NULL, upgrade="never")
  install_count<-install_count+1
} else {
  print(glue("Installed WHAM version {model_wham_commit} in haddock directory matches assessment. Skipping ..."))
  skip_count<-skip_count+1
  }


# Wrap up message
print(glue("Finished checking wham installation matches wham version used for assessment."))
print(glue("{install_count} wham installls changed.  {skip_count} installs untouched."))


