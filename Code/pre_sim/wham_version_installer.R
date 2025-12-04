# WHAM_version_helper
# Running these projections requires different versions of WHAM. You could install a new version of wham every time
# you want to do a projection.

# Or you could install these and switch using an explicit call to the library location.
# this is code to help with that second thing.


library(tidyverse)
library(TMB)
library(haven)
library(glue)

###########Begin Housekeeping##################################################
#Set paths, input names, and savefile names.

BLAST_root<-file.path("//nefscfile","BLAST","READ-SSB-Lee-BLAST")
cod_input_folder<-file.path(BLAST_root,"cod_haddock_fy2025","source_data","cod","input")
haddock_input_folder<-file.path(BLAST_root,"cod_haddock_fy2025","source_data","haddock","input")

cod_wham_lib <- file.path(Sys.getenv("R_LIBS_USER"), "cod_wham_install")
haddock_wham_lib <- file.path(Sys.getenv("R_LIBS_USER"), "haddock_wham_install")

dir.create(file.path(cod_wham_lib), showWarnings = FALSE)
dir.create(file.path(haddock_wham_lib), showWarnings = FALSE)


# Read in accepted cod model
mod_accepted <-
  readRDS(file = file.path(cod_input_folder,"mod_base_2023_noBLLS.rds"))

###################################################################################
###################################################################################
#Make sure that the version of WHAM that was used to generate the model is installed
###################################################################################
###################################################################################

# take a look at the version of WHAM used to generate the model.
model_wham_commit<-strsplit(mod_accepted$wham_commit,split="@")[[1]][2]
model_wham_commit<-gsub(")", "", model_wham_commit)


remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), lib=cod_wham_lib, auth_token=NULL , upgrade="never")










# Read in accepted haddock  model
haddock_accepted <-
  readRDS(file = file.path(haddock_input_folder,"mod_nola_dcpe_blls2.rds"))

###################################################################################
###################################################################################
#Make sure that the version of WHAM that was used to generate the model is installed
###################################################################################
###################################################################################

# take a look at the version of WHAM used to generate the model.
model_wham_commit<-strsplit(haddock_accepted$wham_commit,split="@")[[1]][2]
model_wham_commit<-gsub(")", "", model_wham_commit)


remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), lib=haddock_wham_lib, auth_token=NULL, upgrade="never")

