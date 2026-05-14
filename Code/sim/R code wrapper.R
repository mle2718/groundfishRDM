
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
              "ggplot2","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
              "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
              "purrr", "readr", "here", "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

#Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }

lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

#There are four folders needed::
#input data - contains all the MRIP, biological data, angler characteristics data, as well as some data generated in the simulation
#code - contains all the model code
#output_data - this folder is empty to begin with. It stores final simulation output
#iterative_data -this folder is empty to begin with. It compiles data generated in the simulation

#Need to ensure that the globals below are set up in both this file and the stata model_wrapper.do file.

#Set up R globals for input/output data and code scripts
code_cd=here("Code", "sim")
# input_data_cd="E:/Lou_projects/groundfishRDM/input_data"
# iterative_input_data_cd="E:/Lou_projects/groundfishRDM/process_data"
#
# final_process_data_cd="E:/Lou_projects/groundfishRDM/process_data"
# final_process_outcomes_cd="E:/Lou_projects/groundfishRDM/process_data/base_outcomes"
# final_process_choice_occasions_cd="E:/Lou_projects/groundfishRDM/process_data/n_choice_occasions"
# final_process_misc_cd="E:/Lou_projects/groundfishRDM/process_data/miscellaneous"


final_process_data_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle"
final_process_outcomes_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/base_outcomes"
final_process_choice_occasions_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/n_choice_occasions"
final_process_misc_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/miscellaneous"
final_process_calib_catch_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/calib_catch_draws"


n_simulations<-1 # Number of model iterations
n_draws<-50 # Number of simulated trips per day


# helpers
parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

#First, open "$code_cd\model wrapper.do" and set globals:
#a) data years for different datasets
#b) number of draws (ndraws), which should be the same as the object n_simulations above
#c) cd's

#Second, open "$code_cd\set regulations.do" and set regulations for the calibration and projection period.

#Third, run the model wrapper code below:
#stata('do "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/model wrapper.do"')

###################################################




###################################################
###############Simulation R code###################
###################################################

#Notes:

#Simulation stratum are the groups in which we allocate and simulate choice occasions.
#For the 2025 SFSBSB RDM, the stratum is the combination of mode (pr/fh/sh) and state

#Projection results are based on 100 iterations of the model. In each iteration we pull
#in new distributions of catch-per-trip, directed fishing effort, projected catch-at-length,
#and angler preferences. I calibrate the model with 125 iterations, some of which are
#excluded after Step 2. From the pool of remaining iterations, I use the first 100 in the projection.

#Prior to running the model, transfer some files from .csv to .fst to reduce computing time
dtrip0<-read.csv(file.path(final_process_misc_cd, paste0("directed_trip_draws.csv"))) %>%
  dplyr::mutate(date_parsed = parse_date_any(day),
                month=data.table::month(date_parsed)) %>%
  dplyr::select(-day, -day_y2)

write_fst(dtrip0, file.path(final_process_misc_cd, paste0("directed_trip_draws.fst")))

for(i in 1:n_simulations) {

    catch0<-read_dta(file.path(final_process_calib_catch_cd, paste0("calib_catch_draws_", i,".dta"))) %>%
      dplyr::mutate(date_parsed = parse_date_any(date),
                    month=data.table::month(date_parsed)) %>%
      dplyr::select(-date)

    write_fst(catch0, file.path(final_process_calib_catch_cd, paste0("calib_catch_draws_", i,".fst")))

  }

disc_mort<- readr::read_csv(file.path(final_process_misc_cd, "Discard_Mortality.csv"), show_col_types = FALSE)
write_fst(disc_mort, file.path(final_process_misc_cd, paste0("Discard_Mortality.fst")))

calendar_adj<- readr::read_csv(file.path(final_process_misc_cd, "next_year_calendar_adjustments.csv"), show_col_types = FALSE)
write_fst(calendar_adj, file.path(final_process_misc_cd, paste0("calendar_adj.fst")))


##################### STEP 1 #####################
# Run the calibration algorithm to determine the difference between model-based harvest and MRIP-based harvest.
# I do this for each stratum and each stratum's 125 draws of MRIP trips/catch/harvest.
# This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species.

source(file.path(code_cd,"calibrate_rec_catch0_rewrite.R"))

#Output files:
#calibration_comparison.fst


##################### STEP 2 #####################
#Now, run each stratum's calibration simulation again, but this time allocate discards to harvest,
#or harvest to discards, until the difference between model-based harvest and MRIP-based harvest
#is within abs(5%) or <500 fish.

#If a reallocation of discards to harvest is needed, I reallocate h* percent of all released
# fish that are between [(min. size - 3 inches), min.size] as harvest.

#If a reallocation of harvest to discards is needed, I reallocate h* percent of all harvested fish as discards

#Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 is too large relative to the number of
#fish discarded in the model. So even if I allocate all discards as harvest, the percent difference in harvest
#between the model and MRIP will not be within abs(5%) or <500 fish. The code in Step 2 identifies and drops these iterations.

#This script saves calibration output, as well as the proportion of choice occasions in which we reallocate discards as harvest,
#or vice versa.

#Files needed:

#Scripts needed:
#calibration_catch_weights.R - can be commented out to save time if calibration catch weight are not needed.

source(file.path(code_cd,"calibration_routine_rewrite.R"))

#Output files:
#calibrated_model_stats.fst
#file.path(final_process_choice_occasions_cd,paste0("n_choice_occasions_", s, "_", md, "_", i,".fst"))
#file.path(final_process_outcomes_cd, paste0("base_outcomes_", s, "_", md, "_", i, ".fst"))


##################### STEP 3 #####################
#Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates
#fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration.

source(file.path(code_cd, "predict_rec_catch_data_read.R"))
source(file.path(code_cd, "predict_rec_catch_data_functions.R"))
source(file.path(code_cd, "predict_rec_catch.R"))

#Output files:
# predictions.xlsx - output by mode, season, and draw









