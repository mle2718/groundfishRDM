### Run this Lou

library(data.table)
library(fst)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(here)
library(furrr)
library(future)
conflicts_prefer(data.table::month)
# Optional parallel backend is loaded only in the wrapper below.

final_process_data_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle"
final_process_outcomes_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/base_outcomes"
final_process_choice_occasions_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/n_choice_occasions"
final_process_misc_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/miscellaneous"
final_process_calib_catch_cd="E:/Lou_projects/groundfishRDM/2027_mgt_cycle/calib_catch_draws"

# -----------------------------------------------------------------------------
# User-facing controls
# -----------------------------------------------------------------------------
draws         <- 1:5
n_simulations <- 5
mode_draw     <- c("pr", "fh")
season_draw   <- c("summer", "winter")
draws         <- if (exists("draws")) draws else seq_len(n_simulations)
n_draws       <- 50L

source(here::here("Code/sim/predict_rec_catch_functions.R"))
# Length-weight parameters from the calibration script.
cod_lw_a <- if (exists("cod_lw_a")) cod_lw_a else 0.000005132
cod_lw_b <- if (exists("cod_lw_b")) cod_lw_b else 3.1625
had_lw_a <- if (exists("had_lw_a")) had_lw_a else 0.000009298
had_lw_b <- if (exists("had_lw_b")) had_lw_b else 3.0205

directed_trips <- as.data.table(read_fst(file.path(final_process_misc_cd,"directed_trip_draws.fst")))

# -----------------------------------------------------------------------------
# Main projection execution
# -----------------------------------------------------------------------------

# In an Azure Shiny app, set n_workers from an environment variable or app option,
# e.g. Sys.getenv("RDM_N_WORKERS", unset = parallel::detectCores(logical = FALSE) - 1).
use_parallel <- TRUE
n_workers <- 4   # or however many Azure workers/cores you want available


## Run Model in parallel

n_workers <- if (exists("n_workers")) n_workers else max(1L, parallel::detectCores(logical = FALSE) - 1L)
use_parallel <- if (exists("use_parallel")) use_parallel else TRUE

system.time({
  prediction_draws <- run_cod_hadd_projection(
    season_draw  = season_draw,
    mode_draw    = mode_draw,
    draws        = draws,
    n_workers    = n_workers,
    use_parallel = use_parallel,
    common_inputs = NULL
  )
})

