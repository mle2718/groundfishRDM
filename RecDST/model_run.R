

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

final_process_data_cd=here::here("Data")
final_process_outcomes_cd=here::here("Data/base_outcomes")
final_process_choice_occasions_cd=here::here("Data/n_choice_occassions")
final_process_misc_cd=here::here("Data/miscellaneous")
final_process_calib_catch_cd=here::here("Data/calib_catch_draws")

# -----------------------------------------------------------------------------
# User-facing controls
# -----------------------------------------------------------------------------
draws         <- 1:5
n_simulations <- 5
mode_draw     <- c("pr", "fh")
season_draw   <- c("summer", "winter")
draws         <- if (exists("draws")) draws else seq_len(n_simulations)
n_draws       <- 50L

#policy_name <- args[1]
policy_name <- "SQactual"


source(here::here("Code/sim/predict_rec_catch_functions.R"))
# Length-weight parameters from the calibration script.
cod_lw_a <- if (exists("cod_lw_a")) cod_lw_a else 0.000005132
cod_lw_b <- if (exists("cod_lw_b")) cod_lw_b else 3.1625
had_lw_a <- if (exists("had_lw_a")) had_lw_a else 0.000009298
had_lw_b <- if (exists("had_lw_b")) had_lw_b else 3.0205



saved_regs <- read.csv(here::here(paste0("saved_regs/regs_", policy_name, ".csv")))
for (a in seq_len(nrow(saved_regs))) {
  assign(saved_regs$input[a], saved_regs$value[a])
}


directed_trips <- as.data.table(read_fst(file.path(final_process_misc_cd,"directed_trip_draws.fst")))

directed_trips <- directed_trips %>%
  dplyr::select(mode, date_parsed, dtrip,
                cod_bag, cod_min, hadd_bag, hadd_min, draw) %>%
  dplyr::mutate(date_adj = date_parsed) %>%
  dplyr::mutate(
    cod_bag = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(codFH_seas1_op) &
        date_adj <= lubridate::ymd(codFH_seas1_cl) ~ as.numeric(codFH_1_bag),
      TRUE ~ 0),
    cod_bag = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(codPR_seas1_op) &
        date_adj <= lubridate::ymd(codPR_seas1_cl) ~ as.numeric(codPR_1_bag),
      TRUE ~ cod_bag),
    cod_bag = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(codFH_seas2_op) &
        date_adj <= lubridate::ymd(codFH_seas2_cl) ~ as.numeric(codFH_2_bag),
      TRUE ~ cod_bag),
    cod_bag = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(codPR_seas2_op) &
        date_adj <= lubridate::ymd(codPR_seas2_cl) ~ as.numeric(codPR_2_bag),
      TRUE ~ cod_bag),
    cod_bag = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(codFH_seas3_op) &
        date_adj <= lubridate::ymd(codFH_seas3_cl) ~ as.numeric(codFH_3_bag),
      TRUE ~ cod_bag),
    cod_bag = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(codPR_seas3_op) &
        date_adj <= lubridate::ymd(codPR_seas3_cl) ~ as.numeric(codPR_3_bag),
      TRUE ~ cod_bag),

    cod_min = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(codFH_seas1_op) &
        date_adj <= lubridate::ymd(codFH_seas1_cl) ~ as.numeric(codFH_1_len) * 2.54,
      TRUE ~ 100),
    cod_min = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(codPR_seas1_op) &
        date_adj <= lubridate::ymd(codPR_seas1_cl) ~ as.numeric(codPR_1_len) * 2.54,
      TRUE ~ cod_min),
    cod_min = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(codFH_seas2_op) &
        date_adj <= lubridate::ymd(codFH_seas2_cl) ~ as.numeric(codFH_2_len) * 2.54,
      TRUE ~ cod_min),
    cod_min_y2 = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(codPR_seas2_op) &
        date_adj <= lubridate::ymd(codPR_seas2_cl) ~ as.numeric(codPR_2_len) * 2.54,
      TRUE ~ cod_min),
    cod_min_y2 = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(codFH_seas3_op) &
        date_adj <= lubridate::ymd(codFH_seas3_cl) ~ as.numeric(codFH_3_len) * 2.54,
      TRUE ~ cod_min),
    cod_min_y2 = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(codPR_seas3_op) &
        date_adj <= lubridate::ymd(codPR_seas3_cl) ~ as.numeric(codPR_3_len) * 2.54,
      TRUE ~ cod_min),

    hadd_bag = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(hadFH_seas1_op) &
        date_adj <= lubridate::ymd(hadFH_seas1_cl) ~ as.numeric(hadFH_1_bag),
      TRUE ~ 0),
    hadd_bag = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(hadPR_seas1_op) &
        date_adj <= lubridate::ymd(hadPR_seas1_cl) ~ as.numeric(hadPR_1_bag),
      TRUE ~ hadd_bag),
    hadd_bag = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(hadFH_seas2_op) &
        date_adj <= lubridate::ymd(hadFH_seas2_cl) ~ as.numeric(hadFH_2_bag),
      TRUE ~ hadd_bag),
    hadd_bag = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(hadPR_seas2_op) &
        date_adj <= lubridate::ymd(hadPR_seas2_cl) ~ as.numeric(hadPR_2_bag),
      TRUE ~ hadd_bag),
    hadd_bag = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(hadFH_seas3_op) &
        date_adj <= lubridate::ymd(hadFH_seas3_cl) ~ as.numeric(hadFH_3_bag),
      TRUE ~ hadd_bag),
    hadd_bag = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(hadPR_seas3_op) &
        date_adj <= lubridate::ymd(hadPR_seas3_cl) ~ as.numeric(hadPR_3_bag),
      TRUE ~ hadd_bag),

    hadd_min = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(hadFH_seas1_op) &
        date_adj <= lubridate::ymd(hadFH_seas1_cl) ~ as.numeric(hadFH_1_len) * 2.54,
      TRUE ~ 100),
    hadd_min = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(hadPR_seas1_op) &
        date_adj <= lubridate::ymd(hadPR_seas1_cl) ~ as.numeric(hadPR_1_len) * 2.54,
      TRUE ~ hadd_min),
    hadd_min = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(hadFH_seas2_op) &
        date_adj <= lubridate::ymd(hadFH_seas2_cl) ~ as.numeric(hadFH_2_len) * 2.54,
      TRUE ~ hadd_min),
    hadd_min = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(hadPR_seas2_op) &
        date_adj <= lubridate::ymd(hadPR_seas2_cl) ~ as.numeric(hadPR_2_len) * 2.54,
      TRUE ~ hadd_min),
    hadd_min = dplyr::case_when(
      mode == "fh" & date_adj >= lubridate::ymd(hadFH_seas3_op) &
        date_adj <= lubridate::ymd(hadFH_seas3_cl) ~ as.numeric(hadFH_3_len) * 2.54,
      TRUE ~ hadd_min),
    hadd_min = dplyr::case_when(
      mode == "pr" & date_adj >= lubridate::ymd(hadPR_seas3_op) &
        date_adj <= lubridate::ymd(hadPR_seas3_cl) ~ as.numeric(hadPR_3_len) * 2.54,
      TRUE ~ hadd_min)
  )

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

prediction_draws$policy_name <- policy_name

write_csv(prediction_draws, file = here::here("output", paste0("output_", policy_name, "_", time_saver, ".csv")))


