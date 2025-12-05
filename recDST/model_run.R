print("start model")
library(magrittr)
library(fst)
library(plyr)
library(dplyr)
#library(tidyverse)
#devtools::install_github("NEFSC/READ.SSB.groundfishRecDST")



predictions_all = list()

n_draws<-50

mode_draw   <- c("pr", "fh")
season_draw <- c("summer", "winter")

param_grid <- expand.grid(
  md = mode_draw,
  s  = season_draw,
  stringsAsFactors = FALSE
)

ndraws=50 #number of choice occasions to simulate per strata

#l_w_conversion parameters =
cod_lw_a = 0.000005132
cod_lw_b = 3.1625
had_lw_a = 0.000009298
had_lw_b = 3.0205

disc_mort<- fst::read_fst(file.path(here::here("Data/miscellaneous"), "Discard_Mortality.fst")) %>%
  dplyr::rename(month=Month)

adjust_doy <- function(date) {
  doy <- lubridate::yday(date)
  if (doy >= 121) doy - 120 else doy + 245
}

## CORRECT DOY
##### Cod #########
CodFH_seas1_1 <- adjust_doy(lubridate::yday(input$CodFH_seas1[1]))
CodFH_seas1_2 <- adjust_doy(lubridate::yday(input$CodFH_seas1[2]))
CodFH_seas2_1 <- adjust_doy(lubridate::yday(input$CodFH_seas2[1]))
CodFH_seas2_2 <- adjust_doy(lubridate::yday(input$CodFH_seas2[2]))
CodPR_seas1_1 <- adjust_doy(lubridate::yday(input$CodPR_seas1[1]))
CodPR_seas1_2 <- adjust_doy(lubridate::yday(input$CodPR_seas1[2]))
CodPR_seas2_1 <- adjust_doy(lubridate::yday(input$CodPR_seas2[1]))
CodPR_seas2_2 <- adjust_doy(lubridate::yday(input$CodPR_seas2[2]))
###### haddock
HadFH_seas1_1 <- adjust_doy(lubridate::yday(input$HadFH_seas1[1]))
HadFH_seas1_2 <- adjust_doy(lubridate::yday(input$HadFH_seas1[2]))
HadFH_seas2_1 <- adjust_doy(lubridate::yday(input$HadFH_seas2[1]))
HadFH_seas2_2 <- adjust_doy(lubridate::yday(input$HadFH_seas2[2]))
HadFH_seas3_1 <- adjust_doy(lubridate::yday(input$HadFH_seas3[1]))
HadFH_seas3_2 <- adjust_doy(lubridate::yday(input$HadFH_seas3[2]))


HadPR_seas1_1 <- adjust_doy(lubridate::yday(input$HadPR_seas1[1]))
HadPR_seas1_2 <- adjust_doy(lubridate::yday(input$HadPR_seas1[2]))
HadPR_seas2_1 <- adjust_doy(lubridate::yday(input$HadPR_seas2[1]))
HadPR_seas2_2 <- adjust_doy(lubridate::yday(input$HadPR_seas2[2]))
HadPR_seas3_1 <- adjust_doy(lubridate::yday(input$HadPR_seas3[1]))
HadPR_seas3_2 <- adjust_doy(lubridate::yday(input$HadPR_seas3[2]))

#print(directed_trips)
directed_trips<-fst::read_fst(file.path(here::here("Data/miscellaneous"), paste0("directed_trip_draws_final.fst")))

directed_trips <- directed_trips %>%
  dplyr::mutate(cod_min_SQ = cod_min_y2,  cod_bag_SQ = cod_bag_y2,
              hadd_min_SQ = hadd_min_y2, hadd_bag_SQ = hadd_bag_y2,

    cod_bag_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas1_1 & doy <= CodFH_seas1_2 ~ as.numeric(input$CodFH_1_bag), TRUE ~ cod_bag_y2),
    cod_bag_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas1_1 & doy <= CodPR_seas1_2 ~ as.numeric(input$CodPR_1_bag), TRUE ~ cod_bag_y2),
    cod_bag_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas2_1 & doy <= CodFH_seas2_2 ~ as.numeric(input$CodFH_2_bag), TRUE ~ cod_bag_y2),
    cod_bag_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas2_1 & doy <= CodPR_seas2_2 ~ as.numeric(input$CodPR_2_bag), TRUE ~ cod_bag_y2),

    cod_min_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas1_1 & doy <= CodFH_seas1_2 ~ as.numeric((input$CodFH_1_len*2.54)), TRUE ~ cod_min_y2),
    cod_min_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas1_1 & doy <= CodPR_seas1_2 ~ as.numeric((input$CodPR_1_len*2.54)), TRUE ~ cod_min_y2),
    cod_min_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas2_1 & doy <= CodFH_seas2_2 ~ as.numeric((input$CodFH_2_len*2.54)), TRUE ~ cod_min_y2),
    cod_min_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas2_1 & doy <= CodPR_seas2_2 ~ as.numeric((input$CodPR_2_len*2.54)), TRUE ~ cod_min_y2),

    hadd_bag_y2=dplyr::case_when(mode == "fh" & doy >= HadFH_seas1_1 & doy <= HadFH_seas1_2 ~ as.numeric(input$HadFH_1_bag), TRUE ~ hadd_bag_y2),
    hadd_bag_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas1_1 & doy <= HadPR_seas1_2 ~ as.numeric(input$HadPR_1_bag), TRUE ~ hadd_bag_y2),
    hadd_bag_y2=dplyr::case_when(mode == "fh" & doy >= HadFH_seas2_1 & doy <= HadFH_seas2_2 ~ as.numeric(input$HadFH_2_bag), TRUE ~ hadd_bag_y2),
    hadd_bag_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas2_1 & doy <= HadPR_seas2_2 ~ as.numeric(input$HadPR_2_bag), TRUE ~ hadd_bag_y2),
    hadd_bag_y2=dplyr::case_when(mode == "fh" & doy >= HadFH_seas3_1 & doy <= HadFH_seas3_2 ~ as.numeric(input$HadFH_3_bag), TRUE ~ hadd_bag_y2),
    hadd_bag_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas3_1 & doy <= HadPR_seas3_2 ~ as.numeric(input$HadPR_3_bag), TRUE ~ hadd_bag_y2),

    hadd_min_y2=dplyr::case_when(mode == "fh" & doy >= HadFH_seas1_1 & doy <= HadFH_seas1_2 ~ as.numeric((input$HadFH_1_len*2.54)), TRUE ~ hadd_min_y2),
    hadd_min_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas1_1 & doy <= HadPR_seas1_2 ~ as.numeric((input$HadPR_1_len*2.54)), TRUE ~ hadd_min_y2),
    hadd_min_y2=dplyr::case_when(mode == "fh" & doy >= HadFH_seas2_1 & doy <= HadFH_seas2_2 ~ as.numeric((input$HadFH_2_len*2.54)), TRUE ~ hadd_min_y2),
    hadd_min_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas2_1 & doy <= HadPR_seas2_2 ~ as.numeric((input$HadPR_2_len*2.54)), TRUE ~ hadd_min_y2),
    hadd_min_y2=dplyr::case_when(mode == "fh" & doy >= HadFH_seas3_1 & doy <= HadFH_seas3_2 ~ as.numeric((input$HadFH_3_len*2.54)), TRUE ~ hadd_min_y2),
    hadd_min_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas3_1 & doy <= HadPR_seas3_2 ~ as.numeric((input$HadPR_3_len*2.54)), TRUE ~ hadd_min_y2)) %>%

   dplyr::rename(hadd_bag_alt = hadd_bag_y2,
                 hadd_min_alt = hadd_min_y2,
                 cod_bag_alt = cod_bag_y2,
                 cod_min_alt = cod_min_y2)

#mrip_index  <- mrip_index[1:400]
mrip_index  <- mrip_index[1:4]

future::plan(future::multisession, workers = 6)
#future::plan(future::multisession, workers = 124)
get_predictions_out<- function(x){

  directed_trips_draw<-directed_trips %>%
    tibble::tibble() %>%
    dplyr::select(mode, day,  dtrip, draw,
                  starts_with("cod_bag"), starts_with("cod_min"), starts_with("hadd_bag"),starts_with("hadd_min")) %>%
    dplyr::mutate(date=as.Date(day, format = "%d%b%Y"),
                  season = ifelse(lubridate::month(date) %in% c(9, 10, 11, 12, 1, 2, 3, 4), "winter", "summer")) %>%
    dplyr::filter(draw == dr) %>%
    data.table::as.data.table()

  get_lowest_min_size_draw<-directed_trips%>%
    tibble::tibble() %>%
    dplyr::select(mode, day,  dtrip, draw,
                  starts_with("cod_bag"), starts_with("cod_min"), starts_with("hadd_bag"),starts_with("hadd_min"))

  cod_min_size_FY_draw<-min(get_lowest_min_size_draw$cod_min_y2_same)
  hadd_min_size_FY_draw<-min(get_lowest_min_size_draw$hadd_min_y2_same)

  catch_data0 <- list()
  base_outcomes_angler_dems0 <- list()
  n_choice_occasions0 <- list()

  mode_draw <- c("pr", "fh")
  season_draw <- c("summer", "winter")

  k<-1

  for (md in mode_draw) {
    for (s in season_draw){

      catch_data0[[k]] <- fst::read_fst(file.path(here::here("Data/base_outcomes"), paste0("base_outcomes_final_",s, "_", md, "_", dr,".fst"))) %>%
        dplyr::left_join(directed_trips_draw, by=c("mode", "date")) %>%
        dplyr::rename(tot_cod_catch_base = tot_cod_catch,
                      tot_hadd_catch_base = tot_hadd_catch) %>%
        dplyr::mutate(cod_cat=tot_cod_catch_base,
                      hadd_cat=tot_hadd_catch_base) %>%
        dplyr::select(date, mode, draw,  tripid, catch_draw, season,
                      cod_cat, hadd_cat, starts_with("cod_bag"), starts_with("cod_min"),
                      starts_with("hadd_bag"),starts_with("hadd_min")) %>%
        data.table::as.data.table()

      base_outcomes_angler_dems0[[k]] <- fst::read_fst(file.path(here::here("Data/base_outcomes"),  paste0("base_outcomes_final_",s, "_", md, "_", dr,".fst"))) %>%
        dplyr::select(date, mode,  tripid, catch_draw,
                      tot_keep_cod_base, tot_rel_cod_base,
                      tot_keep_hadd_base, tot_rel_hadd_base,
                      starts_with("beta"),
                      total_trips_12, fish_pref_more, educ1, educ2, educ3, own_boat, cost) %>%
        dplyr::rename(date_parsed=date) %>%
        data.table::as.data.table()

      n_choice_occasions0[[k]] <- fst::read_fst(file.path(here::here("Data/n_choice_occasions"), paste0("n_choice_occasions_final_",s, "_", md, "_", dr,".fst"))) %>%
        dplyr::rename(date_parsed=date)  %>%
        data.table::as.data.table()

      k<-k+1

    }
  }

  catch_data_draw <- dplyr::bind_rows(catch_data0)
  base_outcomes_angler_dems_draw <- dplyr::bind_rows(base_outcomes_angler_dems0)
  n_choice_occasions_draw <- dplyr::bind_rows(n_choice_occasions0)

  rm(base_outcomes_angler_dems0, n_choice_occasions0, catch_data0)

  # Size data used in projections is "baseline"proj_catch_at_length.fst"
  # For testing, change the size data file to "baseline_catch_at_length.fst"
  cod_size_data_draw <- fst::read_fst(file.path(here::here("Data/miscellaneous"), "baseline_catch_at_length.fst"))  %>%
    dplyr::filter(species=="cod", draw==dr) %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(fitted_prob, length, season) %>%
    data.table::as.data.table()

  hadd_size_data_draw <- fst::read_fst(file.path(here::here("Data/miscellaneous"), "baseline_catch_at_length.fst"))  %>%
    dplyr::filter(species=="hadd", draw==dr) %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(fitted_prob, length, season) %>%
    data.table::as.data.table()

  calendar_adjustments_draw <- fst::read_fst(file.path(here::here("Data/miscellaneous"), paste0("calendar_adj_final.fst"))) %>%
    dplyr::filter(draw==dr) %>%
    dplyr::select(-dtrip, -dtrip_y2, -draw, -good_draw) %>%
    data.table::as.data.table()

  # Pull in calibration comparison information about trip-level harvest/discard re-allocations
  calib_comparison_draw<-fst::read_fst(file.path(here::here("Data/miscellaneous"), "calibrated_model_stats_final.fst")) %>%
    dplyr::filter(draw==dr) %>%
    data.table::as.data.table()

  calib_comparison_draw<-calib_comparison_draw %>%
    dplyr::rename(n_legal_rel_hadd=n_legal_hadd_rel,
                  n_legal_rel_cod=n_legal_cod_rel,
                  n_sub_kept_hadd=n_sub_hadd_kept,
                  n_sub_kept_cod=n_sub_cod_kept,
                  prop_legal_rel_hadd=prop_legal_hadd_rel,
                  prop_legal_rel_cod=prop_legal_cod_rel,
                  prop_sub_kept_hadd=prop_sub_hadd_kept,
                  prop_sub_kept_cod=prop_sub_cod_kept,
                  convergence_cod=cod_convergence,
                  convergence_hadd=hadd_convergence)

  ##########
  # List of species suffixes
  species_suffixes <- c("cod", "hadd")

  # Get all variable names
  all_vars <- names(calib_comparison_draw)

  # Identify columns that are species-specific (contain _cod or _hadd)
  species_specific_vars <- all_vars[
    stringr::str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
  ]

  id_vars <- setdiff(all_vars, species_specific_vars)

  ## --- build draw-specific inputs ---
  calib_comparison_draw<-calib_comparison_draw %>%
    dplyr::select(mode, season, all_of(species_specific_vars))

  # Extract base variable names (without __cod or _hadd)
  base_names <- unique(stringr::str_replace(species_specific_vars, "_(cod|hadd)$", ""))

  # Pivot the data longer on the species-specific columns
  calib_comparison_draw <- calib_comparison_draw %>%
    tidyr::pivot_longer(
      cols = all_of(species_specific_vars),
      names_to = c(".value", "species"),
      names_pattern = "(.*)_(cod|hadd)"
    ) %>%
    dplyr::distinct()

  source(here::here("Code/sim/predict_rec_catch.R"))
  test<- predict_rec_catch(x = x, draw = k,
                           baseline_comparison1 = baseline_comparison,
                           select_season = select_season, select_mode = select_mode,
                           directed_trips_table = directed_trips2,
                           calibration_data_table = calibration_data,
                           calendar_adjust = calendar_adjust,
                           costs_new_all = costs,
                           size_data_read = size_data_read,
                           discard_mortality_dat = Disc_mort)

  #print("test")
  #print(test)
  #pred <- pred %>% rbind(test)



}
#})
# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions


#write.csv(pred, file = here::here("SQ_predictions_cm.csv"))
predictions_out10<- furrr::future_map_dfr(mrip_index, ~get_predictions_out(.))
