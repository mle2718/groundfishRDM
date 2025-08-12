print("start model")
library(magrittr)
#library(tidyverse)
#devtools::install_github("NEFSC/READ.SSB.groundfishRecDST")

Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 400)

predictions_all = list()

size_data_read <- readr::read_csv(here::here("data-raw/projected_CaL_cod_hadd_cm.csv"), progress = FALSE)
Disc_mort<- readr::read_csv(here::here("data-raw/Discard_Mortality.csv"), show_col_types = FALSE, progress = FALSE)
directed_trips<-readr::read_csv(here::here("data-raw/directed_trips/directed_trips_doy_cm.csv"), progress = FALSE)

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

# dtest<- directed_trips %>%
#   dplyr::filter(draw == 1 & mode == "fh")
# write.csv(dtest, file = "dtrips_before_select17.csv")

baseline_comparison1<-readRDS(here::here("data-raw/calibration_comparison.rds")) %>%
  dplyr::arrange(draw, mrip_index) %>%
  dplyr::group_by(draw) %>%
  dplyr::mutate(draw_id = dplyr::cur_group_id()) %>%
  dplyr::filter(draw_id<=100)
dplyr::n_distinct(baseline_comparison1$draw)

calendar_adjust1 <- readr::read_csv(here::here("data-raw/next year calendar adjustments.csv"), show_col_types = FALSE, progress = FALSE)

mrip_index <- c(unique(baseline_comparison1$mrip_index))

#mrip_index  <- mrip_index[1:400]
mrip_index  <- mrip_index[1:4]

future::plan(future::multisession, workers = 6)
#future::plan(future::multisession, workers = 124)
get_predictions_out<- function(x){

  baseline_comparison<-baseline_comparison1 %>%
    dplyr::filter(mrip_index==x) %>%
    dplyr::mutate(all_cod_keep_2_release=ifelse(tot_keep_cod_model>0 & tot_cod_keep_mrip==0, 1, 0),
                  all_hadd_keep_2_release=ifelse(tot_keep_hadd_model>0 & tot_hadd_keep_mrip==0, 1, 0))

  k <- unique(baseline_comparison$draw)
  select_mode = unique(baseline_comparison$mode)
  select_season = unique(baseline_comparison$open)

  print(x)

  calibration_data =  readr::read_csv(here::here(paste0("data-raw/calibration/pds_new_", select_mode,"_", select_season, "_",k, ".csv")), progress = FALSE)


  #cost files
  #costs =  arrow::read_feather(here::here(paste0("data-raw/calibration/costs_", select_mode,"_", select_season, "_",k, ".feather")))
  costs =  readr::read_csv(here::here(paste0("data-raw/calibration/costs_", select_mode,"_", select_season, "_",k, ".csv")), progress = FALSE)

  costs<- costs %>%
    dplyr::mutate(tot_cod_catch = as.numeric(costs$tot_keep_cod_base) + as.numeric(costs$tot_rel_cod_base),
                  tot_had_catch = as.numeric(costs$tot_keep_had_base) + as.numeric(costs$tot_rel_had_base))

  # calibration_data_table_base <- split(calibration_output_by_period, calibration_output_by_period$state)
  # cost_files_all_base <- split(costs_new_all, costs_new_all$state)

  calendar_adjust<- calendar_adjust1 %>%
    dplyr::filter(draw == k,
                  mode == select_mode)

  directed_trips2 <- directed_trips %>%
    dplyr::filter(draw == k, mode == select_mode)
  # print(unique(directed_trips2$cod_min_y2))
  # print(unique(directed_trips2$cod_bag_y2))
  # print(unique(directed_trips2$hadd_min_y2))
  # print(unique(directed_trips2$hadd_bag_y2))

  # print(directed_trips2)
  #directed_trips2<- read.csv(here::here("dtrips_test_BASE.csv"))
  #write.csv(directed_trips2, here::here("dtrips_test_AFTER.csv"))

  source(here::here("predict_rec_catch.R"))
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
