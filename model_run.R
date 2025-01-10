print("start model")
library(magrittr)
#library(tidyverse)
#devtools::install_github("NEFSC/READ.SSB.groundfishRecDST")

Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 400)

predictions_all = list()

size_data_read <- readr::read_csv(here::here("data-raw/projected_CaL_cod_hadd_cm.csv"))
Disc_mort<- readr::read_csv(here::here("data-raw/Discard_Mortality.csv"), show_col_types = FALSE)
directed_trips<-readr::read_csv(here::here("data-raw/directed_trips/directed_trips_doy_cm.csv"))


## CORRECT DOY
##### Cod #########
CodFH_seas1_1 <- if(lubridate::yday(input$CodFH_seas1[1]) >= 121){
  lubridate::yday(input$CodFH_seas1[1]) - 120
}else {
  lubridate::yday(input$CodFH_seas1[1]) + 245
}
CodFH_seas1_2 <- if(lubridate::yday(input$CodFH_seas1[2]) >= 121){
  lubridate::yday(input$CodFH_seas1[2]) - 120
} else {
  lubridate::yday(input$CodFH_seas1[2]) + 245
}
CodFH_seas2_1 <- if(lubridate::yday(input$CodFH_seas2[1]) >= 121){
  lubridate::yday(input$CodFH_seas2[1]) - 120
} else {
  lubridate::yday(input$CodFH_seas2[1]) + 245
}
CodFH_seas2_2 <- if(lubridate::yday(input$CodFH_seas2[2]) >= 121){
  lubridate::yday(input$CodFH_seas2[2]) - 120
} else {
  lubridate::yday(input$CodFH_seas2[2]) +245
}

CodPR_seas1_1 <- if(lubridate::yday(input$CodPR_seas1[1]) >= 121){
  lubridate::yday(input$CodPR_seas1[1]) - 120
} else {
  lubridate::yday(input$CodPR_seas1[1]) + 245
}
CodPR_seas1_2 <- if(lubridate::yday(input$CodPR_seas1[2]) >= 121){
  lubridate::yday(input$CodPR_seas1[2]) - 120
} else {
  lubridate::yday(input$CodPR_seas1[2]) + 245
}
CodPR_seas2_1 <- if(lubridate::yday(input$CodPR_seas2[1]) >= 121){
  lubridate::yday(input$CodPR_seas2[1]) - 120
} else {
  lubridate::yday(input$CodPR_seas2[1]) + 245
}
CodPR_seas2_2 <- if(lubridate::yday(input$CodPR_seas2[2]) >= 121){
  lubridate::yday(input$CodPR_seas2[2]) - 120
} else {
  lubridate::yday(input$CodPR_seas2[2]) + 245
}
###### haddock
HadFH_seas1_1 <- if(lubridate::yday(input$HadFH_seas1[1]) >= 121){
  lubridate::yday(input$HadFH_seas1[1]) - 120
} else {
  lubridate::yday(input$HadFH_seas1[1]) + 245
}
HadFH_seas1_2 <- if(lubridate::yday(input$HadFH_seas1[2]) >= 121){
  lubridate::yday(input$HadFH_seas1[2]) - 120
} else {
  lubridate::yday(input$HadFH_seas1[2]) + 245
}
HadFH_seas2_1 <- if(lubridate::yday(input$HadFH_seas2[1]) >= 121){
  lubridate::yday(input$HadFH_seas2[1]) - 120
} else {
  lubridate::yday(input$HadFH_seas2[1]) + 245
}
HadFH_seas2_2 <- if(lubridate::yday(input$HadFH_seas2[2]) >= 121){
  lubridate::yday(input$HadFH_seas2[2]) - 120
} else {
  lubridate::yday(input$HadFH_seas2[2]) + 245
}
HadFH_seas3_1 <- if(lubridate::yday(input$HadFH_seas3[1]) >= 121){
  lubridate::yday(input$HadFH_seas3[1]) - 120
} else {
  lubridate::yday(input$HadFH_seas3[1]) + 245
}
HadFH_seas3_2 <- if(lubridate::yday(input$HadFH_seas3[2]) >= 121){
  lubridate::yday(input$HadFH_seas3[2]) - 120
} else {
  lubridate::yday(input$HadFH_seas3[2]) + 245
}


HadPR_seas1_1 <- if(lubridate::yday(input$HadPR_seas1[1]) >= 121){
  lubridate::yday(input$HadPR_seas1[1]) - 120
} else {
  lubridate::yday(input$HadPR_seas1[1]) + 245
}
HadPR_seas1_2 <- if(lubridate::yday(input$HadPR_seas1[2]) >= 121){
  lubridate::yday(input$HadPR_seas1[2]) - 120
} else {
  lubridate::yday(input$HadPR_seas1[2]) + 245
}
HadPR_seas2_1 <- if(lubridate::yday(input$HadPR_seas2[1]) >= 121){
  lubridate::yday(input$HadPR_seas2[1]) - 120
} else {
  lubridate::yday(input$HadPR_seas2[1]) + 245
}
HadPR_seas2_2 <- if(lubridate::yday(input$HadPR_seas2[2]) >= 121){
  lubridate::yday(input$HadPR_seas2[2]) - 120
} else {
  lubridate::yday(input$HadPR_seas2[2]) + 245
}
HadPR_seas3_1 <- if(lubridate::yday(input$HadPR_seas3[1]) >= 121){
  lubridate::yday(input$HadPR_seas3[1]) - 120
} else {
  lubridate::yday(input$HadPR_seas3[1]) + 245
}
HadPR_seas3_2 <- if(lubridate::yday(input$HadPR_seas3[2]) >= 121){
  lubridate::yday(input$HadPR_seas3[2]) - 120
} else {
  lubridate::yday(input$HadPR_seas3[2]) + 245
}

print(CodFH_seas1_1 )
print(CodFH_seas1_2 )

print(CodPR_seas1_1 )
print(CodPR_seas1_2 )

print(CodFH_seas2_1 )
print(CodFH_seas2_2 )

print(CodPR_seas2_1 )
print(CodPR_seas2_2 )


print(directed_trips)

directed_trips <- directed_trips %>%
  dplyr::mutate(cod_min_SQ = cod_min_y2,  cod_bag_SQ = cod_bag_y2,
              hadd_min_SQ = hadd_min_y2, hadd_bag_SQ = hadd_bag_y2)

directed_trips<- directed_trips %>%
  dplyr::mutate(
    cod_bag_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas1_1 & doy <= CodFH_seas1_2 ~ as.numeric(input$CodFH_1_bag), TRUE ~ cod_bag_y2),
    cod_bag_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas1_1 & doy <= CodPR_seas1_2 ~ as.numeric(input$CodPR_1_bag), TRUE ~ cod_bag_y2),
    cod_bag_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas2_1 & doy <= CodFH_seas2_2 ~ as.numeric(input$CodFH_2_bag), TRUE ~ cod_bag_y2),
    cod_bag_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas2_1 & doy <= CodPR_seas2_2 ~ as.numeric(input$CodPR_2_bag), TRUE ~ cod_bag_y2),

    cod_min_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas1_1 & doy <= CodFH_seas1_2 ~ as.numeric((input$CodFH_1_len*2.54)), TRUE ~ cod_min_y2),
    cod_min_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas1_1 & doy <= CodPR_seas1_2 ~ as.numeric((input$CodPR_1_len*2.54)), TRUE ~ cod_min_y2),
    cod_min_y2=dplyr::case_when(mode == "fh" & doy >= CodFH_seas2_1 & doy <= CodFH_seas2_2 ~ as.numeric((input$CodFH_2_len*2.54)), TRUE ~ cod_min_y2),
    cod_min_y2=dplyr::case_when(mode == "pr" & doy >= CodPR_seas2_1 & doy <= CodPR_seas2_2 ~ as.numeric((input$CodPR_2_len*2.54)), TRUE ~ cod_min_y2))



directed_trips<- directed_trips %>%
  dplyr::mutate(

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
    hadd_min_y2=dplyr::case_when(mode == "pr" & doy >= HadPR_seas3_1 & doy <= HadPR_seas3_2 ~ as.numeric((input$HadPR_3_len*2.54)), TRUE ~ hadd_min_y2))

directed_trips <- directed_trips %>%
   # dplyr::mutate(cod_min_alt = cod_min_y2, cod_bag_alt = cod_bag_y2,
   #               hadd_min_alt = hadd_min_y2, hadd_bag_alt = hadd_bag_y2) %>%
  # ##open august
  # # dplyr::mutate(cod_bag_alt = dplyr::case_when(month1 == 8 ~ 1, TRUE ~ cod_bag_alt),
  # #                cod_min_alt = dplyr::case_when(month1 == 8 ~ 23*2.54, TRUE ~ cod_min_alt)) %>%
  # #
  # ##open whole year
  # dplyr::mutate(cod_bag_alt = 1,
  #               cod_min_alt = 23*2.54) %>%
  # #####
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

calendar_adjust1 <- readr::read_csv(here::here("data-raw/next year calendar adjustments.csv"), show_col_types = FALSE)

mrip_index <- c(unique(baseline_comparison1$mrip_index))

mrip_index  <- mrip_index[1:400]
#mrip_index  <- mrip_index[1:4]

#future::plan(future::multisession, workers = 6)
future::plan(future::multisession, workers = 124)
get_predictions_out<- function(x){

  baseline_comparison<-baseline_comparison1 %>%
    dplyr::filter(mrip_index==x) %>%
    dplyr::mutate(all_cod_keep_2_release=ifelse(tot_keep_cod_model>0 & tot_cod_keep_mrip==0, 1, 0),
                  all_hadd_keep_2_release=ifelse(tot_keep_hadd_model>0 & tot_hadd_keep_mrip==0, 1, 0))

  k <- unique(baseline_comparison$draw)
  select_mode = unique(baseline_comparison$mode)
  select_season = unique(baseline_comparison$open)

  print(x)

  calibration_data =  readr::read_csv(here::here(paste0("data-raw/calibration/pds_new_", select_mode,"_", select_season, "_",k, ".csv")))


  #cost files
  #costs =  arrow::read_feather(here::here(paste0("data-raw/calibration/costs_", select_mode,"_", select_season, "_",k, ".feather")))
  costs =  readr::read_csv(here::here(paste0("data-raw/calibration/costs_", select_mode,"_", select_season, "_",k, ".csv")))

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

#write.csv(predictions_out10, file = "predictions3.csv")
# pred11_80 = pred
#
# pred = rbind(pred1_4, pred5_10, pred11_80)
# write.csv(pred, file = here::here(paste0("output/pred_test_KB_works.csv")))
# predictions_out <- pred %>%
#   dplyr::group_by(Category, mode, catch_disposition, param,  number_weight, draw_out) %>%
#   dplyr::reframe(Value = sum(Value)) %>%
#   dplyr::ungroup()


#
# StatusQuo <- openxlsx::read.xlsx(here::here("data-raw/StatusQuo/baseline_CT3.xlsx"))
#
# StatusQuo_corrections<- openxlsx::read.xlsx(here::here("data-raw/StatusQuo/All_states_SQ_corrections1.xlsx")) %>%
#   dplyr::filter(state == state1)
#
# StatusQuo<-StatusQuo %>%
#   dplyr::left_join(StatusQuo_corrections, by=c("state", "mode", "Category", "keep_release", "number_weight")) %>%
#   dplyr::mutate(correction=dplyr::case_when(is.na(correction)~1, TRUE~correction)) %>%
#   dplyr::mutate(Value=as.numeric(Value), correction=as.numeric(correction),
#                 Value=Value*correction) %>%
#   dplyr::rename(value_SQ = Value)
#
# predictions_merge <- predictions_out10 %>% #predictions_out10 %>%
#   dplyr::rename(value_alt= Value) %>%
#   dplyr::mutate(draw = as.numeric(draw)) %>%
#   dplyr::left_join(StatusQuo, by = c("Category","mode", "keep_release","param" ,"number_weight","state", "draw"))
#
# all_dat <- predictions_merge %>%
#   dplyr::select(!c("correction", "param")) %>%
#   dplyr::mutate(Category = dplyr::recode(Category,  "bsb" = "Black Sea Bass",
#                                          "sf" = "Summer Flounder",
#                                          "scup" = "Scup"),
#                 keep_release = dplyr::recode(keep_release,  "keep" = "harvest",
#                                              "Discmortality" = "dead release"),
#                 number_weight = dplyr::recode(number_weight,  "Weight" = "pounds",
#                                               "Number" = "numbers"),
#                 mode = dplyr::recode(mode,  "fh" = "For Hire",
#                                      "pr" = "Private",
#                                      "sh" = "Shore"),
#                 stat = paste(keep_release, number_weight),
#                 median_perc_diff = "NA",
#                 reach_target = "NA") %>%
#   dplyr::rename(region = state,
#                 species = Category,
#                 median_value_alt = value_alt,
#                 median_value_SQ = value_SQ) %>%
#   dplyr::select(!c("keep_release", "number_weight"))
#
#
# predictions_merge <- predictions_merge %>%
#   #dplyr::left_join(StatusQuo, by = c("Category","mode", "keep_release","number_weight","state")) %>%
#   dplyr::filter(Category %in% c("sf", "bsb", "scup"),
#                 mode!="all",
#                 keep_release=="keep",
#                 #number_weight %in% c("Weight_avg", "Weight") ) %>%
#                 number_weight %in% c("Weight", "Weight_avg") ) %>%
#   dplyr::select(-param) %>%
#   dplyr::mutate(value_SQ = as.numeric(value_SQ),
#                 value_alt = as.numeric(value_alt))
#
# predictions_weight <- predictions_merge %>%
#   dplyr::filter(number_weight == "Weight") %>%
#   dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
#   dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))
#
# predictions_avg <- predictions_merge %>%
#   dplyr::filter(number_weight == "Weight_avg")
#
# predictions_merge2<- predictions_avg %>%
#   dplyr::left_join(predictions_weight, by = c("Category","mode", "state","draw")) %>%
#   dplyr::select(-keep_release.x, -keep_release.y, -number_weight.y) %>%
#   dplyr::mutate(imputed_value_alt= perc_change_weight/100,
#                 imputed_value_alt = value_SQ * imputed_value_alt,
#                 imputed_value_alt=imputed_value_alt+value_SQ)
# #check = ((imputed_value_alt-value_SQ)/value_SQ)*100)
#
# predictions_merge2<- predictions_merge2 %>%
#   #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
#   dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) &
#                                                        !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>%
#   #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
#   dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) &
#                                                        is.na(value_alt_Weight) &
#                                                        value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))
# state_harvest_output<- predictions_merge2 %>%
#   dplyr::group_by(draw, Category, state) %>%
#   dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>%
#   dplyr::arrange(state, Category, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
#                                                imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
# state_harvest_output <- state_harvest_output %>%
#   dplyr::mutate(domain=paste0(state, "_", Category)) %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,imputed_value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup()
#
# state_harvest_output<- state_harvest_output %>%
#   dplyr::mutate(harv_target=dplyr::case_when(Category=="scup"~scup_percent_change*value_SQ_sum, TRUE~NA),
#                 #harv_target=dplyr::case_when(Category=="bsb"~bsb_percent_change*value_SQ_sum, TRUE~harv_target),
#                 harv_target=dplyr::case_when(Category=="sf"~sf_percent_change*value_SQ_sum, TRUE~harv_target)) %>%
#   dplyr::mutate(reach_target=dplyr::case_when(imputed_value_alt_sum<=harv_target  ~1, TRUE~0))
#
# categories_state=list()
#
# for(d in unique(state_harvest_output$domain)){
#
#   new<- state_harvest_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$imputed_value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#   reach_target<- sum(new$reach_target)
#
#
#   categories_state[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
#     ))
#
#   categories_state[[d]]$domain<-d
#
#
# }
# state_harvest_results= rlist::list.stack(categories_state, fill=TRUE)
# state_harvest_results<- state_harvest_results %>%
#   tidyr::separate(domain, into = c("region", "species"))  %>%
#   dplyr::mutate(stat="harvest pounds",
#                 mode="all modes") %>%
#   dplyr::relocate(region, stat, species, mode)
#
#
# ###########################
# #state by mode-level output
# state_mode_harvest_output<- predictions_merge2 %>%
#   dplyr::group_by(draw, Category, state, mode) %>%
#   dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>%
#   dplyr::arrange(state, mode, Category, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
#                                                imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
# state_mode_harvest_output_check<- state_mode_harvest_output %>%
#   dplyr::filter(perc_diff=="NaN")
#
# state_mode_harvest_output <- state_mode_harvest_output %>%
#   dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,imputed_value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(domain,perc_diff)
#
# state_mode_harvest_output<- state_mode_harvest_output %>%
#   dplyr::mutate(harv_target=dplyr::case_when(Category=="scup"~scup_percent_change*value_SQ_sum, TRUE~NA),
#                 #harv_target=dplyr::case_when(Category=="bsb"~bsb_percent_change*value_SQ_sum, TRUE~harv_target),
#                 harv_target=dplyr::case_when(Category=="sf"~sf_percent_change*value_SQ_sum, TRUE~harv_target)) %>%
#   dplyr::mutate(reach_target=dplyr::case_when(imputed_value_alt_sum<=harv_target~1, TRUE~0))
#
# categories_state_mode=list()
#
# for(d in unique(state_mode_harvest_output$domain)){
#
#   new<- state_mode_harvest_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$imputed_value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#   reach_target<- sum(new$reach_target)
#
#
#
#   categories_state_mode[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
#     ))
#
#   categories_state_mode[[d]]$domain<-d
#
#
# }
# state_mode_harvest_results= rlist::list.stack(categories_state_mode, fill=TRUE)
# state_mode_harvest_results<- state_mode_harvest_results %>%
#   tidyr::separate(domain, into = c("region", "species", "mode"))  %>%
#   dplyr::mutate(stat="harvest pounds")
#
#
#
# ### Release
#
# predictions_releases_merge <- predictions_out10 %>%
#   dplyr::rename(value_alt= Value) %>%
#   dplyr::mutate(draw = as.numeric(draw)) %>%
#   dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
#   dplyr::mutate(value_SQ = as.numeric(value_SQ),
#                 value_alt = as.numeric(value_alt)) %>%
#   dplyr::filter(keep_release %in% c("release", "Discmortality") )
#
# predictions_release_weight <- predictions_releases_merge %>%
#   dplyr::filter(number_weight == "Weight") %>%
#   dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
#   dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))
#
# predictions_release_avg <- predictions_releases_merge %>%
#   dplyr::filter(number_weight == "Weight_avg")
#
#
#
# predictions_releases_merge2<- predictions_release_avg %>%
#   dplyr::left_join(predictions_release_weight, by = c("Category","mode", "state","draw", "keep_release")) %>%
#   dplyr::select(-number_weight.y) %>%
#   dplyr::mutate(imputed_value_alt= perc_change_weight/100,
#                 imputed_value_alt = value_SQ * imputed_value_alt,
#                 imputed_value_alt=imputed_value_alt+value_SQ)
# #check = ((imputed_value_alt-value_SQ)/value_SQ)*100)
#
# predictions_releases_merge2<- predictions_releases_merge2 %>%
#   #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
#   dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) &
#                                                        !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>%
#   #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
#   dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) &
#                                                        is.na(value_alt_Weight) &
#                                                        value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))
#
# state_release_output<- predictions_releases_merge2 %>%
#   dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>%
#   dplyr::group_by(draw, domain) %>%
#   dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>%
#   dplyr::arrange(domain, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
#                                                imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
# state_release_output <- state_release_output %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,imputed_value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup()
#
# categories_release_state=list()
#
# for(d in unique(state_release_output$domain)){
#
#   new<- state_release_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$imputed_value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#
#   categories_release_state[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ
#     ))
#
#   categories_release_state[[d]]$domain<-d
#
#
# }
# state_release_results= rlist::list.stack(categories_release_state, fill=TRUE)
#
# state_release_results<- state_release_results %>%
#   tidyr::separate(domain, into = c("species", "region", "stat1")) %>%
#   dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release pounds", TRUE~stat1),
#                 stat=dplyr::case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat),
#                 mode="all modes") %>%
#   dplyr::select(-stat1)
#
# ###########################
# #state by mode-level release output
# state_mode_release_output<- predictions_releases_merge2 %>%
#   dplyr::group_by(draw, Category, state, mode, keep_release) %>%
#   dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>%
#   dplyr::arrange(state, mode, Category, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
#                                                imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
#
# state_mode_release_output <- state_mode_release_output %>%
#   dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,imputed_value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(domain,perc_diff)
#
# categories_releases_state_mode=list()
#
# for(d in unique(state_mode_release_output$domain)){
#
#   new<- state_mode_release_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$imputed_value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#
#   categories_releases_state_mode[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ
#     ))
#
#   categories_releases_state_mode[[d]]$domain<-d
#
#
# }
# state_mode_release_results= rlist::list.stack(categories_releases_state_mode, fill=TRUE)
# state_mode_release_results<- state_mode_release_results %>%
#   tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>%
#   dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release pounds", TRUE~stat1),
#                 stat=dplyr::case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat)) %>%
#   dplyr::select(-stat1)
#
# release_ouput<- state_release_results %>% rbind(state_mode_release_results)
#
#
# ### CV
# CV_state_mode <- predictions_out10 %>%
#   dplyr::mutate(draw = as.numeric(draw)) %>%
#   dplyr::rename(value_alt= Value) %>%
#   dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw")) %>%
#   dplyr::mutate(value_SQ = as.numeric(value_SQ),
#                 value_alt = as.numeric(value_alt)) %>%
#   dplyr::filter(Category %in% c("CV", "ntrips"),
#                 mode!="all" ) %>%
#   dplyr::select(!c(param.x, param.y, keep_release.x, keep_release.y)) %>%
#   dplyr::mutate(value_SQ = as.numeric(value_SQ),
#                 value_alt = as.numeric(value_alt),
#                 perc_diff=((value_alt-value_SQ)/value_SQ)*100)
#
# state_CV<- CV_state_mode %>%
#   dplyr::group_by(draw, Category, state) %>%
#   dplyr::summarise(value_alt_sum = sum(value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=(value_SQ_sum-value_alt_sum))
#
#
# #sort observations and create index by species
# state_CV<- state_CV %>%
#   dplyr::group_by(Category, state) %>%
#   dplyr::arrange(Category,state, value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(Category)) %>%
#   dplyr::arrange(Category,state, perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(Category)) %>%
#   dplyr::arrange(Category,state,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(Category)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(Category,state,perc_diff) %>%
#   dplyr::mutate(domain=paste0(state, "_", Category))
#
#
#
# state_Cvs=list()
#
# for(d in unique(state_CV$domain)){
#
#   new<- state_CV %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#
#   state_Cvs[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ
#     ))
#
#   state_Cvs[[d]]$domain<-d
#
#
# }
# state_CV_results= rlist::list.stack(state_Cvs, fill=TRUE)
# state_CV_results<- state_CV_results %>%
#   tidyr::separate(domain, into = c("region", "stat")) %>%
#   dplyr::mutate(mode="all modes", species="all species")
#
#
#
# ###########################
# #state mode-level CV output
# state_mode_CV_output<- CV_state_mode %>%
#   dplyr::group_by(draw, Category, state, mode) %>%
#   dplyr::summarise(value_alt_sum = sum(value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=value_SQ_sum-value_alt_sum) %>%
#   dplyr::arrange(state, mode, Category, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
#                                                value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
#
# state_mode_CV_output <- state_mode_CV_output %>%
#   dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(domain,perc_diff)
#
# categories_CV_state_mode=list()
#
# for(d in unique(state_mode_CV_output$domain)){
#
#   new<- state_mode_CV_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#
#   categories_CV_state_mode[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ
#     ))
#
#   categories_CV_state_mode[[d]]$domain<-d
#
#
# }
# state_mode_CV_results= rlist::list.stack(categories_CV_state_mode, fill=TRUE)
# state_mode_CV_results<- state_mode_CV_results %>%
#   tidyr::separate(domain, into = c("region", "stat",  "mode"))  %>%
#   dplyr::mutate(species="all species")
#
#
#
# ##### Numbers
# alt<- predictions_out10 %>%
#   dplyr::mutate(draw = as.numeric(draw)) %>%
#   dplyr::rename(value_alt = Value) %>%
#   dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
#   dplyr::filter(mode!="all" ) %>%
#   dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
#   dplyr::filter(number_weight %in% c("Number") ) %>%
#   dplyr::select(-param)
#
#
# StatusQuo <- rbind(StatusQuo) %>%
#   dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
#   dplyr::filter(mode!="all" ) %>%
#   dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
#   dplyr::filter(number_weight %in% c("Number") )
#
# predictions_harv_num_merge <- alt %>%
#   dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
#   dplyr::mutate(value_SQ = as.numeric(value_SQ),
#                 value_alt = as.numeric(value_alt))
#
# # predictions_harv_num_merge_MA <- predictions_harv_num_merge %>%
# #   dplyr::filter(state=="MA") %>%
# #   dplyr::mutate(value_SQ=value_SQ-diff, value_alt=value_alt-diff) %>%
# #   dplyr::mutate(perc_diff=((value_alt_adj-value_SQ_adj)/value_SQ_adj)*100) %>%
# #   dplyr::mutate(perc_diff=dplyr::case_when(is.nan(perc_diff) & value_SQ_adj==0 & value_alt_adj==0~0, TRUE~perc_diff)) %>%
# #   dplyr::mutate(imputed_value_alt=dplyr::case_when(is.nan(imputed_value_alt) & value_SQ_adj==0 & value_alt_adj==0~0, TRUE~imputed_value_alt)) %>%
# #   dplyr::select(Category, mode, keep_release, number_weight, value_SQ, imputed_value_alt, state, draw) %>%
# #   dplyr::rename(value_alt=imputed_value_alt)
# #
# #
# #
# # predictions_harv_num_merge<-predictions_harv_num_merge %>%
# #   dplyr::filter(state!="MA")
# #
# # predictions_harv_num_merge<-rbind(predictions_harv_num_merge, predictions_harv_num_merge_NJ)
#
#
#
#
# ###########################
# #state-level output
# state_harv_num_output<- predictions_harv_num_merge %>%
#   dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>%
#   dplyr::group_by(draw, domain) %>%
#   dplyr::summarise(value_alt_sum = sum(value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>%
#   dplyr::arrange(domain, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
#                                                value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
# state_harv_num_output <- state_harv_num_output %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup()
#
# categories_harv_num_state=list()
#
# for(d in unique(state_harv_num_output$domain)){
#
#   new<- state_harv_num_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#
#   categories_harv_num_state[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ
#     ))
#
#   categories_harv_num_state[[d]]$domain<-d
#
#
# }
# state_harv_num_results= rlist::list.stack(categories_harv_num_state, fill=TRUE)
#
# state_harv_num_results<- state_harv_num_results %>%
#   tidyr::separate(domain, into = c("species", "region", "stat1")) %>%
#   dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
#                 stat=dplyr::case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
#                 stat=dplyr::case_when(stat1=="keep"~ "harvest numbers", TRUE~stat),
#                 mode="all modes") %>%
#   dplyr::select(-stat1)
#
#
# rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
#    ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)
#
#
# ###########################
# #state by mode-level harvest num output
# state_mode_harv_num_output<- predictions_harv_num_merge %>%
#   dplyr::group_by(draw, Category, state, mode, keep_release) %>%
#   dplyr::summarise(value_alt_sum = sum(value_alt),
#                    value_SQ_sum = sum(value_SQ)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>%
#   dplyr::arrange(state, mode, Category, draw) %>%
#   dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 & value_alt_sum==0 ~ 0, TRUE ~ perc_diff))
#
#
# state_mode_harv_num_output <- state_mode_harv_num_output %>%
#   dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>%
#   dplyr::group_by(domain) %>%
#   dplyr::arrange(domain,value_alt_sum) %>%
#   dplyr::mutate(n_weight = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,perc_diff) %>%
#   dplyr::mutate(n_perc = dplyr::row_number(domain)) %>%
#   dplyr::arrange(domain,value_SQ_sum) %>%
#   dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(domain,value_SQ_sum)
#
# # state_mode_harv_num_output_check<-state_mode_harv_num_output %>%
# #   dplyr::filter(state=="NY" & Category=="scup" & keep_release=="keep" & mode=="fh")
#
# categories_harv_num_state_mode=list()
#
# for(d in unique(state_mode_harv_num_output$domain)){
#
#   new<- state_mode_harv_num_output %>%
#     dplyr::filter(domain==d) #%>%
#   #dplyr::arrange(n_perc)
#
#   lb_value_alt<- new$value_alt_sum[new$n_weight==11]
#   lb_perc_diff<- new$perc_diff[new$n_perc==11]
#   lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11]
#
#   ub_value_alt<- new$value_alt_sum[new$n_weight==90]
#   ub_perc_diff<- new$perc_diff[new$n_perc==90]
#   ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90]
#
#   median_value_alt<- median(new$value_alt_sum)
#   median_perc_diff<- median(new$perc_diff)
#   median_value_SQ<- median(new$value_SQ_sum)
#
#
#   categories_harv_num_state_mode[[d]] <- as.data.frame(
#     cbind(
#       median_perc_diff,lb_perc_diff, ub_perc_diff,
#       median_value_alt,lb_value_alt, ub_value_alt,
#       median_value_SQ,lb_value_SQ, ub_value_SQ
#     ))
#
#   categories_harv_num_state_mode[[d]]$domain<-d
#
#
# }
# state_mode_harv_num_results= rlist::list.stack(categories_harv_num_state_mode, fill=TRUE)
# state_mode_harv_num_results<- state_mode_harv_num_results %>%
#   tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>%
#   dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
#                 stat=dplyr::case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
#                 stat=dplyr::case_when(stat1=="keep"~ "harvest numbers", TRUE~stat)) %>%
#   dplyr::select(-stat1)
#
#
# rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
#    ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)
#
#
# predictions<- plyr::rbind.fill(state_mode_harv_num_results, state_harv_num_results,
#                                state_harvest_results, state_mode_harvest_results,release_ouput) %>%
#   dplyr::mutate(reach_target = as.character(reach_target),
#                 reach_target = dplyr::case_when(median_value_SQ == 0 ~ "Not Applicable", TRUE ~ reach_target),
#                 reach_target = dplyr::case_when(species == "bsb" ~ "No harvest target", TRUE ~ reach_target),
#                 reach_target = dplyr::case_when(stat == "harvest numbers" ~ "No harvest target", TRUE ~ reach_target),
#                 reach_target = dplyr::case_when(stat == "release numbers" ~ "No harvest target", TRUE ~ reach_target),
#                 reach_target = dplyr::case_when(stat == "dead release numbers" ~ "No harvest target", TRUE ~ reach_target),
#                 median_perc_diff = round(median_perc_diff, 2),
#                 median_value_alt = round(median_value_SQ + ((median_perc_diff/100)* median_value_SQ), 0),
#                 median_value_SQ = round(median_value_SQ, 0)) %>%
#   plyr::rbind.fill(state_CV_results, state_mode_CV_results) %>%
#   dplyr::mutate(species = dplyr::recode(species, "bsb"= "Black Sea Bass", "sf" = "Summer Flounder", "scup" = "Scup"),
#                 mode = dplyr::recode(mode, "fh" = "For Hire", "pr" = "Private", "sh" = "Shore"),
#                 draw = "Summary") %>%
#   dplyr::select(region, stat, mode, draw, species, median_value_SQ, median_value_alt, median_perc_diff, reach_target) %>%
#   rbind(all_dat) %>%
#   dplyr::rename("State" = region,
#                 "Statistic" = stat,
#                 "Mode" = mode,
#                 "Species" = species,
#                 "Status-quo value (median)" = median_value_SQ,
#                 "Alternative option value" = median_value_alt,
#                 "% difference from status-quo outcome (median)" = median_perc_diff,
#                 "% under harvest target (out of 100 simulations)" = reach_target)
#
#
# ## join with Traceys SQ

