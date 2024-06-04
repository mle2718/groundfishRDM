print("start model")

predictions_all = list()

cod_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/cod_prob_star_2024_CT.csv")),  show_col_types = FALSE)

had_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/had_prob_star_2024_CT.csv")),  show_col_types = FALSE)

l_w_conversion <-readr::read_csv(file.path(here::here("data-raw/size_data/L_W_Conversion.csv")),  show_col_types = FALSE) %>%
  dplyr::mutate(ln_a = as.numeric(ln_a))


directed_trips<-readRDS(file.path(here::here(paste0("data-raw/directed_trips/directed_trips.rds"))))


directed_trips<- directed_trips %>%
  dplyr::mutate(
    cod_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$CodFH_seas1[1]) & day_i <= lubridate::yday(input$CodFH_seas1[2]) ~ as.numeric(input$CodFH_1_bag), TRUE ~ 0),
    cod_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$CodPR_seas1[1]) & day_i <= lubridate::yday(input$CodPR_seas1[2]) ~ as.numeric(input$CodPR_1_bag), TRUE ~ cod_bag),


    cod_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$CodFH_seas2[1]) & day_i <= lubridate::yday(input$CodFH_seas2[2]) ~ as.numeric(input$CodFH_2_bag), TRUE ~ cod_bag),
    cod_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$CodPR_seas2[1]) & day_i <= lubridate::yday(input$CodPR_seas2[2]) ~ as.numeric(input$CodPR_2_bag), TRUE ~ cod_bag),

    cod_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$CodFH_seas1[1]) & day_i <= lubridate::yday(input$CodFH_seas1[2]) ~ as.numeric(input$CodFH_1_len[1]), TRUE ~ 100),
    cod_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$CodPR_seas1[1]) & day_i <= lubridate::yday(input$CodPR_seas1[2]) ~ as.numeric(input$CodPR_1_len[1]), TRUE ~ cod_min),

    cod_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$CodFH_seas2[1]) & day_i <= lubridate::yday(input$CodFH_seas2[2]) ~ as.numeric(input$CodFH_2_len[1]), TRUE ~ cod_min),
    cod_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$CodPR_seas2[1]) & day_i <= lubridate::yday(input$CodPR_seas2[2]) ~ as.numeric(input$CodPR_2_len[1]), TRUE ~ cod_min))


directed_trips<- directed_trips %>%
  dplyr::mutate(
    had_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$HadFH_seas1[1]) & day_i <= lubridate::yday(input$HadFH_seas1[2]) ~ as.numeric(input$HadFH_1_bag), TRUE ~ 0),
    had_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$HadPR_seas1[1]) & day_i <= lubridate::yday(input$HadPR_seas1[2]) ~ as.numeric(input$HadPR_1_bag), TRUE ~ had_bag),
    had_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$HadFH_seas2[1]) & day_i <= lubridate::yday(input$HadFH_seas2[2]) ~ as.numeric(input$HadFH_2_bag), TRUE ~ had_bag),
    had_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$HadPR_seas2[1]) & day_i <= lubridate::yday(input$HadPR_seas2[2]) ~ as.numeric(input$HadPR_2_bag), TRUE ~ had_bag),
    had_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$HadFH_seas3[1]) & day_i <= lubridate::yday(input$HadFH_seas3[2]) ~ as.numeric(input$HadFH_3_bag), TRUE ~ had_bag),
    had_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$HadPR_seas3[1]) & day_i <= lubridate::yday(input$HadPR_seas3[2]) ~ as.numeric(input$HadPR_3_bag), TRUE ~ had_bag),

    had_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$HadFH_seas1[1]) & day_i <= lubridate::yday(input$HadFH_seas1[2]) ~ as.numeric(input$HadFH_1_len), TRUE ~ 100),
    had_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$HadPR_seas1[1]) & day_i <= lubridate::yday(input$HadPR_seas1[2]) ~ as.numeric(input$HadPR_1_len), TRUE ~ had_min),
    had_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$HadFH_seas2[1]) & day_i <= lubridate::yday(input$HadFH_seas2[2]) ~ as.numeric(input$HadFH_2_len), TRUE ~ had_min),
    had_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$HadPR_seas2[1]) & day_i <= lubridate::yday(input$HadPR_seas2[2]) ~ as.numeric(input$HadPR_2_len), TRUE ~ had_min),
    had_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$HadFH_seas3[1]) & day_i <= lubridate::yday(input$HadFH_seas3[2]) ~ as.numeric(input$HadFH_3_len), TRUE ~ had_min),
    had_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$HadPR_seas3[1]) & day_i <= lubridate::yday(input$HadPR_seas3[2]) ~ as.numeric(input$HadPR_3_len), TRUE ~ had_min))



