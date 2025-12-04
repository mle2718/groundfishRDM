
code_cd=here("Code", "test_code")
source(file.path(code_cd,"predict_rec_catch_functions2.R"))
source(file.path(code_cd,"predict_rec_catch_data_functions1.R"))

system.time({

# Local loop
draws <- 1:100
predictions_all <- purrr::map_dfr(
  draws,
  run_one_draw
)

})


summary_predictions<-predictions_all %>%
  as.data.table() %>%
  dplyr::filter(mode=="all modes" ) %>%
  dplyr::group_by(metric, species) %>%
  dplyr::summarise(mean_value=mean(value), sd=sd(value))


