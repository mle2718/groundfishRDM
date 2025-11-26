
code_cd=here("Code", "test_code")
source(file.path(code_cd,"predict_rec_catch_functions2.R"))
source(file.path(code_cd,"predict_rec_catch_data_functions1.R"))

system.time({

# Local loop
draws <- 1:15
predictions_all <- purrr::map_dfr(
  draws,
  run_one_draw
)

})


