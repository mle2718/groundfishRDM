# Predict Rec Catch
# This function predict recreational catch for cod and haddock


#Convert key data frames to data.table format early:
setDT(directed_trips)
setDT(catch_data)
setDT(calib_comparison)
setDT(cod_size_data)
setDT(hadd_size_data)

#Set up constants (unchanged):
mode_draw <- c("pr", "fh")
season_draw <- c("open", "closed")

#Step 2: Reorganize calibration parameters#
calib_lookup <- calib_comparison %>%
  dplyr::select(mode, species, rel_to_keep, keep_to_rel,
                p_rel_to_keep, p_keep_to_rel,
                prop_sub_kept, prop_legal_rel) %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = c(rel_to_keep, keep_to_rel, p_rel_to_keep, p_keep_to_rel, prop_sub_kept, prop_legal_rel),
    names_glue = "{.value}_{species}"
  )

setDT(calib_lookup)

## Run for all modes and seasons + aggregate
mode_draw   <- c("pr", "fh")
season_draw <- c("open", "closed")

param_grid <- expand.grid(
  md     = mode_draw,
  s = season_draw,
  stringsAsFactors = FALSE
)

results_list <- apply(param_grid, 1, function(row) {
  simulate_cod(md = row[["md"]], s = row[["s"]])
})


trip_data_cod <- rbindlist(lapply(results_list, `[[`, "trip_data"))

data.table::setkey(trip_data_cod, domain2)

zero_catch_cod <- rbindlist(lapply(results_list, `[[`, "zero_catch"))

size_data_cod <- rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE) %>%
  dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))

## Run for all modes + aggregate  - black sea bass
results_list <- apply(param_grid, 1, function(row) {
  simulate_hadd(md = row[["md"]], s = row[["s"]])
})

trip_data_hadd <- rbindlist(lapply(results_list, `[[`, "trip_data")) %>%
  dplyr::select(-date, -mode, -catch_draw, -tripid)

data.table::setkey(trip_data_hadd, domain2)

zero_catch_hadd <- rbindlist(lapply(results_list, `[[`, "zero_catch"))

size_data_hadd <- rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE) %>%
  dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))


# merge the trip data
trip_data <- merge(trip_data_cod, trip_data_hadd, by = "domain2", all = TRUE)
trip_data[is.na(trip_data)] <- 0

# Convert to data.table
data.table::setDT(size_data_cod)
data.table::setDT(size_data_hadd)
data.table::setDT(zero_catch_cod)
data.table::setDT(zero_catch_hadd)

# length data cod and hadd
length_data <- merge(size_data_cod, size_data_hadd,
                     by = c("date", "mode", "tripid", "catch_draw"),
                     all = TRUE)


#First merge cod and hadd zero catches
zero_catch_check<- merge(zero_catch_cod, zero_catch_hadd,
                        by = c("date", "mode", "tripid", "catch_draw"),
                        all = TRUE)[
                            tot_keep_cod_new == 0 & tot_rel_cod_new == 0 &
                            tot_keep_hadd_new == 0 & tot_rel_hadd_new == 0,
                            .(date, mode, tripid, catch_draw)
                          ]


# Bind rows (rbindlist is faster and more memory-efficient)
length_data <- data.table::rbindlist(list(length_data, zero_catch_check), fill = TRUE)


# Replace NA values with 0 again (if necessary)
length_data[is.na(length_data)] <- 0

rm(zero_catch_cod,zero_catch_hadd,zero_catch_check)

length_data<-data.table::as.data.table(length_data)


# Convert to data.table
data.table::setDT(trip_data)
data.table::setDT(length_data)

head(trip_data$date, 20)
unique(substr(trip_data$date, 1, 10))

# Mutate efficiently
trip_data[, date_parsed := lubridate::ymd(date)]
trip_data[, `:=`(
  tot_cat_cod_new  = tot_keep_cod_new + tot_rel_cod_new,
  tot_cat_hadd_new   = tot_keep_hadd_new + tot_rel_hadd_new,
  date = NULL
)]


length_data[, date_parsed := lubridate::ymd(date)][, date := NULL]


trip_data <- trip_data[base_outcomes, on = .(date_parsed, mode, tripid, catch_draw), nomatch = 0L]





