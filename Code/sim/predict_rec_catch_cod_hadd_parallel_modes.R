# Efficient projection-year simulation for Gulf of Maine cod and haddock.
#
# This script follows the same structure as predict_rec_catch_final.R for
# summer flounder / black sea bass / scup, but adapts the workflow to the
# cod-haddock model's season x mode x draw calibration structure.
#
# Key conventions carried forward from calibrate_rec_catch1_rewrite.R:
#   1. Calibration parameters are read from calibrated_model_stats.fst.
#   2. Reallocation parameters are species-specific within season-mode-draw.
#   3. Fish reallocated from kept -> released remain released for accounting
#      totals, but still count as kept fish in utility calculations.
#   4. Projection totals are probability-weighted using the cod/haddock DCE
#      utility model.
#
# Expected objects/directories supplied by your wrapper or environment:
#   final_process_misc_cd
#   final_process_proj_catch_cd      # projection catch draws directory
#   final_process_outcomes_cd        # calibration baseline outcomes directory
#   final_process_choice_occasions_cd# calibration n-choice occasions directory
#   final_process_output_cd          # optional output directory
#   n_simulations                    # number of draws to run
#
# Optional files used if present:
#   projected_catch_at_length.csv OR proj_catch_at_length.csv
#   projected_directed_trip_draws.fst OR directed_trip_draws_projection.fst
#   proj_catch_draws_{draw}.fst OR project_catch_draws_{draw}.fst
#   proj_year_calendar_adjustments.fst/csv

library(data.table)
library(fst)
library(readr)
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

# Length-weight parameters from the calibration script.
cod_lw_a <- if (exists("cod_lw_a")) cod_lw_a else 0.000005132
cod_lw_b <- if (exists("cod_lw_b")) cod_lw_b else 3.1625
had_lw_a <- if (exists("had_lw_a")) had_lw_a else 0.000009298
had_lw_b <- if (exists("had_lw_b")) had_lw_b else 3.0205

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

cod_hadd_season <- function(date_parsed) {
  data.table::fifelse(data.table::month(date_parsed) %in% c(9, 10, 11, 12, 1, 2, 3, 4),
                      "winter", "summer")
}

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

calc_prob_trip <- function(v_trip, v_optout) {
  z <- v_trip - v_optout
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  ez <- exp(z[!pos])
  out[!pos] <- ez / (1 + ez)
  out
}

first_existing_file <- function(path_candidates, required = TRUE) {
  path <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(path) && isTRUE(required)) {
    stop("None of these files exist: ", paste(path_candidates, collapse = "; "), call. = FALSE)
  }
  path
}

check_required_cols <- function(dt, cols, object_name) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(object_name, " is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

zero_missing_cols <- function(dt, cols) {
  cols <- intersect(cols, names(dt))
  for (cc in cols) data.table::set(dt, which(is.na(dt[[cc]])), cc, 0)
  invisible(dt)
}

# -----------------------------------------------------------------------------
# Common input loading
# -----------------------------------------------------------------------------

read_projection_common_inputs_cod_hadd <- function(final_process_misc_cd,
                                                   season_draw,
                                                   mode_draw,
                                                   draws) {

  calib <- data.table::as.data.table(fst::read_fst(
    file.path(final_process_misc_cd, "calibrated_model_stats.fst")
  ))

  # Some calibration outputs keep wide species-specific columns then collapsed
  # generic columns. This script uses the generic columns.
  needed_calib <- c("season", "mode", "draw", "species", "floor_used_in",
                    "keep_to_rel", "rel_to_keep", "p_rel_to_keep",
                    "p_keep_to_rel", "all_keep_to_rel")

  for (cc in setdiff(needed_calib, names(calib))) {
    if (cc == "all_keep_to_rel") calib[, (cc) := as.integer(p_keep_to_rel >= 1)]
    else calib[, (cc) := NA_real_]
  }

  calib[is.na(all_keep_to_rel), all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]
  calib <- calib[season %in% season_draw & mode %in% mode_draw & draw %in% draws]
  data.table::setkey(calib, season, mode, draw, species)

  size_lookup <- as.data.table(
    readr::read_csv(file.path(final_process_misc_cd, "baseline_catch_at_length.csv"),
                    show_col_types = FALSE))

  # Expected columns from calibration: species, draw, season, fitted_prob, length.
  check_required_cols(size_lookup, c("species", "draw", "season", "fitted_prob", "length"), "size_lookup")
  size_lookup <- size_lookup[
    species %in% c("cod", "hadd") & draw %in% draws & season %in% season_draw & !is.na(fitted_prob),
    .(species, draw, season, fitted_prob, length)
  ]
  data.table::setkey(size_lookup, species, draw, season)

  directed_trips <- as.data.table(read_fst(file.path(final_process_misc_cd,"directed_trip_draws.fst")))
  directed_trips[, season := cod_hadd_season(date_parsed)]

  check_required_cols(directed_trips, c("draw", "season", "mode", "date_parsed", "dtrip"), "directed_trips")
  directed_trips <- directed_trips[draw %in% draws & season %in% season_draw & mode %in% mode_draw]
  data.table::setkey(directed_trips, draw, season, mode, date_parsed)

  cod_disc_mort<- fst::read_fst(file.path(final_process_misc_cd, "Discard_Mortality.fst")) %>%
    dplyr::rename(month=Month) %>%
    dplyr::filter(spp2=="cod")

  hadd_disc_mort<- fst::read_fst(file.path(final_process_misc_cd, "Discard_Mortality.fst")) %>%
    dplyr::rename(month=Month) %>%
    dplyr::filter(spp2!="cod")

  calendar_adjustments <- read_fst(file.path(final_process_misc_cd,"calendar_adj.fst"))

  list(
    calib = calib,
    size_lookup = size_lookup,
    directed_trips = directed_trips,
    cod_disc_mort = cod_disc_mort,
    hadd_disc_mort = hadd_disc_mort,
    calendar_adjustments = calendar_adjustments
  )
}

# -----------------------------------------------------------------------------
# Species simulator
# -----------------------------------------------------------------------------

simulate_species_project_cod_hadd <- function(catch_dt,
                                              catch_col,
                                              bag_col,
                                              min_col,
                                              size_dt,
                                              species_prefix = c("cod", "hadd"),
                                              rel_to_keep = 0,
                                              keep_to_rel = 0,
                                              p_rel_to_keep = 0,
                                              p_keep_to_rel = 0,
                                              all_keep_to_rel = 0,
                                              cod_disc_mort = data.table(),
                                              hadd_disc_mort = data.table(),
                                              floor_sublegal_abs = NULL,
                                              utility_adjust = TRUE) {

  species_prefix <- as.character(species_prefix)[1]
  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")

  keep_col    <- paste0("tot_keep_", species_prefix, "_new")
  rel_col     <- paste0("tot_rel_", species_prefix, "_new")
  catch_col_o <- paste0("tot_cat_", species_prefix, "_new")
  keep_wt_col <- paste0("tot_keep_", species_prefix, "_weight_lb_new")
  rel_wt_col  <- paste0("tot_rel_", species_prefix, "_weight_lb_new")
  discmort_wt_col <- paste0("tot_discmort_", species_prefix, "_weight_lb_new")
  util_keep_col <- paste0("util_keep_", species_prefix, "_new")
  util_rel_col  <- paste0("util_rel_", species_prefix, "_new")

  empty_out <- unique(catch_dt[, ..key_cols])
  empty_out[, c(keep_col, rel_col, catch_col_o, util_keep_col, util_rel_col,
                keep_wt_col, rel_wt_col, discmort_wt_col) := .(0L, 0L, 0L, 0L, 0L, 0, 0, 0)]

  if (!nrow(catch_dt) || !nrow(size_dt) || sum(size_dt$fitted_prob, na.rm = TRUE) <= 0) {
    return(empty_out)
  }

  pos_dt <- catch_dt[get(catch_col) > 0,
                     .(date_parsed, mode, tripid, catch_draw,
                       catch_n = as.integer(round(get(catch_col))),
                       bag     = get(bag_col),
                       min_sz  = get(min_col))]

  if (!nrow(pos_dt)) {
    data.table::setkeyv(empty_out, key_cols)
    return(empty_out)
  }

  fish_dt <- data.table::copy(pos_dt[rep(seq_len(.N), pmax(catch_n, 0L))])
  fish_dt[, fishid := seq_len(.N)]
  fish_dt[, fitted_length := sample(size_dt$length, .N, replace = TRUE, prob = size_dt$fitted_prob)]
  fish_dt[, month := as.integer(format(date_parsed, "%m"))]

  if (species_prefix=="cod"){
    fish_dt[, fish_weight_lb := cod_lw_a*fitted_length^cod_lw_b]
    fish_dt <-fish_dt %>% left_join(cod_disc_mort,by="month")
    fish_dt <- data.table::copy(data.table::as.data.table(fish_dt))
  }

  if (species_prefix=="hadd"){
    fish_dt[, fish_weight_lb := had_lw_a*fitted_length^had_lw_b]
    fish_dt[, spp2 := data.table::fifelse(
      fitted_length > 50, "had_lg","had_sm")]
    fish_dt <- fish_dt %>% left_join(hadd_disc_mort,by=c("spp2", "month"))
    fish_dt <- data.table::copy(data.table::as.data.table(fish_dt))
  }

  fish_dt[, fish_weight_lb := fish_weight_lb * 2.20462262185]

  fish_dt[, posskeep := data.table::fifelse(fitted_length >= min_sz, 1L, 0L)]
  data.table::setorder(fish_dt, date_parsed, mode, tripid, catch_draw, fishid)
  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  fish_dt[, keep := data.table::fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := data.table::fifelse(keep == 0L, 1L, 0L)]
  fish_dt[, kept_to_released_flag := 0L]


  floor_sublegal <- floor_sublegal_abs

  fish_dt[, subl_harv_indicator := data.table::fifelse(release == 1L & fitted_length >= floor_sublegal, 1L, 0L)]

  rel_to_keep <- ifelse(is.na(rel_to_keep), 0, rel_to_keep)
  keep_to_rel <- ifelse(is.na(keep_to_rel), 0, keep_to_rel)
  p_rel_to_keep <- max(0, min(1, ifelse(is.na(p_rel_to_keep), 0, p_rel_to_keep)))
  p_keep_to_rel <- max(0, min(1, ifelse(is.na(p_keep_to_rel), 0, p_keep_to_rel)))
  all_keep_to_rel <- ifelse(is.na(all_keep_to_rel), as.integer(p_keep_to_rel >= 1), all_keep_to_rel)

  if (rel_to_keep == 1 && fish_dt[, sum(release)] > 0) {
    realloc_dt <- fish_dt[subl_harv_indicator == 1L]
    base_dt    <- fish_dt[subl_harv_indicator == 0L]
    if (nrow(realloc_dt)) {
      realloc_dt[, u := runif(.N)]
      data.table::setorder(realloc_dt, u)
      realloc_dt[, idx := seq_len(.N)]
      n_to_keep <- round(p_rel_to_keep * nrow(realloc_dt))
      n_to_keep <- max(0L, min(n_to_keep, nrow(realloc_dt)))
      realloc_dt[, keep_new := data.table::fifelse(idx <= n_to_keep, 1L, 0L)]
      realloc_dt[, rel_new  := data.table::fifelse(keep_new == 0L, 1L, 0L)]
      realloc_dt[, `:=`(keep = keep_new, release = rel_new)]
      realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]
      fish_dt <- data.table::rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
    }
  }

  if (keep_to_rel == 1 && fish_dt[, sum(keep)] > 0) {
    if (all_keep_to_rel == 1) {
      fish_dt[, kept_to_released_flag := keep]
      fish_dt[, release := keep + release]
      fish_dt[, keep := 0L]
    } else {
      realloc_dt <- fish_dt[keep == 1L]
      base_dt    <- fish_dt[keep == 0L]
      if (nrow(realloc_dt)) {
        realloc_dt[, u := runif(.N)]
        data.table::setorder(realloc_dt, u)
        realloc_dt[, idx := seq_len(.N)]
        n_to_release <- round(p_keep_to_rel * nrow(realloc_dt))
        n_to_release <- max(0L, min(n_to_release, nrow(realloc_dt)))
        realloc_dt[, rel_new  := data.table::fifelse(idx <= n_to_release, 1L, 0L)]
        realloc_dt[, keep_new := data.table::fifelse(rel_new == 0L, 1L, 0L)]
        realloc_dt[, kept_to_released_flag := data.table::fifelse(rel_new == 1L, 1L, 0L)]
        realloc_dt[, `:=`(keep = keep_new, release = rel_new)]
        realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]
        fish_dt <- data.table::rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
      }
    }
  }

  if (isTRUE(utility_adjust)) {
    fish_dt[, keep_util := data.table::fifelse(keep == 1L | kept_to_released_flag == 1L, 1L, 0L)]
    fish_dt[, release_util := data.table::fifelse(release == 1L & kept_to_released_flag == 0L, 1L, 0L)]
  } else {
    fish_dt[, keep_util := keep]
    fish_dt[, release_util := release]
  }


  fish_dt[, keep_weight_lb := keep * fish_weight_lb]
  fish_dt[, release_weight_lb := release * fish_weight_lb]
  fish_dt[, discmort_weight_lb := release_weight_lb * Discard_mortality]

  trip_pos <- fish_dt[, .(
    keep_n = sum(keep),
    rel_n = sum(release),
    catch_n = sum(keep + release),
    keep_util = sum(keep_util),
    rel_util = sum(release_util),
    keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
    rel_weight_lb = sum(release_weight_lb, na.rm = TRUE),
    discmort_weight_lb = sum(discmort_weight_lb, na.rm = TRUE)
  ), by = key_cols]

  data.table::setnames(
    trip_pos,
    old = c("keep_n", "rel_n", "catch_n", "keep_util", "rel_util",
            "keep_weight_lb", "rel_weight_lb", "discmort_weight_lb"),
    new = c(keep_col, rel_col, catch_col_o, util_keep_col, util_rel_col,
            keep_wt_col, rel_wt_col, discmort_wt_col)
  )

  out <- merge(empty_out[, ..key_cols], trip_pos, by = key_cols, all.x = TRUE)
  fill_cols <- setdiff(names(out), key_cols)
  zero_missing_cols(out, fill_cols)
  data.table::setkeyv(out, key_cols)
  out[]
}

# -----------------------------------------------------------------------------
# Combined season-draw projection: both modes are processed from one catch read
# -----------------------------------------------------------------------------

project_one_cod_hadd_both_modes <- function(s,
                                            dr,
                                            common_inputs,
                                            modes = mode_draw,
                                            n_draws = get("n_draws", envir = .GlobalEnv)) {

  directed_trips_sd <- common_inputs$directed_trips[draw == dr & season == s & mode %in% modes]

  catch_path <- file.path(final_process_calib_catch_cd,paste0("calib_catch_draws_", dr, ".fst"))

  catch_data_all <- data.table::as.data.table(fst::read_fst(catch_path))
  data.table::setnames(
    catch_data_all,
    old = c("cod_cat_sim", "hadd_cat_sim", "cost_sim"),
    new = c("cod_cat", "hadd_cat", "cost"),
    skip_absent = TRUE
  )
  if (!"season" %in% names(catch_data_all)) catch_data_all[, season := cod_hadd_season(date_parsed)]
  catch_data_all <- catch_data_all[season == s & mode %in% modes]
  if (!nrow(catch_data_all)) return(NULL)

  catch_data_all <- merge(
    catch_data_all,
    directed_trips_sd[, .(mode, date_parsed, dtrip, cod_bag, cod_min, hadd_bag, hadd_min)],
    by = c("mode", "date_parsed"),
    all.x = TRUE
  )

  check_required_cols(catch_data_all,
                      c("date_parsed", "mode", "tripid", "catch_draw", "cod_cat", "hadd_cat",
                        "cod_bag", "cod_min", "hadd_bag", "hadd_min", "cost"),
                      "catch_data_all")

  size_lookup <- common_inputs$size_lookup
  calib <- common_inputs$calib

  # Split once. This avoids repeatedly scanning the full draw-level catch table.
  catch_by_mode <- split(catch_data_all, by = "mode", keep.by = TRUE)
  mode_outputs <- vector("list", length(modes))
  names(mode_outputs) <- modes

  for (md in modes) {
    catch_data <- catch_by_mode[[md]]
    if (is.null(catch_data) || !nrow(catch_data)) next

    angler_cols <- intersect(
      c("date_parsed", "mode", "tripid", "total_trips_12", "fish_pref_more",
        "educ1", "educ2", "educ3", "own_boat", "cost", "age",
        grep("^beta", names(catch_data), value = TRUE)),
      names(catch_data)
    )
    angler_dems <- unique(catch_data[, ..angler_cols])

    size_lookup <- common_inputs$size_lookup

    cod_size_data  <- size_lookup[list("cod", dr, s), .(fitted_prob, length)]
    hadd_size_data <- size_lookup[list("hadd", dr, s), .(fitted_prob, length)]

    cod_calib <- calib[list(s, md, dr, "cod")]
    hadd_calib <- calib[list(s, md, dr, "hadd")]

    cod_floor_used_in <- ifelse(is.na(cod_calib$floor_used_in[1]), 3, cod_calib$floor_used_in[1])
    hadd_floor_used_in <- ifelse(is.na(hadd_calib$floor_used_in[1]), 3, hadd_calib$floor_used_in[1])

    cod_floor_sublegal_abs <- min(common_inputs$directed_trips$cod_min, na.rm = TRUE) -
      cod_floor_used_in * 2.54

    hadd_floor_sublegal_abs <- min(common_inputs$directed_trips$hadd_min, na.rm = TRUE) -
      hadd_floor_used_in * 2.54

    if (!nrow(cod_calib)) {
      cod_calib <- data.table(rel_to_keep = 0, keep_to_rel = 0, p_rel_to_keep = 0,
                              p_keep_to_rel = 0, all_keep_to_rel = 0, floor_used_in = 3)
    }
    if (!nrow(hadd_calib)) {
      hadd_calib <- data.table(rel_to_keep = 0, keep_to_rel = 0, p_rel_to_keep = 0,
                               p_keep_to_rel = 0, all_keep_to_rel = 0, floor_used_in = 3)
    }

    cod_res <- simulate_species_project_cod_hadd(
      catch_dt = catch_data,
      catch_col = "cod_cat",
      bag_col = "cod_bag",
      min_col = "cod_min",
      size_dt = cod_size_data,
      species_prefix = "cod",
      rel_to_keep = cod_calib$rel_to_keep[1],
      keep_to_rel = cod_calib$keep_to_rel[1],
      p_rel_to_keep = cod_calib$p_rel_to_keep[1],
      p_keep_to_rel = cod_calib$p_keep_to_rel[1],
      all_keep_to_rel = cod_calib$all_keep_to_rel[1],
      cod_disc_mort = common_inputs$cod_disc_mort,
      hadd_disc_mort = common_inputs$hadd_disc_mort,
      floor_sublegal_abs = cod_floor_sublegal_abs,
      utility_adjust = TRUE
    )

    hadd_res <- simulate_species_project_cod_hadd(
      catch_dt = catch_data,
      catch_col = "hadd_cat",
      bag_col = "hadd_bag",
      min_col = "hadd_min",
      size_dt = hadd_size_data,
      species_prefix = "hadd",
      rel_to_keep = hadd_calib$rel_to_keep[1],
      keep_to_rel = hadd_calib$keep_to_rel[1],
      p_rel_to_keep = hadd_calib$p_rel_to_keep[1],
      p_keep_to_rel = hadd_calib$p_keep_to_rel[1],
      all_keep_to_rel = hadd_calib$all_keep_to_rel[1],
      cod_disc_mort = common_inputs$cod_disc_mort,
      hadd_disc_mort = common_inputs$hadd_disc_mort,
      floor_sublegal_abs = hadd_floor_sublegal_abs,
      utility_adjust = TRUE
    )

    key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
    trip_data <- merge(cod_res, hadd_res, by = key_cols, all = TRUE)
    fill_cols <- setdiff(names(trip_data), key_cols)
    zero_missing_cols(trip_data, fill_cols)

    data.table::setkey(angler_dems, date_parsed, mode, tripid)
    trip_data <- merge(trip_data, angler_dems, by = c("date_parsed", "mode", "tripid"), all.x = TRUE)

    base_path <- file.path(final_process_outcomes_cd, paste0("base_outcomes_", s, "_", md, "_", dr, ".fst"))
    base_outcomes <- data.table::as.data.table(fst::read_fst(base_path))

    base_keep <- intersect(c("date_parsed", "mode", "tripid", "catch_draw",
                             "tot_keep_cod_base", "tot_rel_cod_base", "tot_cat_cod_base",
                             "tot_keep_hadd_base", "tot_rel_hadd_base", "tot_cat_hadd_base",
                             "util_keep_cod_base", "util_rel_cod_base",
                             "util_keep_hadd_base", "util_rel_hadd_base"),
                           names(base_outcomes))
    base_outcomes <- base_outcomes[, ..base_keep]

    data.table::setkeyv(base_outcomes, key_cols)
    data.table::setkeyv(trip_data, key_cols)
    trip_data <- base_outcomes[trip_data]
    fill_cols <- grep("^tot_|^util_", names(trip_data), value = TRUE)
    zero_missing_cols(trip_data, fill_cols)

    trip_data[, `:=`(
      v0_trip =
        beta_sqrt_cod_keep     * sqrt(util_keep_cod_base) +
        beta_sqrt_cod_release  * sqrt(util_rel_cod_base) +
        beta_sqrt_hadd_keep    * sqrt(util_keep_hadd_base) +
        beta_sqrt_hadd_release * sqrt(util_rel_hadd_base) +
        beta_sqrt_cod_hadd_keep * (sqrt(util_keep_cod_base) * sqrt(util_keep_hadd_base)) +
        beta_cost * cost,

      vA_trip =
        beta_sqrt_cod_keep     * sqrt(util_keep_cod_new) +
        beta_sqrt_cod_release  * sqrt(util_rel_cod_new) +
        beta_sqrt_hadd_keep    * sqrt(util_keep_hadd_new) +
        beta_sqrt_hadd_release * sqrt(util_rel_hadd_new) +
        beta_sqrt_cod_hadd_keep * (sqrt(util_keep_cod_new) * sqrt(util_keep_hadd_new)) +
        beta_cost * cost,

      v_optout =
        beta_opt_out +
        beta_opt_out_trips12   * total_trips_12 +
        beta_opt_out_fish_pref * fish_pref_more +
        beta_opt_out_educ2     * educ2 +
        beta_opt_out_educ3     * educ3 +
        beta_opt_out_ownboat   * own_boat
    )]

    mean_trip_data <- copy(trip_data)

    beta_drop <- setdiff(
      grep("^beta", names(mean_trip_data), value = TRUE),
      "beta_cost"
    )

    drop_cols <- intersect(
      c(beta_drop,
        "opt_out", "cost", "total_trips_12", "educ1", "educ2", "educ3",
        "fish_pref_more", "own_boat", "age"),
      names(mean_trip_data)
    )

    if (length(drop_cols)) mean_trip_data[, (drop_cols) := NULL]

    keep_vars <- setdiff(names(mean_trip_data), c("date_parsed", "mode", "tripid"))
    mean_trip_data <- mean_trip_data[, lapply(.SD, mean, na.rm = TRUE),
                                     by = .(date_parsed, mode, tripid),
                                     .SDcols = keep_vars]
    mean_trip_data[, `:=`(
      probA = calc_prob_trip(vA_trip, v_optout),
      prob0 = calc_prob_trip(v0_trip, v_optout),
      log_sum_alt  = log(exp(vA_trip) + exp(v_optout)),
      log_sum_base = log(exp(v0_trip) + exp(v_optout))
    )]

    mean_trip_data[, CV := -1 * ((log_sum_alt - log_sum_base) / beta_cost)]

    outcome_cols <- intersect(c(
      "tot_keep_cod_new", "tot_rel_cod_new", "tot_cat_cod_new",
      "tot_keep_hadd_new", "tot_rel_hadd_new", "tot_cat_hadd_new",
      "tot_keep_cod_weight_lb_new", "tot_rel_cod_weight_lb_new", "tot_discmort_cod_weight_lb_new",
      "tot_keep_hadd_weight_lb_new", "tot_rel_hadd_weight_lb_new", "tot_discmort_hadd_weight_lb_new"
    ), names(mean_trip_data))

    mean_trip_data[, (outcome_cols) := lapply(.SD, function(x) x * probA), .SDcols = outcome_cols]

    nchoice_path <- file.path(final_process_choice_occasions_cd, paste0("n_choice_occasions_", s, "_", md, "_", dr, ".fst"))
    n_choice_occasions <- data.table::as.data.table(fst::read_fst(nchoice_path))
    n_choice_occasions <- n_choice_occasions[, .(date_parsed, mode, n_choice_occasions)]

    mean_trip_data <- merge(mean_trip_data, n_choice_occasions, by = c("date_parsed", "mode"), all.x = TRUE)
    mean_trip_data[is.na(n_choice_occasions), n_choice_occasions := 0]
    mean_trip_data[, month := data.table::month(date_parsed)]

    cal_adj <- copy(common_inputs$calendar_adjustments) %>%
      dplyr::filter(draw==dr) %>%
      dplyr::select(mode, month, expansion_factor)
    by_cols <- intersect(c("mode", "month"), names(cal_adj))
    mean_trip_data <- merge(mean_trip_data, cal_adj, by = by_cols, all.x = TRUE)

    if (!"expansion_factor" %in% names(mean_trip_data)) mean_trip_data[, expansion_factor := 1]
    mean_trip_data[is.na(expansion_factor), expansion_factor := 1]
    mean_trip_data[, expand := (n_choice_occasions * 1) / n_draws]

    scale_cols <- intersect(c(outcome_cols, "probA", "prob0", "CV"), names(mean_trip_data))
    mean_trip_data[, (scale_cols) := lapply(.SD, function(x) x * expand), .SDcols = scale_cols]
    data.table::setnames(mean_trip_data, c("probA", "prob0"), c("n_trips_alt", "n_trips_base"), skip_absent = TRUE)

    trip_metrics <- intersect(c(
      "CV", "n_trips_alt", "n_trips_base",
      "tot_keep_cod_new", "tot_rel_cod_new", "tot_cat_cod_new",
      "tot_keep_hadd_new", "tot_rel_hadd_new", "tot_cat_hadd_new",
      "tot_keep_cod_weight_lb_new", "tot_rel_cod_weight_lb_new", "tot_discmort_cod_weight_lb_new",
      "tot_keep_hadd_weight_lb_new", "tot_rel_hadd_weight_lb_new", "tot_discmort_hadd_weight_lb_new"
    ), names(mean_trip_data))

    mean_trip_data[, season := s]

    mode_outputs[[md]] <- mean_trip_data[ ,
      lapply(.SD, sum, na.rm = TRUE),
      by = .(season, mode),
      .SDcols = trip_metrics
    ]

    }

  model_output <- data.table::rbindlist(mode_outputs, use.names = TRUE, fill = TRUE)

  model_output_all <- model_output[, lapply(.SD, sum, na.rm = TRUE), by = .(season), .SDcols = setdiff(names(model_output), c("season", "mode"))]
  model_output_all[, mode := "all modes"]
  data.table::setcolorder(model_output_all, names(model_output))
  model_output <- data.table::rbindlist(list(model_output, model_output_all), use.names = TRUE, fill = TRUE)

  trip_metrics <- setdiff(names(model_output), c("season", "mode"))
  model_output_long <- data.table::melt(
    model_output,
    id.vars = c("season", "mode"),
    measure.vars = trip_metrics,
    variable.name = "metric",
    value.name = "value"
  )
  model_output_long[, iteration := dr]
  model_output_long[]
}

# -----------------------------------------------------------------------------
# Parallel wrapper over draws
# -----------------------------------------------------------------------------
run_cod_hadd_projection <- function(season_draw = get("season_draw", envir = .GlobalEnv),
                                    mode_draw = get("mode_draw", envir = .GlobalEnv),
                                    draws = get("draws", envir = .GlobalEnv),
                                    n_workers = max(1L, parallel::detectCores(logical = FALSE) - 1L),
                                    use_parallel = TRUE,
                                    common_inputs = NULL) {

  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs_cod_hadd(
      final_process_misc_cd = final_process_misc_cd,
      season_draw = season_draw,
      mode_draw = mode_draw,
      draws = draws
    )
  }

  jobs <- data.table::CJ(season = season_draw, draw = draws, sorted = FALSE)

  run_one <- function(ii) {
    s <- jobs$season[ii]
    dr <- jobs$draw[ii]
    message("Projection: season=", s, ", draw=", dr, ", modes=", paste(mode_draw, collapse = ","))
    project_one_cod_hadd_both_modes(
      s = s,
      dr = dr,
      common_inputs = common_inputs,
      modes = mode_draw,
      n_draws = n_draws
    )
  }

  if (isTRUE(use_parallel) && length(draws) > 1L && requireNamespace("future.apply", quietly = TRUE)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    # multisession is safer than multicore for Shiny/Azure and Windows.
    future::plan(future::multisession, workers = n_workers)
    predictions_list <- future.apply::future_lapply(
      seq_len(nrow(jobs)),
      run_one,
      future.seed = TRUE
    )
  } else {
    predictions_list <- lapply(seq_len(nrow(jobs)), run_one)
  }

  data.table::rbindlist(predictions_list, use.names = TRUE, fill = TRUE)
}

# -----------------------------------------------------------------------------
# Main projection execution
# -----------------------------------------------------------------------------

# In an Azure Shiny app, set n_workers from an environment variable or app option,
# e.g. Sys.getenv("RDM_N_WORKERS", unset = parallel::detectCores(logical = FALSE) - 1).
use_parallel <- TRUE
n_workers <- 4   # or however many Azure workers/cores you want available

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

# -----------------------------------------------------------------------------
# Final baseline-vs-projected comparison
# -----------------------------------------------------------------------------

prediction_long <- copy(prediction_draws)
prediction_long[, metric := as.character(metric)]
prediction_long[, species := data.table::fcase(
  grepl("_cod_", metric), "cod",
  grepl("_hadd_", metric), "hadd",
  default = NA_character_
)]

trip_compare <- data.table::dcast(
  prediction_long[metric %in% c("n_trips_alt", "n_trips_base")],
  season + mode + iteration ~ metric,
  value.var = "value"
)

trip_compare <- trip_compare[, .(
  season, mode, iteration,
  species = NA_character_,
  metric = "trips",
  baseline_value = n_trips_base,
  projected_value = n_trips_alt
)]

prediction_long2 <- prediction_long[!metric %in% c("n_trips_alt", "n_trips_base")]
prediction_long2[, metric_clean := data.table::fcase(
  metric == "CV", "compensating variation ($)",
  grepl("tot_keep_.*weight_lb", metric), "harvest (lbs.)",
  grepl("tot_rel_.*weight_lb", metric), "discards (lbs.)",
  grepl("tot_discmort_.*weight_lb", metric), "dead discards (lbs.)",
  grepl("tot_keep_", metric), "harvest (#s)",
  grepl("tot_rel_", metric), "discards (#s)",
  grepl("tot_cat_", metric), "catch (#s)",
  default = metric
)]
prediction_long2[, metric := metric_clean]
prediction_long2[, metric_clean := NULL]
data.table::setnames(prediction_long2, "value", "projected_value")

calib_full <- data.table::as.data.table(fst::read_fst(file.path(final_process_misc_cd, "calibrated_model_stats.fst")))
calib_keep_cols <- intersect(
  c("season", "mode", "draw", "species", "model_keep", "model_rel", "model_catch",
    "model_keep_lbs", "model_rel_lbs", "model_discmort_lbs"),
  names(calib_full)
)
calib_keep <- calib_full[season %in% season_draw & mode %in% mode_draw & draw %in% draws, ..calib_keep_cols]
data.table::setnames(calib_keep, "draw", "iteration", skip_absent = TRUE)

calib_all_modes <- calib_keep[, .(
  model_keep = sum(model_keep, na.rm = TRUE),
  model_rel = sum(model_rel, na.rm = TRUE),
  model_catch = sum(model_catch, na.rm = TRUE),
  model_keep_lbs = sum(model_keep_lbs, na.rm = TRUE),
  model_rel_lbs = sum(model_rel_lbs, na.rm = TRUE),
  model_discmort_lbs = sum(model_discmort_lbs, na.rm = TRUE)
), by = .(season, iteration, species)]
calib_all_modes[, mode := "all modes"]

calib_keep <- data.table::rbindlist(list(calib_keep, calib_all_modes), use.names = TRUE, fill = TRUE)

calib_long <- data.table::melt(
  calib_keep,
  id.vars = c("season", "mode", "iteration", "species"),
  measure.vars = intersect(c("model_keep", "model_rel", "model_keep_lbs",
                             "model_rel_lbs", "model_discmort_lbs", "model_catch"), names(calib_keep)),
  variable.name = "metric",
  value.name = "baseline_value"
)

calib_long[, metric := data.table::fcase(
  metric == "model_keep", "harvest (#s)",
  metric == "model_rel", "discards (#s)",
  metric == "model_catch", "catch (#s)",
  metric == "model_keep_lbs", "harvest (lbs.)",
  metric == "model_rel_lbs", "discards (lbs.)",
  metric == "model_discmort_lbs", "dead discards (lbs.)",
  default = as.character(metric)
)]

final_compare <- merge(
  prediction_long2,
  calib_long,
  by = c("season", "mode", "iteration", "species", "metric"),
  all.x = TRUE
)

final_compare <- data.table::rbindlist(list(final_compare, trip_compare), use.names = TRUE, fill = TRUE)

final_compare[, difference := projected_value - baseline_value]
final_compare[, pct_difference := safe_divide(projected_value - baseline_value, baseline_value) * 100]
final_compare[, difference := round(difference, 1)]
final_compare[, pct_difference := round(pct_difference, 1)]
final_compare[, projected_value := round(projected_value, 0)]
final_compare[, baseline_value := round(baseline_value, 0)]

data.table::setcolorder(
  final_compare,
  c("iteration", "season", "mode", "species", "metric",
    "baseline_value", "projected_value", "difference", "pct_difference")
)
data.table::setorder(final_compare, iteration, season, mode, species, metric)



# ---- Summarize by draw, then average across draws ----

# 1. Sum output within each draw across seasons/modes where appropriate
final_compare_draw_sums <- final_compare[ , .(
    baseline_value  = sum(baseline_value, na.rm = TRUE),
    projected_value = sum(projected_value, na.rm = TRUE)
  ),  by = .(iteration, mode, species, metric)
]

final_compare_draw_sums[, difference := projected_value - baseline_value]
final_compare_draw_sums[, pct_difference :=
                          safe_divide(difference, baseline_value) * 100
]

# 2. Average summed draw-level outputs across draws
final_compare_draw_avg <- final_compare_draw_sums[,  .(
    baseline_value  = mean(baseline_value, na.rm = TRUE),
    projected_value = mean(projected_value, na.rm = TRUE),
    difference      = mean(difference, na.rm = TRUE),
    pct_difference  = mean(pct_difference, na.rm = TRUE)
  ),
  by = .(mode, species, metric)
]

final_compare_draw_avg[, iteration := "draw average"]

# 3. Optional rounding and ordering
final_compare_draw_avg[, `:=`(
  baseline_value  = round(baseline_value, 0),
  projected_value = round(projected_value, 0),
  difference      = round(difference, 1),
  pct_difference  = round(pct_difference, 1)
)]

data.table::setcolorder(
  final_compare_draw_avg,
  c("iteration", "mode", "species", "metric",
    "baseline_value", "projected_value", "difference", "pct_difference")
)

