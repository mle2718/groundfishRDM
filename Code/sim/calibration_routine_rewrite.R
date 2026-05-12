
# iterative calibration routine for cod / hadd
# bounded search with best-so-far selection; no catch-hold adjustment.

library(data.table)
library(arrow)
library(haven)
library(readr)
library(fst)


print_calib_progress_cod <- function(cod_row, iter_used, s, md, i) {

  cat("\n--------------------------------------------------\n")
  cat("calibration progress cod \n")
  cat("Season:", s, "| Mode:", md, "| Draw:", i, "| Iteration:", iter_used, "\n")

  cat("\nTotal kept:\n")
  print(data.table::data.table(
    MRIP  = cod_row$MRIP_keep,
    model = cod_row$model_keep,
    diff  = cod_row$diff_keep,
    pct_diff = cod_row$pct_diff_keep
  ))

  cat("--------------------------------------------------\n")
}

print_calib_progress_hadd <- function(hadd_row, iter_used, s, md, i) {

  cat("\n--------------------------------------------------\n")
  cat("calibration progress haddock \n")
  cat("Season:", s, "| Mode:", md, "| Draw:", i, "| Iteration:", iter_used, "\n")

  cat("\nTotal kept:\n")
  print(data.table::data.table(
    MRIP  = hadd_row$MRIP_keep,
    model = hadd_row$model_keep,
    diff  = hadd_row$diff_keep,
    pct_diff = hadd_row$pct_diff_keep
  ))

  cat("--------------------------------------------------\n")
}

cod_disc_mort<- fst::read_fst(file.path(final_process_misc_cd, "Discard_Mortality.fst")) %>%
  dplyr::rename(month=Month) %>%
  dplyr::filter(spp2=="cod")

hadd_disc_mort<- fst::read_fst(file.path(final_process_misc_cd, "Discard_Mortality.fst")) %>%
  dplyr::rename(month=Month) %>%
  dplyr::filter(spp2!="cod")

MRIP_comparison <- as.data.table(
  haven::read_dta(file.path(final_process_misc_cd, "simulated_catch_totals.dta"))
)

setnames(
  MRIP_comparison,
  old = c("tot_dtrip_sim",
          "tot_cod_cat_sim", "tot_hadd_cat_sim",
          "tot_cod_keep_sim", "tot_hadd_keep_sim",
          "tot_cod_rel_sim", "tot_hadd_rel_sim"),
  new = c("estimated_trips",
          "cod_catch", "hadd_catch",
          "cod_keep", "hadd_keep",
          "cod_rel", "hadd_rel"),
  skip_absent = TRUE
)


baseline_output0 <- as.data.table(fst::read_fst(
  file.path(final_process_misc_cd, "calibration_comparison.fst")))

cod_hadd_season <- function(date_parsed) {
  fifelse(data.table::month(date_parsed) %in% c(9, 10, 11, 12, 1, 2, 3, 4),
          "winter", "summer")
}

check_required_cols <- function(dt, cols, object_name) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(object_name, " is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}


# Reconstruct catch columns defensively if the step-0 file omitted them
if (!("MRIP_catch" %in% names(baseline_output0)) && all(c("MRIP_keep", "MRIP_rel") %in% names(baseline_output0))) {
  baseline_output0[, MRIP_catch := MRIP_keep + MRIP_rel]
}
if (!("model_catch" %in% names(baseline_output0)) && all(c("model_keep", "model_rel") %in% names(baseline_output0))) {
  baseline_output0[, model_catch := model_keep + model_rel]
}
if (!("diff_catch" %in% names(baseline_output0)) && all(c("model_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, diff_catch := model_catch - MRIP_catch]
}
if (!("pct_diff_catch" %in% names(baseline_output0)) && all(c("diff_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, pct_diff_catch := fifelse(MRIP_catch != 0, 100 * diff_catch / MRIP_catch, NA_real_)]
}


mode_draw   <- c("pr", "fh")
season_draw <- c("summer", "winter")
draws       <- seq_len(n_simulations)


tol_abs_fish <- 500
tol_abs_pct  <- 5
max_iter     <- 25
p_tol        <- 1e-4

species_vec <- c("cod", "hadd")

is_achieved <- function(diff_keep, pct_diff_keep, MRIP_keep = NA_real_) {
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish)
  }

  (is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish) ||
    (is.finite(pct_diff_keep) && abs(pct_diff_keep) < tol_abs_pct)
}

score_species <- function(diff_keep, pct_diff_keep, diff_catch, pct_diff_catch,
                          MRIP_keep = NA_real_, MRIP_catch = NA_real_) {

  keep_score <- if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf
  } else {
    min(
      if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf,
      if (is.finite(pct_diff_keep)) abs(pct_diff_keep) / tol_abs_pct else Inf
    )
  }

  catch_score <- if (is.finite(MRIP_catch) && MRIP_catch == 0) {
    if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf
  } else {
    min(
      if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf,
      if (is.finite(pct_diff_catch)) abs(pct_diff_catch) / (4 * tol_abs_pct) else Inf
    )
  }

  keep_score + 0.15 * catch_score
}

extract_species_row <- function(dt, sp, md, s, i) {
  out <- as.data.table(dt)[species == sp]
  if (nrow(out) == 0L) {
    out <- data.table(
      mode = md, species = sp,
      MRIP_keep = NA_real_, model_keep = 0, diff_keep = NA_real_, pct_diff_keep = NA_real_,
      MRIP_rel = NA_real_, model_rel = 0, diff_rel = NA_real_, pct_diff_rel = NA_real_,
      MRIP_catch = NA_real_, model_catch = 0, diff_catch = NA_real_, pct_diff_catch = NA_real_,
      rel_to_keep_new = 0, keep_to_rel_new = 0, p_rel_to_keep_new = 0, p_keep_to_rel_new = 0,
      draw = i, season = s
    )
  }
  out[1]
}

make_season <- function(base_row) {

  mrip_keep  <- as.numeric(base_row$MRIP_keep)
  model_keep <- as.numeric(base_row$model_keep)
  diff_keep  <- as.numeric(base_row$diff_keep)

  # zero-target case
  if (is.finite(mrip_keep) && mrip_keep == 0) {

    # if model is also effectively zero, nothing to do
    if (is.finite(model_keep) && model_keep == 0) {
      return(list(
        direction = "none",
        p = 0,
        lo = 0,
        hi = NA_real_,
        achieved = TRUE,
        convergence = 1L,
        best_score = Inf,
        best_row = NULL
      ))
    }

    # if model_keep > 0 and MRIP_keep == 0, only keep->rel makes sense
    direction <- "keep_to_rel"
    p0 <- ifelse(is.finite(base_row$p_keep_to_rel), as.numeric(base_row$p_keep_to_rel), 0)
    p0 <- max(0, min(1, p0))

    return(list(
      direction = direction,
      p = p0,
      lo = 0,
      hi = 1,
      achieved = FALSE,
      convergence = 1L,
      best_score = Inf,
      best_row = NULL,
      frozen = FALSE,
      best_p = p0
    ))
  }

  direction <- if (isTRUE(base_row$rel_to_keep == 1)) {
    "rel_to_keep"
  } else if (isTRUE(base_row$keep_to_rel == 1)) {
    "keep_to_rel"
  } else {
    "none"
  }

  p0 <- if (direction == "rel_to_keep") {
    base_row$p_rel_to_keep
  } else if (direction == "keep_to_rel") {
    base_row$p_keep_to_rel
  } else {
    0
  }

  p0 <- max(0, min(1, as.numeric(p0)))

  list(
    direction = direction,
    p = p0,
    lo = 0,
    hi = if (p0 > 0 && p0 < 1) 1 else NA_real_,
    achieved = FALSE,
    convergence = 1L,
    best_score = Inf,
    best_row = NULL
  )
}

push_globals <- function(seasons_by_sp, target_env = .GlobalEnv) {
  for (sp in species_vec) {
    season <- seasons_by_sp[[sp]]
    assign(paste0("rel_to_keep_", sp),
           as.integer(season$direction == "rel_to_keep"),
           envir = target_env)
    assign(paste0("keep_to_rel_", sp),
           as.integer(season$direction == "keep_to_rel"),
           envir = target_env)
    assign(paste0("p_rel_to_keep_", sp),
           if (season$direction == "rel_to_keep") season$p else 0,
           envir = target_env)
    assign(paste0("p_keep_to_rel_", sp),
           if (season$direction == "keep_to_rel") season$p else 0,
           envir = target_env)
    assign(paste0("all_keep_to_rel_", sp),
           as.integer(season$direction == "keep_to_rel" && season$p >= 1 - p_tol),
           envir = target_env)
  }
}

update_bracket <- function(season, row) {

  if (isTRUE(season$frozen)) {
    return(season)
  }

  if (season$direction == "none") {
    season$achieved <- TRUE
    return(season)
  }

  diff_keep     <- as.numeric(row$diff_keep)
  pct_diff_keep <- as.numeric(row$pct_diff_keep)
  diff_catch    <- as.numeric(row$diff_catch)
  pct_diff_catch<- as.numeric(row$pct_diff_catch)
  MRIP_keep     <- as.numeric(row$MRIP_keep)
  MRIP_catch    <- as.numeric(row$MRIP_catch)

  season$achieved <- is_achieved(diff_keep, pct_diff_keep, MRIP_keep)

  this_score <- score_species(
    diff_keep      = diff_keep,
    pct_diff_keep  = pct_diff_keep,
    diff_catch     = diff_catch,
    pct_diff_catch = pct_diff_catch,
    MRIP_keep      = MRIP_keep,
    MRIP_catch     = MRIP_catch
  )

  if (season$achieved) {
    season$best_row <- copy(row)
    season$best_score <- -Inf
    season$best_p <- season$p
    season$frozen <- TRUE
    return(season)
  }

  if (this_score < season$best_score) {
    season$best_score <- this_score
    season$best_row <- copy(row)
  }

  if (season$direction == "rel_to_keep") {
    # larger p => more keep
    if (is.finite(diff_keep) && diff_keep < 0) {
      season$lo <- max(season$lo, season$p)
    } else if (is.finite(diff_keep) && diff_keep > 0) {
      season$hi <- if (is.na(season$hi)) season$p else min(season$hi, season$p)
    }
  } else if (season$direction == "keep_to_rel") {
    # larger p => fewer keep
    if (is.finite(diff_keep) && diff_keep > 0) {
      season$lo <- max(season$lo, season$p)
    } else if (is.finite(diff_keep) && diff_keep < 0) {
      season$hi <- if (is.na(season$hi)) season$p else min(season$hi, season$p)
    }
  }

  old_p <- season$p

  if (!is.na(season$hi)) {
    season$p <- (season$lo + season$hi) / 2
  } else {
    season$p <- if (old_p == 0) 0.1 else min(1, max(old_p * 1.5, old_p + 0.05))
  }

  season$p <- max(0, min(1, season$p))

  if (abs(season$p - old_p) < p_tol && !season$achieved) {
    season$convergence <- 0L
  }

  if (season$p >= 1 - p_tol && is.na(season$hi) && !season$achieved) {
    season$convergence <- 0L
  }

  season
}


calibrated <- vector("list", length(season_draw) * length(mode_draw) * length(draws))
k <- 1L
for (s in season_draw) {
  for (md in mode_draw) {
    for (i in draws) {

      baseline_targets_current <- baseline_output0[season == s & draw == i & mode == md]
      if (nrow(baseline_targets_current) == 0L) next

      if (all(is.na(baseline_targets_current$MRIP_keep))) {
        out <- copy(baseline_targets_current)
        out[, `:=`(
          keep_to_rel_cod = 0, rel_to_keep_cod = 0, p_rel_to_keep_cod = 0, p_keep_to_rel_cod = 0, convergence_cod = NA_real_,
          keep_to_rel_hadd = 0, rel_to_keep_hadd = 0, p_rel_to_keep_hadd = 0, p_keep_to_rel_hadd = 0, convergence_hadd = NA_real_,
          iter_used = 0L
        )]
        calibrated[[k]] <- out
        k <- k + 1L
        next
      }

      seasons_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
      for (sp in species_vec) {
        base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
        seasons_by_sp[[sp]] <- make_season(base_row)
        seasons_by_sp[[sp]]$best_row <- copy(base_row)
        seasons_by_sp[[sp]]$best_score <- score_species(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$diff_catch,
          base_row$pct_diff_catch,
          base_row$MRIP_keep,
          base_row$MRIP_catch
        )

        seasons_by_sp[[sp]]$achieved <- is_achieved(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$MRIP_keep
        )
      }

      iter_used <- 0L
      last_result <- NULL

      cod_floor_below_min_in   <- 3
      hadd_floor_below_min_in  <- 3

      repeat {


        push_globals(seasons_by_sp, target_env = environment())
        source(file.path(code_cd, "calibrate_rec_catch1_rewrite.R"), local = environment())

        last_result <- copy(as.data.table(calib_comparison1))

        all_done <- TRUE
        for (sp in species_vec) {
          row <- extract_species_row(last_result, sp, md, s, i)
          seasons_by_sp[[sp]] <- update_bracket(seasons_by_sp[[sp]], row)

          if (!seasons_by_sp[[sp]]$achieved && seasons_by_sp[[sp]]$convergence == 1L) {
            all_done <- FALSE
          }
        }

        cod1<-seasons_by_sp[["cod"]]
        hadd1<-seasons_by_sp[["hadd"]]
        cod_row<- cod1[["best_row"]]
        hadd_row<- hadd1[["best_row"]]

        print_calib_progress_cod(cod_row, iter_used = 0, s = s, md = md, i = i)
        print_calib_progress_hadd(hadd_row, iter_used = 0, s = s, md = md, i = i)

        iter_used <- iter_used + 1L

        if (all_done || iter_used >= max_iter) break
      }


      floor_used_in_cod   <- cod_floor_below_min_in
      floor_used_in_hadd  <- hadd_floor_below_min_in


      final_rows <- rbindlist(lapply(species_vec, function(sp) {
        season <- seasons_by_sp[[sp]]
        row <- if (!is.null(season$best_row)) copy(season$best_row) else extract_species_row(last_result, sp, md, s, i)
        row
      }), use.names = TRUE, fill = TRUE)

      final_rows[, `:=`(
        n_sub_kept_cod      = n_sub_kept_cod,
        prop_sub_kept_cod   = prop_sub_kept_cod,
        n_legal_rel_cod     = n_legal_rel_cod,
        prop_legal_rel_cod  = prop_legal_rel_cod,
        original_rel_eligible_cod  = original_rel_eligible_cod,

        n_sub_kept_hadd     = n_sub_kept_hadd,
        prop_sub_kept_hadd  = prop_sub_kept_hadd,
        n_legal_rel_hadd    = n_legal_rel_hadd,
        prop_legal_rel_hadd = prop_legal_rel_hadd,
        original_rel_eligible_hadd  = original_rel_eligible_hadd,

        floor_used_in_cod   = floor_used_in_cod,
        floor_used_in_hadd  = floor_used_in_hadd
      )]

      # set final convergence based on best-so-far row, not just the last attempted row
      final_rows[species == "cod", `:=`(
        keep_to_rel_cod = as.integer(seasons_by_sp[["cod"]]$direction == "keep_to_rel"),
        rel_to_keep_cod = as.integer(seasons_by_sp[["cod"]]$direction == "rel_to_keep"),
        p_rel_to_keep_cod = if (seasons_by_sp[["cod"]]$direction == "rel_to_keep") {
          seasons_by_sp[["cod"]]$best_p } else 0,
        p_keep_to_rel_cod = if (seasons_by_sp[["cod"]]$direction == "keep_to_rel") {
          seasons_by_sp[["cod"]]$best_p} else 0,
        convergence_cod = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
      )]

      final_rows[species == "hadd", `:=`(
        keep_to_rel_hadd = as.integer(seasons_by_sp[["hadd"]]$direction == "keep_to_rel"),
        rel_to_keep_hadd = as.integer(seasons_by_sp[["hadd"]]$direction == "rel_to_keep"),
        p_rel_to_keep_hadd = if (seasons_by_sp[["hadd"]]$direction == "rel_to_keep") {
          seasons_by_sp[["hadd"]]$best_p } else 0,
        p_keep_to_rel_hadd = if (seasons_by_sp[["hadd"]]$direction == "keep_to_rel") {
          seasons_by_sp[["hadd"]]$best_p} else 0,
        convergence_hadd = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
      )]


      # fill non-target species columns with zeros where still missing
      fill_zero_cols <- c(
        "keep_to_rel_cod","rel_to_keep_cod","p_rel_to_keep_cod","p_keep_to_rel_cod","convergence_cod",
        "keep_to_rel_hadd","rel_to_keep_hadd","p_rel_to_keep_hadd","p_keep_to_rel_hadd","convergence_hadd",
        "n_sub_kept_cod","n_legal_rel_cod","prop_sub_kept_cod","prop_legal_rel_cod",
        "n_sub_kept_hadd","n_legal_rel_hadd","prop_sub_kept_hadd","prop_legal_rel_hadd",
        "floor_used_in_cod", "floor_used_in_hadd", "original_rel_eligible_cod", "original_rel_eligible_hadd")

      for (cc in intersect(fill_zero_cols, names(final_rows))) {
        set(final_rows, which(is.na(final_rows[[cc]])), cc, 0)
      }

      final_rows[, iter_used := iter_used]
      setcolorder(final_rows, c("draw","season", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                "floor_used_in_cod", "floor_used_in_hadd",
                                setdiff(names(final_rows), c("draw","season", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                                             "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                                             "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                                             "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                                             "floor_used_in_cod", "floor_used_in_hadd"))))

      calibrated[[k]] <- final_rows
      k <- k + 1L
    }
  }
}

calibrated_combined <- rbindlist(calibrated, use.names = TRUE, fill = TRUE)

drop_cols <- c(
  "rel_to_keep_new", "keep_to_rel_new",
  "p_rel_to_keep_new", "p_keep_to_rel_new"
)

drop_cols <- intersect(drop_cols, names(calibrated_combined))
calibrated_combined[, (drop_cols) := NULL]

# one row per season-mode-draw is enough, since the final calibration values are wide
calibrated_combined <- unique(calibrated_combined, by = c("season", "mode", "draw", "species"))

front_cols <- c("season", "mode", "draw")
front_cols <- intersect(front_cols, names(calibrated_combined))
data.table::setcolorder(calibrated_combined, c(front_cols, setdiff(names(calibrated_combined), front_cols)))


# identify all species suffixes
species_levels <- c("cod", "hadd")

# find all columns that have species suffixes
suffix_pattern <- paste0("(", paste(species_levels, collapse = "|"), ")$")
cols <- names(calibrated_combined)

suffix_cols <- cols[grepl(paste0("_", suffix_pattern), cols)]

# get base variable names (remove suffix)
base_names <- unique(sub(paste0("_", suffix_pattern), "", suffix_cols))

# for each base variable, create a collapsed version
for (v in base_names) {

  new_col <- v

  calibrated_combined[, (new_col) := fifelse(
    species == "cod",  get(paste0(v, "_cod")),
    fifelse(
      species == "hadd", get(paste0(v, "_hadd")), NA_real_) ) ]
}

# drop the wide columns
calibrated_combined[, (suffix_cols) := NULL]

# reorder columns
setcolorder(calibrated_combined, c("season", "mode", "draw", "species", base_names))


# identify non-coverged cells and re-run with expanded floor_sublegal_harvest
library(data.table)

# assume this is your first-pass output in the CURRENT naming format
# one row per season-mode-draw-species
# columns include:
# season, mode, draw, species, floor_used_in,
# keep_to_rel, rel_to_keep, p_rel_to_keep, p_keep_to_rel, convergence,
# MRIP_keep, model_keep, diff_keep, pct_diff_keep, etc.

calibrated_combined <- data.table::as.data.table(calibrated_combined)

# helper for your current long-format output
needs_floor4_rerun <- function(rel_to_keep, convergence, diff_keep, pct_diff_keep, MRIP_keep) {
  # only rerun rel_to_keep cases that still did not converge
  if (!isTRUE(rel_to_keep == 1)) return(FALSE)
  if (!isTRUE(convergence == 0)) return(FALSE)

  # zero-MRIP keep case: use abs diff only
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) >= 500)
  }

  # otherwise use your usual tolerance logic
  keep_bad_abs <- is.finite(diff_keep) && abs(diff_keep) >= 500
  keep_bad_pct <- is.finite(pct_diff_keep) && abs(pct_diff_keep) >= 5

  keep_bad_abs || keep_bad_pct || !is.finite(pct_diff_keep)
}

problem_rows <- calibrated_combined[
  , needs_rerun := mapply(
    needs_floor4_rerun,
    rel_to_keep,
    convergence,
    diff_keep,
    pct_diff_keep,
    MRIP_keep
  )
][needs_rerun == TRUE]

# optional: inspect what will be rerun
print(problem_rows[, .(
  season, mode, draw, species, floor_used_in,
  rel_to_keep, p_rel_to_keep,
  MRIP_keep, model_keep, diff_keep, pct_diff_keep,
  convergence
)])

rerun_results <- vector("list", nrow(problem_rows))

if (nrow(problem_rows) > 0) {
  for (rr in seq_len(nrow(problem_rows))) {

    row_i <- problem_rows[rr]

    # map current long-format names into the scalar objects expected by the rerun script
    s  <- row_i$season
    md <- row_i$mode
    i  <- row_i$draw
    target_species <- row_i$species


    baseline_targets_current <- baseline_output0[season == s & draw == i & mode == md]
    if (nrow(baseline_targets_current) == 0L) next

    if (all(is.na(baseline_targets_current$MRIP_keep))) {
      out <- copy(baseline_targets_current)
      out[, `:=`(
        keep_to_rel_cod = 0, rel_to_keep_cod = 0, p_rel_to_keep_cod = 0, p_keep_to_rel_cod = 0, convergence_cod = NA_real_,
        keep_to_rel_hadd = 0, rel_to_keep_hadd = 0, p_rel_to_keep_hadd = 0, p_keep_to_rel_hadd = 0, convergence_hadd = NA_real_,
        iter_used = 0L
      )]
      rerun_results[[rr]] <- out
      rr <- rr + 1L
      next
    }

    seasons_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
    for (sp in species_vec) {
      base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
      seasons_by_sp[[sp]] <- make_season(base_row)
      seasons_by_sp[[sp]]$best_row <- copy(base_row)
      seasons_by_sp[[sp]]$best_score <- score_species(
        base_row$diff_keep,
        base_row$pct_diff_keep,
        base_row$diff_catch,
        base_row$pct_diff_catch,
        base_row$MRIP_keep,
        base_row$MRIP_catch
      )

      seasons_by_sp[[sp]]$achieved <- is_achieved(
        base_row$diff_keep,
        base_row$pct_diff_keep,
        base_row$MRIP_keep
      )
    }

    iter_used <- 0L
    last_result <- NULL

    floor_below_min_in <- 4
    cod_floor_below_min_in   <- 3
    hadd_floor_below_min_in  <- 3

    if (target_species == "cod")   cod_floor_below_min_in   <- floor_below_min_in
    if (target_species == "hadd")  hadd_floor_below_min_in  <- floor_below_min_in

    repeat {


      push_globals(seasons_by_sp, target_env = environment())
      source(file.path(code_cd, "calibrate_rec_catch1_rewrite.R"), local = environment())

      last_result <- copy(as.data.table(calib_comparison1))

      all_done <- TRUE
      for (sp in species_vec) {
        row <- extract_species_row(last_result, sp, md, s, i)
        seasons_by_sp[[sp]] <- update_bracket(seasons_by_sp[[sp]], row)

        if (!seasons_by_sp[[sp]]$achieved && seasons_by_sp[[sp]]$convergence == 1L) {
          all_done <- FALSE
        }
      }


      cod1<-seasons_by_sp[["cod"]]
      hadd1<-seasons_by_sp[["hadd"]]
      cod_row<- cod1[["best_row"]]
      hadd_row<- hadd1[["best_row"]]

      print_calib_progress_cod(cod_row, iter_used = 0, s = s, md = md, i = i)
      print_calib_progress_hadd(hadd_row, iter_used = 0, s = s, md = md, i = i)


      iter_used <- iter_used + 1L

      if (all_done || iter_used >= max_iter) break
    }


    floor_used_in_cod   <- cod_floor_below_min_in
    floor_used_in_hadd  <- hadd_floor_below_min_in


    final_rows <- rbindlist(lapply(species_vec, function(sp) {
      season <- seasons_by_sp[[sp]]
      row <- if (!is.null(season$best_row)) copy(season$best_row) else extract_species_row(last_result, sp, md, s, i)
      row
    }), use.names = TRUE, fill = TRUE)

    final_rows[, `:=`(
      n_sub_kept_cod      = n_sub_kept_cod,
      prop_sub_kept_cod   = prop_sub_kept_cod,
      n_legal_rel_cod     = n_legal_rel_cod,
      prop_legal_rel_cod  = prop_legal_rel_cod,

      n_sub_kept_hadd     = n_sub_kept_hadd,
      prop_sub_kept_hadd  = prop_sub_kept_hadd,
      n_legal_rel_hadd    = n_legal_rel_hadd,
      prop_legal_rel_hadd = prop_legal_rel_hadd,

      floor_used_in_cod   = floor_used_in_cod,
      floor_used_in_hadd  = floor_used_in_hadd
    )]

    # set final convergence based on best-so-far row, not just the last attempted row
    final_rows[species == "cod", `:=`(
      keep_to_rel_cod = as.integer(seasons_by_sp[["cod"]]$direction == "keep_to_rel"),
      rel_to_keep_cod = as.integer(seasons_by_sp[["cod"]]$direction == "rel_to_keep"),
      p_rel_to_keep_cod = if (seasons_by_sp[["cod"]]$direction == "rel_to_keep") {
        seasons_by_sp[["cod"]]$best_p } else 0,
      p_keep_to_rel_cod = if (seasons_by_sp[["cod"]]$direction == "keep_to_rel") {
        seasons_by_sp[["cod"]]$best_p} else 0,
      convergence_cod = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
    )]

    final_rows[species == "hadd", `:=`(
      keep_to_rel_hadd = as.integer(seasons_by_sp[["hadd"]]$direction == "keep_to_rel"),
      rel_to_keep_hadd = as.integer(seasons_by_sp[["hadd"]]$direction == "rel_to_keep"),
      p_rel_to_keep_hadd = if (seasons_by_sp[["hadd"]]$direction == "rel_to_keep") {
        seasons_by_sp[["hadd"]]$best_p } else 0,
      p_keep_to_rel_hadd = if (seasons_by_sp[["hadd"]]$direction == "keep_to_rel") {
        seasons_by_sp[["hadd"]]$best_p} else 0,
      convergence_hadd = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
    )]


    # fill non-target species columns with zeros where still missing
    fill_zero_cols <- c(
      "keep_to_rel_cod","rel_to_keep_cod","p_rel_to_keep_cod","p_keep_to_rel_cod","convergence_cod",
      "keep_to_rel_hadd","rel_to_keep_hadd","p_rel_to_keep_hadd","p_keep_to_rel_hadd","convergence_hadd",
      "n_sub_kept_cod","n_legal_rel_cod","prop_sub_kept_cod","prop_legal_rel_cod",
      "n_sub_kept_hadd","n_legal_rel_hadd","prop_sub_kept_hadd","prop_legal_rel_hadd",
      "floor_used_in_cod", "floor_used_in_hadd")

    for (cc in intersect(fill_zero_cols, names(final_rows))) {
      set(final_rows, which(is.na(final_rows[[cc]])), cc, 0)
    }

    final_rows[, iter_used := iter_used]
    setcolorder(final_rows, c("draw","season", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                              "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                              "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                              "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                              "floor_used_in_cod", "floor_used_in_hadd",
                              setdiff(names(final_rows), c("draw","season", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                                           "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                                           "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                                           "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                                           "floor_used_in_cod", "floor_used_in_hadd"))))

    rerun_results[[rr]] <- final_rows
    rr <- rr + 1L
  }



calibrated_combined2 <- rbindlist(rerun_results, use.names = TRUE, fill = TRUE)

drop_cols <- c(
  "rel_to_keep_new", "keep_to_rel_new",
  "p_rel_to_keep_new", "p_keep_to_rel_new"
)

drop_cols <- intersect(drop_cols, names(calibrated_combined2))
calibrated_combined2[, (drop_cols) := NULL]

# one row per season-mode-draw is enough, since the final calibration values are wide
calibrated_combined2 <- unique(calibrated_combined2, by = c("season", "mode", "draw", "species"))

front_cols <- c("season", "mode", "draw")
front_cols <- intersect(front_cols, names(calibrated_combined2))
data.table::setcolorder(calibrated_combined2, c(front_cols, setdiff(names(calibrated_combined2), front_cols)))


# identify all species suffixes
species_levels <- c("cod", "hadd")

# find all columns that have species suffixes
suffix_pattern <- paste0("(", paste(species_levels, collapse = "|"), ")$")
cols <- names(calibrated_combined2)

suffix_cols <- cols[grepl(paste0("_", suffix_pattern), cols)]

# get base variable names (remove suffix)
base_names <- unique(sub(paste0("_", suffix_pattern), "", suffix_cols))

# for each base variable, create a collapsed version
for (v in base_names) {

  new_col <- v

  calibrated_combined2[, (new_col) := fifelse(
    species == "cod",  get(paste0(v, "_cod")),
    fifelse(
      species == "hadd", get(paste0(v, "_hadd")), NA_real_)  )]
}

# drop the wide columns
calibrated_combined2[, (suffix_cols) := NULL]

# reorder columns
#setcolorder(calibrated_combined2, c("season", "mode", "draw", "species", base_names))


# replace original problematic rows with the rerun rows
if (nrow(calibrated_combined2) > 0) {
  key_cols <- c("season", "mode", "draw", "species")

  calibrated_combined_final <- calibrated_combined[
    !calibrated_combined2,
    on = key_cols
  ]

  calibrated_combined_final <- data.table::rbindlist(
    list(calibrated_combined_final, calibrated_combined2),
    use.names = TRUE,
    fill = TRUE
  )
}

} else {
  calibrated_combined_final<- calibrated_combined
}

# optional final sort
data.table::setorderv(calibrated_combined_final, c("season", "mode", "draw", "species"))

# check for any problem rows
problem_rows <- calibrated_combined_final[
  , needs_rerun := mapply(
    needs_floor4_rerun,
    rel_to_keep,
    convergence,
    diff_keep,
    pct_diff_keep,
    MRIP_keep
  )
][needs_rerun == TRUE]

fst::write_fst(calibrated_combined_final,  file.path(final_process_misc_cd,
                                                     paste0("calibrated_model_stats.fst")))
