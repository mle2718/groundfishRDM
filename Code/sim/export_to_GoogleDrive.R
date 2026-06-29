
#export files to Google Drive
# Upload files to Google Drive
library(googledrive)

# Connect to Google Drive
# NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
drive_auth(cache = here(".secrets"), email = TRUE)

# Output folders on google drive
base_outcomes_path <-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","base_outcomes")
n_choice_occasions_path<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","n_choice_occassions")
calib_catch_draws_path <-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","calib_catch_draws")
miscellaneous_path <-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","miscellaneous")

folder_info <- drive_get(
  path = base_outcomes_path,
  shared_drive = "NMFS NEC READ SSB"
)
base_outcomes_path<-folder_info$id

folder_info <- drive_get(
  path = n_choice_occasions_path,
  shared_drive = "NMFS NEC READ SSB"
)
n_choice_occasions_path<-folder_info$id

folder_info <- drive_get(
  path = calib_catch_draws_path,
  shared_drive = "NMFS NEC READ SSB"
)
calib_catch_draws_path<-folder_info$id

folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)
miscellaneous_path<-folder_info$id


# Upload all files in a local folder to a Google Drive folder
upload_folder_to_drive <- function(local_folder, drive_folder_id,
                                   pattern = "\\.(fst|csv|xlsx|dta)$",
                                   if_exists = c("skip", "overwrite", "rename"),
                                   recursive = FALSE) {

  if_exists <- match.arg(if_exists)

  files_to_upload <- list.files(
    path = local_folder,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )

  if (length(files_to_upload) == 0) {
    warning("No files found in: ", local_folder)
    return(invisible(NULL))
  }

  existing_files <- googledrive::drive_ls(
    path = googledrive::as_id(drive_folder_id)
  )

  purrr::walk(files_to_upload, function(f) {

    file_name <- basename(f)
    message("Uploading: ", file_name)

    existing_match <- existing_files[existing_files$name == file_name, ]

    if (nrow(existing_match) > 0 && if_exists == "skip") {
      message("  Skipping existing file: ", file_name)
      return(NULL)
    }

    upload_name <- file_name

    if (nrow(existing_match) > 0 && if_exists == "rename") {
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      upload_name <- paste0(
        tools::file_path_sans_ext(file_name),
        "_", stamp, ".",
        tools::file_ext(file_name)
      )
    }

    tryCatch(
      {
        googledrive::drive_upload(
          media = f,
          path = googledrive::as_id(drive_folder_id),
          name = upload_name,
          overwrite = if_exists == "overwrite"
        )
      },
      error = function(e) {
        warning("Failed to upload ", file_name, ": ", conditionMessage(e))
      }
    )
  })

  invisible(files_to_upload)
}

upload_folder_to_drive(
  local_folder = final_process_misc_cd,
  drive_folder_id = miscellaneous_path,
  pattern = "\\.(fst|csv|xlsx|dta)$",
  if_exists = "overwrite"
)


upload_folder_to_drive(
  local_folder = final_process_outcomes_cd,
  drive_folder_id = base_outcomes_path,
  pattern = "\\.fst$",
  if_exists = "overwrite"
)

upload_folder_to_drive(
  local_folder = final_process_choice_occasions_cd,
  drive_folder_id = n_choice_occasions_path,
  pattern = "\\.fst$",
  if_exists = "overwrite"
)

upload_folder_to_drive(
  local_folder = final_process_calib_catch_cd,
  drive_folder_id = calib_catch_draws_path,
  pattern = "\\.fst$",
  if_exists = "overwrite"
)




# Identify expected and actual files on Google Drive, print files that are missing to manually upload
library(googledrive)
library(data.table)

# Expected draw IDs
draws <- 1:101
modes <- c("pr", "fh")
seasons <- c("summer", "winter")

# Expected file names
expected_calib <- data.table(
  folder = "calib_catch_draws",
  file_name = paste0("calib_catch_draws_", draws, ".fst")
)

expected_base <- CJ(
  season = seasons,
  mode = modes,
  draw = draws
)[
  ,
  .(
    folder = "base_outcomes",
    file_name = paste0("base_outcomes_", season, "_", mode, "_", draw, ".fst")
  )
]

expected_choice <- CJ(
  season = seasons,
  mode = modes,
  draw = draws
)[
  ,
  .(
    folder = "n_choice_occassions",
    file_name = paste0("n_choice_occasions_", season, "_", mode, "_", draw, ".fst")
  )
]

expected_files <- rbindlist(
  list(expected_calib, expected_base, expected_choice),
  fill = TRUE
)

# Get actual files currently on Google Drive
get_drive_file_names <- function(drive_folder_id, folder_label) {
  x <- googledrive::drive_ls(
    path = googledrive::as_id(drive_folder_id)
  )

  data.table(
    folder = folder_label,
    file_name = x$name,
    drive_id = x$id
  )
}

actual_files <- rbindlist(list(
  get_drive_file_names(calib_catch_draws_path, "calib_catch_draws"),
  get_drive_file_names(base_outcomes_path, "base_outcomes"),
  get_drive_file_names(n_choice_occasions_path, "n_choice_occassions")
))

# Identify missing files
missing_files <- expected_files[
  !actual_files,
  on = c("folder", "file_name")
]

# Print summary
cat("\nExpected files by folder:\n")
print(expected_files[, .N, by = folder])

cat("\nActual files found on Google Drive by folder:\n")
print(actual_files[, .N, by = folder])

cat("\nMissing files by folder:\n")
print(missing_files[, .N, by = folder])

# Print missing file names
if (nrow(missing_files) == 0) {
  cat("\nNo missing files detected.\n")
} else {
  cat("\nMissing files:\n")
  print(missing_files[order(folder, file_name)])
}
