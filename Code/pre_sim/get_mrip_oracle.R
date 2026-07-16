# this R helper file pulls MRIP data from Oracle using the mriptacklebox.
# it takes 2 arguments, first_year and last_year, in sequence.
# because it takes 2 arguments, you'll have to run it from the command line with
# Rscript get_mrip_oracle.R 2023 2025
# or you can run it from stata


# Define arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Error: This script requires exactly two arguments.", call. = FALSE)
}

#read in arguments. Ensure they are numeric
first_yr  <- as.numeric(args[1])
last_yr   <-  as.numeric(args[2])

# Show them, just in case.
cat("First Year:", first_yr, "\n")
cat("Last Year:", last_yr, "\n")


# Load libraries
# install the main branch
#remotes::install_github("NEFSC/READ-PDB-mriptacklebox")

library("here")
library("mriptacklebox")
library("ROracle")
library("tidyverse")
library("DBI")
library("glue")
library("haven")
library("conflicted")
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)


# standard "here", username setup, and paths
here::i_am("Code/pre_sim/get_mrip_oracle.R")
source(here("Code", "helpers", "developer_setup.R"))
output_folder<-file.path(gf.data.dir, "miscellaneous")

#for help with versioning
todaysdate<-Sys.Date()

# Connect to Oracle
drv<-dbDriver("Oracle")
con_name<-eval(nefscdb_con)


yearlist<-first_yr:last_yr
wavelist<-1:6

# pull data and then disconnect
mrip_pull <- mrip_microdata(
  years = yearlist, waves = wavelist,
  typ = c('trip', 'catch', 'size', 'size_b2'),
  format = c('nefsc_db'),
  nefsc_db_con=con_name
)
dbDisconnect(con_name)


# A little data munging

# all lower case
mrip_pull <- map(mrip_pull, ~rename_with(.x, tolower)
                 )

#append mrip_pull_date into all elements. Force it to July 16, 2026 format
mrip_pull <- map(mrip_pull, ~ mutate(
  .x, mrip_pull_date =as.character(format(todaysdate,"%B %d, %Y") ) )
  )

#force certain things to character
mrip_pull <- map(mrip_pull, ~ mutate(
  .x, across(c(strat_id, psu_id, id_code,zip), as.character))
  )


# write all the elements of x to a dta file
walk2(mrip_pull, names(mrip_pull), ~ write_dta(
  .x,
  path=file.path(output_folder, glue("mrip_{.y}.dta"))
  )
)

# append the mrip_pull_date to the mrip_pull list as a tibble

datestamp<-as_tibble(todaysdate)
colnames(datestamp)<-"mrip_pull_date"
mrip_pull$mrip_pull_date<-datestamp

# write this to an rds file.
write_rds(mrip_pull, file=file.path(output_folder, glue("mrip_pull{todaysdate}.Rds")))

message("MRIP data successfully pulled on: ", format(todaysdate,"%B %d, %Y") )

