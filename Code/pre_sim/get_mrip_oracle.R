# this R helper file pulls MRIP data from Oracle using the mriptacklebox.
# it takes 2 arguments, first_year and last_year, in sequence.


args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Error: This script requires exactly two arguments.", call. = FALSE)
}

first_yr  <- as.numeric(args[1])
last_yr   <-  as.numeric(args[2])# Convert to numeric if needed


cat("First Year:", first_yr, "\n")
cat("Last Year:", last_yr, "\n")



# install the mt2 (dev) branch
#remotes::install_github("NEFSC/READ-PDB-mriptacklebox@mt2",
#                        , upgrade="never")
library("here")
library("mriptacklebox")
library("ROracle")
library("tidyverse")
library("DBI")
library("glue")
library("haven")
library("conflicted")

here::i_am("Code/pre_sim/get_mrip_oracle.R")
source(here("Code", "helpers", "developer_setup.R"))

output_folder<-file.path(gf.data.dir, "miscellaneous")

drv<-dbDriver("Oracle")
con_name<-eval(nefscdb_con)


yearlist<-first_yr:last_yr
wavelist<-1:6

x <- mrip_microdata(
  years = yearlist, waves = wavelist,
  typ = c('trip', 'catch', 'size', 'size_b2'),
  format = c('nefsc_db'),
  nefsc_db_con=con_name
)


# all lower case
x <- map(x, ~ rename_with(.x, tolower))

# append the date ran to the object

x$DateRan<-Sys.Date()
# write this to an rds file.
write_rds(x, file=file.path(output_folder, glue("mrip_pull.Rds")))

# write this to an dtas file.
write_dta(x$trip,
          path=file.path(output_folder,
                         glue("mrip_trip.dta"))
)

write_dta(x$catch,
          path=file.path(output_folder,
              glue("mrip_catch.dta"))
          )

write_dta(x$size,
          path=file.path(output_folder,
                         glue("mrip_size.dta"))
)
write_dta(x$size_b2,
          path=file.path(output_folder,
                         glue("mrip_size_b2.dta"))
)

