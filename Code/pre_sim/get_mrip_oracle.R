# this R helper file pulls MRIP data from Oracle using the mriptacklebox.


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
first_yr<-2023
last_yr<-2026

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

# write this to an rds file.
write_rds(x, file=file.path(output_folder, glue("mrip_pull.Rds")))

# write this to an dtas file.
write_dta(x$trip,
          path=file.path(output_folder,
                         glue("mrip_trip{first_yr}_{last_yr}.dta"))
)

write_dta(x$catch,
          path=file.path(output_folder,
              glue("mrip_catch{first_yr}_{last_yr}.dta"))
          )

write_dta(x$size,
          path=file.path(output_folder,
                         glue("mrip_size{first_yr}_{last_yr}.dta"))
)
write_dta(x$size_b2,
          path=file.path(output_folder,
                         glue("mrip_size_b2{first_yr}_{last_yr}.dta"))
)

