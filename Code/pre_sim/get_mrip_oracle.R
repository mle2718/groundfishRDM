# this R helper file pulls MRIP data from Oracle using the mriptacklebox.


# install the database_testing branch
remotes::install_github("NEFSC/READ-PDB-mriptacklebox@database_testing",
                        , upgrade="never")

library("mriptacklebox")
library("ROracle")
library("tidyverse")
library("conflicted")
library("DBI")

drv<-dbDriver("Oracle")
con_name<-eval(nefscdb_con)

yearlist<-2024:2025
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
