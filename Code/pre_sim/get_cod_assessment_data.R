###################################################
### Code to read in WGOM Cod Stock assessment data provided by Charles Peretti.
# The terminal year of this stock assessment is 2023.
# The bioeconomic model needs a few parameters that go into the stock assessment.
# It also needs some parameters that come out of the stock assessment.
# This code does just 1 projection.
# 2) 75%Fmsy 2025-2027 (potential ABCs)
# Bridging is done by picking the last row of "agg_catch" from the projection
# for 2023 and 2024
# This is pretty standard.
# Some inputs to ASAP are scalars, some are vectors, and some are matrices.
# I use tail(.x, 1) to pick the last "thing" of a vector or matrix, which is usually the final year of data.

# Aggregate Weights (SSB and catch) are in metric tons.
# Individual weights (Weights at age vectors) are in kg


############
# Parameters that go into the stock assessment
# Fraction of year that elapses before SSB Calculation (Jan1=0.0)
# Natural Mortality -- note that in the original blast model, this is a scalar.
  # In ASAP, this is specified as a vector of M at age, which is stacked into a T by A matrix (T is number of years and A is number of Age classes)

############
# Parameters that come out of the stock assessment
# Weights at age
############
# The WGOM cod stock assessment has 2 fleets, Commercial and recreational.  There is also a 'blended' fleet.

#BLAST 1.0 uses these 1xA row vectors.
# cod_jan1_weights
# cod_midyear_weights
# cod_catch_weights
# cod_ssb_weights
# cod_discard_weights
# cod_discard_fraction not in the model for commercial
# cod_maturity

# Historical NAA comes out of the WHAM stock assessment.

######
# Parameters that come out of the projections
######
# Numbers at age (in the future).  There isn't a stochastic projection for WGOM
# cod.  WGOM Cod NAA are assumed to be lognormally distributed with a mean and sd parameters.
# I use rlnorm() to generate a distribution

############ End description###################################################

library(tidyverse)
library(TMB)
library(haven)
library(glue)

###########Begin Housekeeping##################################################
#Set paths, input names, and savefile names.

BLAST_root<-file.path("//nefscfile","BLAST","READ-SSB-Lee-BLAST")
input_folder<-file.path(BLAST_root,"cod_haddock_fy2025","source_data","cod","input")
output_folder<-file.path(BLAST_root,"cod_haddock_fy2026", "source_data","cod","output",Sys.Date())
dir.create(file.path(output_folder), showWarnings = FALSE)


ASAP_file_in<-"WGOM_COD_ASAP_2023_SEL3_2023.DAT"
FullProjectionsSaveFile<-"WGOMCod_Projections.rds"
ProjectedNAASaveFile<-"WGOM_Cod_projected_NAA_from_2024Assessment.dta"
HistoricalNAASaveFile<-"WGOM_Cod_historical_NAA_from_2024Assessment.dta"

# Read in accepted model
mod_accepted <-
  readRDS(file = file.path(input_folder,"mod_base_2023_noBLLS.rds"))

###################################################################################
###################################################################################
#Make sure that the version of WHAM that was used to generate the model is installed
###################################################################################
###################################################################################

# take a look at the version of WHAM used to generate the model.
model_wham_commit<-strsplit(mod_accepted$wham_commit,split="@")[[1]][2]
model_wham_commit<-gsub(")", "", model_wham_commit)

mod_accepted$model_name <- "Accepted"
mod_list <- list(mod_accepted)



all_packages <- installed.packages()

# Install check wham commit, install proper commit if needed
# No wham installed, install the one that matches the model_wham_commit
if("wham" %in% all_packages[, "Package"]==FALSE){
  remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), auth_token=NULL)
}

if (model_wham_commit!=packageDescription("wham")$RemoteSha){
  cat("Installed WHAM commit is", packageDescription("wham")$RemoteSha, "does not match the \n",
      "WHAM commit from projection model.  Changing WHAM version \n",
      "This may take a few minutes\n")
  remotes::install_github(glue("timjmiller/wham@{model_wham_commit}"), auth_token=NULL)
} else{
}
stopifnot(model_wham_commit==packageDescription("wham")$RemoteSha)
# keep getting a warning message about magrittr, but things seem to work.

cat("Model Wham version is", model_wham_commit, "\n")
cat("Installed wham commit is", packageDescription("wham")$RemoteSha,"\n")


###################################################################################
###################################################################################
#End WHAM commit verification
###################################################################################
###################################################################################

library(wham)


################################################################################
################################################################################
# Read in ASAP3 dat file and pick parameters
################################################################################
################################################################################
asap3 <- read_asap3_dat(file.path(input_folder,ASAP_file_in))


# Placeholders and parameters
periods<-12 # there are 12 months in a year
# Which year do you want a projection for, How many projections? Set a seed.
YearProj<-2025
num_NAA_draws<-10000
set.seed(6)

# I use tail(.x, 1) to pick the last "thing" out of the ASAP input file

############
# Parameters that go into the stock assessment
############
# Fraction of year that elapses before SSB Calculation (Jan1=0.0)
cMp1=asap3[[1]]$dat$fracyr_spawn
cFp1=cMp1

# Natural Mortality
cMyr=tail(asap3[[1]]$dat$M,1)
cM=cMyr/periods



############
# Parameters that come out of the stock assessment
############
# Weights at age. Opem the ASAP3 file in the ASAP gui and pick through the pointers.
# cod_jan1_weights=  Matrix 3 for Jan 1
cod_jan1_weights = tail(asap3[[1]]$dat$WAA_mats[[3]],1)

# cod_midyear_weights -- Matrix 3 for computing SSB
cod_midyear_weights = cod_jan1_weights

#  matrix 1 for commercial catch
cod_catch_weights = tail(asap3[[1]]$dat$WAA_mats[[1]],1)


# cod_ssb_weights  -- Matrix 3 for computing SSB
cod_ssb_weights=cod_jan1_weights

# cod_discard_weights matrix 1 for commercial discards
cod_discard_weights = tail(asap3[[1]]$dat$WAA_mats[[1]],1)

# cod_discard_fraction not in the model for commercial
cod_discard_fraction<-cod_discard_weights*0
# cod_maturity
cod_maturity= tail(asap3[[1]]$dat$maturity,1)

# These parameters aren't actually saved anywhere, because I'm not 100% sure of
# the easiest way to save them.








################################################################################
################################################################################
# Pull models to make projections ##############################################
################################################################################
################################################################################





###################################PROJECTIONS #################################
# Set specs ####################################################################
set_specs <- function(mod) {
  bridge <- sum(tail(mod$env$data$agg_catch,1)) #Catch in the last year of the assessment
  Fmsy <- exp(mod$rep$log_FXSPR_static)         #FMSY


  proj.opts_list <-
    list(Model = rep(mod$model_name, times = 1),
         scenario    = c("0.75Fmsy (2025-2027)"), #Scenario 2 from the original projections. This is just a string.
         n.yrs       = rep(list(4), times = 1),   # Number of years is set in in (list(numyears)). Number of scenarios is set with times
         proj_F_opt  = list(c(5, 5, 4, 4)),  # length=numyears.  stack on different things to make different projections. 5=metric tons, 4=an instantanous fishing mortality rate (F)
         proj_Fcatch = list(c(bridge, bridge, rep(0.75 * Fmsy, 2))) #2 # length=numyears
    )
}

proj.opts_list2 <- map_df(mod_list, .f = set_specs)

################################################################################
################################################################################
# Run projections ##############################################################
################################################################################
################################################################################
proj_list <- list()
mod_names <- map_df(mod_list, .f = function(x) data.frame(model_name = x$model_name))
for(i in 1:length(proj.opts_list2$n.yrs)) {

  mod_index <- which(mod_names$model_name == proj.opts_list2$Model[i])

  proj_list[[i]] <-
    project_wham(model = mod_list[[mod_index]],
                 proj.opts = list(n.yrs = proj.opts_list2$n.yrs[[i]],
                                  proj_F_opt  = proj.opts_list2$proj_F_opt[[i]],
                                  proj_Fcatch = proj.opts_list2$proj_Fcatch[[i]]),
                 do.sdrep = T,
                 MakeADFun.silent = T)

  proj_list[[i]]$scenario <- proj.opts_list2$scenario[i]
  proj_list[[i]]$Model <- proj.opts_list2$Model[i]
}

################################################################################
################################################################################
# Create table of results ######################################################
################################################################################
################################################################################
proj_out <-
  map_df(proj_list, .f = function(x) {

    std <- list(TMB:::as.list.sdreport(x$sdrep, what = "Est", report = TRUE),
                TMB:::as.list.sdreport(x$sdrep, what = "Std", report = TRUE))

    logssb <- std[[1]]$log_SSB
    logssb_sd <- std[[2]]$log_SSB
    ssb <- exp(std[[1]]$log_SSB)[,1]
    ssb_90lo <- exp(logssb - qnorm(0.95) * logssb_sd)[,1]
    ssb_90hi <- exp(logssb + qnorm(0.95) * logssb_sd)[,1]


    out <-
      tibble(Model = x$Model,
             scenario = x$scenario,
             Year = x$years_full,
             `F`  = round(apply(exp(x$rep$log_FAA_tot), 1, max),2),
             SSB  = round(ssb, 1),
             `SSB CI (90% low)`  = round(ssb_90lo,1),
             `SSB CI (90% high)` = round(ssb_90hi,1),
             `Catch (Com.)` = round(x$rep$pred_catch[,1],1),
             `Catch (Rec.)` = round(x$rep$pred_catch[,2],1),
             `Catch (Total)` = `Catch (Com.)` + `Catch (Rec.)`) %>%
  filter(Year >= 2023) %>%
  rename(`Projection scenario` = scenario)

    return(out)
  })

################################################################################
################################################################################
# Save the full set of projections
saveRDS(proj_list, file = file.path(output_folder,FullProjectionsSaveFile))
################################################################################
################################################################################


################################################################################
################################################################################
# Get historical and projected NAA
################################################################################
################################################################################
#This pulls objects out of the sdreport. Models are stacked into the proj_list object
std1 <- list(TMB:::as.list.sdreport(proj_list[[1]]$sdrep, what = "Est", report = TRUE),
             TMB:::as.list.sdreport(proj_list[[1]]$sdrep, what = "Std", report = TRUE))


year<-proj_list[[1]]$years_full

# Extract the mean and std dev of log_NAA from the results.
# the 1st dimension of this array contains stock, the second contains region.
# This particular WHAM model only contained 1 stock and 1 region.
NAA_logmean<-std1[[1]]$log_NAA_rep[1,1,,]
NAA_logsd<-std1[[2]]$log_NAA_rep[1,1,,]

#column names
names<-paste0("age",1:ncol(NAA_logmean))

TerminalAssess<-tail(mod_list[[1]]$years_full,1)

# Construct a dataframe of historical Numbers at Age
historical_NAA<-exp(NAA_logmean)
colnames(historical_NAA)<-names
historical_NAA<-as.data.frame(cbind(year,historical_NAA))

historical_NAA <- historical_NAA %>%
  dplyr::filter(year<YearProj)

write_dta(historical_NAA, path=file.path(output_folder,HistoricalNAASaveFile))



# Pick exactly 1 year. See the header.
RowPick<-which(year==YearProj)
stopifnot(length(RowPick)==1)




#extract just 1 row
NAA_logmean<-NAA_logmean[RowPick,]
NAA_logsd<-NAA_logsd[RowPick,]

stopifnot(length(NAA_logmean)==length(NAA_logsd))


#To "bias-correct" the lognormal you would change the SIM_NAA[[ageclass]] line to:
NAA<-list()
for (ageclass in 1:length(NAA_logmean)){
  NAA[[ageclass]]<-rlnorm(num_NAA_draws,NAA_logmean[ageclass]-NAA_logsd[ageclass]^2/2,NAA_logsd[ageclass]) # Feed it straight into rlnorm
}





#smush the list to a Dataframe, give it nice names, add on the year and a replicate number.
NAA<-list2DF(NAA)
colnames(NAA)<-names
NAA <-NAA %>%
  mutate(replicate= row_number(),
         year=YearProj) %>%
  relocate(replicate,year)

write_dta(NAA, path=file.path(output_folder,ProjectedNAASaveFile))


