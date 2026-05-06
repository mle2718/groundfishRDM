# Run this file once, to set up your token

#########################################################################
####################RUN this once to setup your token####################
#########################################################################
# Open the project in Rstudio
#
#library(here)

#options(gargle_oauth_cache = here(".secrets"))
#
#library(googledrive)
#
# # Put your email in, then run to authorize your token.
#drive_auth(email = "your email here")
#
#
# # Verify tokens were cached
# list.files(here(".secrets"))

#########################################################################
####################END of token setup ##################################
#########################################################################


# In any code that you have that needs access to google drive, run the following:
# library(here)
# library(googledrive)
# drive_auth(cache = here(".secrets"), email = TRUE)






