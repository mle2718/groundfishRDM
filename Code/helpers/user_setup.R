# small helper to setup up users files for the data.directory.
# Ideally all data is stored somewhere in "Data" inside the repo but not committed
# Most people will store data there.
# The processed data takes up alot of space, so whoever processes the data
# will need to store it elsewhere.


stopifnot(user %in% c("TP", "LCH", "ML", "KB"))
if (user=="LCH"){
  gf.data.dir<-"E:/Lou_projects/groundfishRDM/2027_mgt_cycle"
} else if (user %in% c("TP","ML", "KB")){
  dir.create(here("Data","2027_mgt_cycle"), showWarnings = TRUE, recursive=TRUE)
  gf.data.dir<-here("Data","2027_mgt_cycle")
}

message("Hello ", user, "  Use the object gf.data.dir in place of here(Data, YYYY_mgt_cycle).")

message("The value of gf.data.dir is: ", gf.data.dir)
