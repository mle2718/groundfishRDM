# small helper to setup up users files for the data.directory.
# Ideally all data is stored somewhere in "Data" inside the repo but not committed
# Most people will store data there.
# The processed data takes up alot of space, so whoever processes the data
# will need to store it elsewhere.


stopifnot(user %in% c("TP", "LCH", "ML", "KB"))
if (user=="LCH"){
  data.dir<-"E:/Lou_projects/groundfishRDM"
} else if (user %in% c("TP","ML", "KB")){
  data.dir<-here("Data")
}

