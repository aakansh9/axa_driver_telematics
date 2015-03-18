# 16 Feb 2015, 11:28
#
# Edited: 18 Feb 2015, 16:09
#
# 'threshold' is the max distance travelled (m) below which the path is classified as short.
#


short.trips <- function(driveri, threshold=100){
  location = "~/Projects/Kaggle-Driver-Telemetics-Analysis/data/featurized/all-641/"
  drivers <- list.files(path=location, pattern ='\\.csv$',full.names=TRUE)

  driver_feat <- fread(drivers[driveri])
  return(which(driver_feat$tdist<=threshold))
}

