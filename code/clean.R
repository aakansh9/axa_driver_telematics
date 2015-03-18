# Jan 29, 2015, 5PM
#
# Load raw data (200 csv's for 2736 drivers each)
# Combine the 200 csv's into 1 csv for 2736 drivers each.
# This is for file reading convenience
#

raw_data_path = "data/raw/drivers/"
library(data.table)
library(zoo)

fread.and.modify <- function(file.number, drivers) {
  tmp <- fread(paste0(driver, "/", file.number, ".csv"), 
                          header = T, sep= ",")
  tmp[,drive:= file.number]
  return(tmp)
}

# save a single csv for each driver
drivers <- list.files(path=raw_data_path,full.names=TRUE)
for (i in 1: length(drivers)) {
  driver <- drivers[i]
  drives <- rbindlist(lapply(1:200, fread.and.modify, driver))
  write.table(drives, paste0(driver, ".csv"), quote=F, sep= ",", 
              row.names =F)
}

rm(driver)
rm(drives)

#
# Smooth the raw data and save to smoothed_data_path.
#
smoothed_data_path = "data/smoothed/"
source('functions/Smooth.Kalam.R')
drivers <- list.files(path=raw_data_path, pattern ='\\.csv$',full.names=TRUE) 
for (i in 1:length(drivers)){
  smooth.kalman(driveri=i)
}
