

driver.feat <- function(driveri, saveloc){
  
  driver_attr <- driver.attr.behav(driveri=driveri, smooth=T)
  trip_list <- split(driver_attr, driver_attr$id)
  f1 <- lapply(trip_list, trip.feat.behav)
  
  driver_attr <- driver.attr.topo(driveri=driveri, smooth=T)
  trip_list <- split(driver_attr, driver_attr$id)
  f2 <- lapply(trip_list, trip.feat.topo)
  
  
  res <- lapply( 1:200, function(i) c( f1[[i]], f2[[i]]))
  res <- do.call('rbind', res)
  colnames(res) <- c('tdist', 'st', 'msd', 'ttime',
                     paste0('s',1:40), paste0('t',1:50), paste0('n',1:30),
                     'avgs', 'avgt', 'avgn',
                     paste0('msgr',1:4), paste0('mtgr',1:4), paste0('mngr',1:4),
                     paste0('vsgr',1:4), paste0('vtgr',1:4), paste0('vngr',1:4),
                     paste0('sgr',1:160), paste0('tgr',1:200), paste0('ngr',1:120),
                     paste0('r',1:10))
  

  drivers <- list.files(path=raw_data_path,pattern ='\\.csv$',full.names=TRUE)
  drivernum <- substr(drivers[driveri],81,nchar(drivers[driveri])-4)
  file_id <- paste0(drivernum,'_',1:200)
  res <- cbind(file_id = file_id, res)
  if (nrow(res)==200 & ncol(res)==(641+1)){
    ## save file
    write.table(res, paste0(saveloc,drivernum,'.csv'), quote=F, sep= ",", row.names =F)
    #return(res)
  }
}

