#
# Load the csv for driveri
# Perform Kalman smoothing with given location variance (default 2)
# and speed variance (default 0.1)
#

library(FKF)
smooth.kalman <- function(driveri, loc.variance=2, spd.variance=0.1){
  
  ## open driveri
  drivers <- list.files(path=raw_data_path, pattern ='\\.csv$',full.names=TRUE)
  driver_data <- fread(drivers[driveri])
  drivernum <- substr(drivers[driveri],81,nchar(drivers[driveri])-4)
  
  id <- driver_data$drive
  x <- driver_data$x
  y <- driver_data$y
  res <- split(data.frame(x=x, y=y), id)
  
  ## smooth driveri
  smoother <- function(df,loc.var=2, spd.var=0.1){
    x <- df$x
    y <- df$y
    yt= t(matrix(c(x,y), ncol=2))
    vx0= x[2]-x[1]
    vy0 = y[2]-y[1]
    a0= c(x[1], y[1], vx0, vy0)
    P0=matrix(c(loc.var,0,0,0,0,loc.var,0,0,0,0,spd.var,0,0,0,0,spd.var), ncol=4)
    dt=matrix(c(0,0,0,0), ncol=1)
    ct=matrix(c(0,0), ncol=1)
    Tt=matrix(c(1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1), ncol=4)
    Zt=matrix(c(1,0,0,1,0,0,0,0), ncol=4)
    Ht =matrix(c(0,0,0,0,0,0,0,0,0,0,spd.var,0,0,0,0,spd.var), ncol=4)
    HHt=Ht %*% t(Ht)
    Gt=matrix(c(loc.var,0,0,loc.var), ncol=2)
    GGt=Gt %*% t(Gt)
    myfkf=fkf(a0=a0, P0=P0, dt=dt, ct=ct, Tt=Tt, Zt=Zt, HHt=HHt, GGt=GGt, yt=yt, check.input=T)
    ans <- as.data.frame(t(myfkf$at))
    ans <- cbind.data.frame(x=ans$V1, y=ans$V2)
    return(ans)
  }
  res <- lapply(res, smoother,loc.var=loc.variance, spd.var=spd.variance)
  h <- do.call ("rbind", res)
  
  ## add id and return
  id <- NULL
  for (i in 1:200){
    id <- c(id, rep(i,nrow(res[[i]])))
  }

  write.table(data.frame(h, drive = id), paste0(smoothed_data_path,drivernum, ".csv"), quote=F, sep= ",", 
              row.names =F)
  #return(data.frame(h, drive = id))
  
}

#############################################################


