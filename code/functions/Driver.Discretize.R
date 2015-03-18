# 19 Feb 2015, 18:36
#

# resolves a quadratic equation to rediscretize trajectory
#
# Discretization uses discretrajr from adehabitatLT package.
# Using 'ratio', you may specify the maximum ratio between number of 
# relocations of the new trajectory. If not specified, this maximum
# is equal to 5 times the number of relocations of the raw trajectory
# 'dist' is the new step/distance length in units of coordinates
#
# If the discretization 'dist' is too small then increments are
# automatically done in steps of 5m.

library(adehabitatLT)

driver.discretize <- function(driveri, ratio=5, dist=10, smooth=F){
  
  if (smooth==T){
    drivers <- list.files(path=smoothed_data_path,pattern ='\\.csv$',full.names=TRUE)
  } else {
    drivers <- list.files(path=raw_data_path,pattern ='\\.csv$',full.names=TRUE)
  }
  
  driver_data <- fread(drivers[driveri])
  id <- driver_data$drive
  x <- driver_data$x
  y <- driver_data$y
  res <- split(data.frame(x=x, y=y), id)
  
  redist <- function(df, nnew, step){
    
    x <- df$x
    y <- df$y
    dat <- 1:nrow(df)#df$date
    
    x0 <- c(x[1],y[1])
    dat0 <- dat[1]
    
    n <- length(x)
    nn <- nnew*n
    
    
    toto <- .C("discretrajr", as.double(x), as.double(y), 
               as.double(dat), double(nn), double(nn), as.integer(n), 
               as.integer(nn), double(nn), as.double(x0[1]), 
               as.double(x0[2]), as.double(step), as.double(dat0), 
               integer(1), PACKAGE = "adehabitatLT")
    
    neff <- toto[[13]]-1
    while(TRUE){
      if (neff < (nn-1)) break()
      step <- step+5
      toto <- .C("discretrajr", as.double(x), as.double(y), 
                 as.double(dat), double(nn), double(nn), as.integer(n), 
                 as.integer(nn), double(nn), as.double(x0[1]), 
                 as.double(x0[2]), as.double(step), as.double(dat0), 
                 integer(1), PACKAGE = "adehabitatLT")
      
      neff <- toto[[13]]-1
    }
    if (neff >= (nn - 1))
      stop("too small rediscretization step length. Try to increase \"ratio\"")
    x <- toto[[4]][1:neff]
    y <- toto[[5]][1:neff]
    dat <- toto[[8]][1:neff]
    ans <- cbind.data.frame(x=x, y=y, date = dat)
    return(ans)
  }
  
  res <- lapply(res, redist,nnew=ratio, step=dist)
  
  id <- NULL
  for (i in 1:200){
    id <- c(id, rep(i,nrow(res[[i]])))
  }
  
  h <- do.call ("rbind", res)
  return(data.frame(h, drive = id))
}