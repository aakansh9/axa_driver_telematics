# 12 Feb 2015, 3:49
#
# Edited: 17 Feb 2015, 13:10
#
# A data frame is generated consisting of behaviour related attributes of driver
# like speed, distance, tangential acceleration, normal acceleration, relative acceleration, ...
# Attribute definitions follow adehabitatLT package terminology.
#


driver.attr.behav <- function (driveri, see.R2n=F, smooth=F, loc.variance=2, spd.variance=0.1){
  
  ## open
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

  ## create speed attributes
  Speed.Attr <- function(df){
    
    df1 <- df[-1, ]
    df2 <- df[-nrow(df), ]

    date <- 1:nrow(df)
    dist <- c(sqrt( (df1$x - df2$x)^2 +(df1$y - df2$y)^2 ),NA)
    dx <- c(df1$x - df2$x, NA)
    dy <- c(df1$y - df2$y, NA)
    d2x <- c(dx[-1] - dx[-length(dx)], NA)
    d2y <- c(dy[-1] - dy[-length(dy)], NA)
    accel <- sqrt(d2x^2 + d2y^2)
    tang.accel <- c(dist[-length(dist)] - dist[-1], NA)
    norm.accel <- sqrt(accel^2 - tang.accel^2)
    abs.angle <- ifelse(dist < 1e-07, NA, atan2(dy, dx)) # !!! is not normalised !!!
    rel.ang <- abs.angle[-1] - abs.angle[-nrow(df)]
    rel.ang <- ifelse(rel.ang <= (-pi), 2 * pi + rel.ang, rel.ang)
    rel.ang <- ifelse(rel.ang > pi, rel.ang -2 * pi, rel.ang) 
    rel.angle <- c(NA, rel.ang)
    
    ans <- cbind.data.frame(time=date, speed = dist, 
                            tang.accel = tang.accel, 
                            norm.accel=norm.accel, rel.angle=rel.angle)
    if (see.R2n == T){
      R2n <- df$x^2 + df$y^2
      ans <- cbind(ans, R2n=R2n)
    }
    return(ans)
  }
  speed_attr <- lapply(res, Speed.Attr)
  res <- lapply( 1:length(res), function(i) cbind( res[[i]],speed_attr[[i]] ) )
  

  ## bind and return
  h <- do.call ("rbind", res)
  return(data.frame(h, id = id))
}
