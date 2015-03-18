# 19 Feb 2015, 22:28
#

# A data frame is generated consisting of path topology related features
# after discretizing the path with equally spaced points.
# Discretization uses discretrajr from adehabitatLT package.
#
# Using 'ratio', you may specify the maximum ratio between number of 
# relocations of the new trajectory. If not specified, this maximum
# is equal to 5 times the number of relocations of the raw trajectory
# 'dist' is the new step/distance length in units of coordinates
# Smoothing is optional and is performed using a Kalman filter with
# location variance 2 m and speed variance 0.1 m/s2.
#
#

driver.attr.topo <- function (driveri, smooth=F, ratio=5, dist=10){
  
  ## open driveri
  df <- driver.discretize(driveri, ratio=ratio, dist=dist, smooth=smooth)
  x <- df$x
  y <- df$y
  date <- df$date
  id <- df$drive
  res <- split(data.frame(x=x, y=y, time =date), id)
  
  ## create topo attributes
  Topo.Attr <- function(df){ #  input, output = dataframe
    
    df1 <- df[-1, ]
    df2 <- df[-nrow(df), ]
    
    index <- 1:nrow(df)
    dist <- c(sqrt( (df1$x - df2$x)^2 +(df1$y - df2$y)^2 ),NA)
    speed <- dist/c((df1$time - df2$time),NA)
    dx <- c(df1$x - df2$x, NA)
    dy <- c(df1$y - df2$y, NA)
    d2x <- c(dx[-1] - dx[-length(dx)], NA)
    d2y <- c(dy[-1] - dy[-length(dy)], NA)
    abs.angle <- ifelse(dist < 1e-07, NA, atan2(dy, dx))
    rel.ang <- abs.angle[-1] - abs.angle[-nrow(df)]
    rel.ang <- ifelse(rel.ang <= (-pi), 2 * pi + rel.ang, rel.ang)
    rel.ang <- ifelse(rel.ang > pi, rel.ang -2 * pi, rel.ang) 
    rel.angle <- c(NA, rel.ang)
    #totuosity
    #if (nrow(df)>4){
    #  tortuosity <- sqrt((df$x[5:nrow(df)]-df$x[1:(nrow(df)-4)])^2 + (df$y[5:nrow(df)]-df$y[1:(nrow(df)-4)])^2)
    #  tortuosity <- 4*dist[1]/tortuosity
    #  tortuosity <- c(NA,NA,tortuosity,NA,NA)*10-10
    #} else{
    #  tortuosity <- rep(NA,nrow(df))
    #}

    
    ans <- cbind.data.frame(index=index, dist=dist, speed=speed, rel.angle=rel.angle)
    return(ans)
  }
  topo_attr <- lapply(res, Topo.Attr)
  res <- lapply( 1:length(res), function(i) cbind( res[[i]],topo_attr[[i]] ) )

  # bind and return
  h <- do.call ("rbind", res)
  return(data.frame(h, id = id))
}
