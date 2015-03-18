#
# 11 Feb 2015, 21:07
#
# Edited: 17 Feb 2015, 15:21
#
#
#
# 'Attributes' refers to a data frame of vector of x,y,rel.angle,tang.accel,
# norm.accel,... for each trip of all drivers.
# 'Features' refers to a single vector corresponding to each trip of all drivers.
# Features for each trip are extracted from its attributes data.
#
# There are two types of features: which are extracted from topo=T
# and which are extracted from topo=F. In both the cases, we assume
# smooth=T and that only the smoothed path represents the nearly actualy
# path and therefore nearly accurate attributes.
# 
# driver.attr.behav() extracts attributes for all trips of a driver
# for which topo=F. driver.attr.topo() extracts attributes for all trips
# of a driver for which topo=T. 
#
# There are two types of features:
#   trip.feat.behav - > topo=F, smooth=T (driver.attr.behav())
#   trip.feat.topo -> topo=T, smooth=T (driver.attr.topo())
#
# There are 4 templates for features:
#   tempA: No. of points of an attribute is in an interval.
#   tempB: No. of points of an attribute is in an interval given another attribute in a given interval.
#   tempC: Mean of an attribute given another attribute in a given interval.
#   tempD: Variance of an attribute given another attribute in a given interval.
#

###
### TEMPLATES
###
### The attr/fattr/gattr must be filtered fist accordingly, to contain 
### the required values.
###
tempA <- function(attr, seqn){
  if (length(attr) == 0){
    f <- rep(NA, length(seqn)-1)
  } else{
    f <- hist(attr, breaks=seqn, plot=F)$density*(seqn[2]-seqn[1])
  }
  return(f) # |f| = |seqn|-1
}

tempB <- function(fattr, gattr, fseq, gseq){
  f <- c()
  attr <- as.data.frame(cbind(fattr,gattr))
  for ( i in 1:(length(gseq)-1) ){
    temp <- attr[ !(  gattr<gseq[i] | gattr>gseq[i+1]  ), ]$fattr
    if ( length(temp) == 0 ) {
      f <- c(f,rep(NA,length(fseq)-1))
    } else{
      f <- c(f, hist(temp, breaks=fseq, plot=F)$density*(fseq[2]-fseq[1]))
    }
  }
  return(f) # |f| = (|fseq|-1)*(|gseq|-1)
}

tempC <- function(fattr, gattr, gseq){
  f <- c()
  attr <- as.data.frame(cbind(fattr,gattr))
  for ( i in 1:(length(gseq)-1) ){
    temp <- attr[ !(  gattr<gseq[i] | gattr>gseq[i+1]  ), ]$fattr
    if ( length(temp) == 0 ) {m=NA} else {m=mean(temp)}
    f <- c(f,m)
  }
  return(f) # |f| = (|gseq|-1)
}

tempD <- function(fattr, gattr, gseq){
  f <- c()
  attr <- as.data.frame(cbind(fattr,gattr))
  for ( i in 1:(length(gseq)-1) ){
    temp <- attr[ !(  gattr<gseq[i] | gattr>gseq[i+1]  ), ]$fattr
    if ( length(temp) == 0 ) {v=NA} else {v=var(temp)}
    f <- c(f,v)
  }
  return(f) # |f| = (|gseq|-1)
}

####
#### FEATURES
####
trip.feat.behav <- function(trip_attr){
  
  # smooth=T, topo=F
  # tdist, straighness, mean squared displacement, ttime
  tdist <- sum(trip_attr$speed[-nrow(trip_attr)])
  st <- sqrt(trip_attr$x[nrow(trip_attr)]^2 + trip_attr$y[nrow(trip_attr)]^2)/tdist
  msd <- var(trip_attr$x) + var(trip_attr$y)
  ttime <- nrow(trip_attr)
  f <- c(tdist, st, msd, ttime)
  
  # smooth=T, topo=F, speed(0,40), tang.accel(-5,5), norm.accel(0,3),
  # speed, tang.accel, norm.accel \neq NA
  # %age of time an attribute is in an interval
  trip_attr <- trip_attr[!(trip_attr$speed > 40), ]
  trip_attr <- trip_attr[!(trip_attr$tang.accel > 5 | trip_attr$tang.accel < -5), ]
  trip_attr <- trip_attr[!(trip_attr$norm.accel > 3 | trip_attr$norm.accel < 0), ]
  trip_attr <- trip_attr[ !( is.na(trip_attr$speed) ), ]
  trip_attr <- trip_attr[ !( is.na(trip_attr$tang.accel) ), ]
  trip_attr <- trip_attr[ !( is.na(trip_attr$norm.accel) ), ]
  #
  s <- tempA(trip_attr$speed, seqn=seq(0,40,1))
  t <- tempA(trip_attr$tang.accel, seqn=seq(-5,5,0.2))
  n <- tempA(trip_attr$norm.accel, seqn=seq(0,3,0.1))
  # Average value of an attribute. Outliers are ignored by trimming as above.
  avg_s <- mean(trip_attr$speed)
  avg_t <- mean(trip_attr$tang.accel)
  avg_n <- mean(trip_attr$norm.accel)
  f <- c(f, s, t, n, avg_s, avg_t, avg_n)
  
  # smooth=T, topo=F, speed(0,40), tang.accel(-5,5), norm.accel(0,3),
  # speed, tang.accel, norm.accel, rel.angle \neq NA
  # Rel.angle trimming is not required. It's included in the feature template function itself.
  # Mean attribute given rel.angle in a given interval.
  trip_attr <- trip_attr[ !( is.na(trip_attr$rel.angle) ), ]
  #
  msgr <- tempC(fattr=trip_attr$speed, gattr=abs(trip_attr$rel.angle), gseq=seq(0,1,0.25))
  mtgr <- tempC(fattr=trip_attr$tang.accel, gattr=abs(trip_attr$rel.angle), gseq=seq(0,1,0.25))
  mngr <- tempC(fattr=trip_attr$norm.accel, gattr=abs(trip_attr$rel.angle), gseq=seq(0,1,0.25))
  # Variance attribute given rel.angle in a given interval.
  vsgr <- tempD(fattr=trip_attr$speed, gattr=abs(trip_attr$rel.angle), gseq=seq(0,1,0.25))
  vtgr <- tempD(fattr=trip_attr$tang.accel, gattr=abs(trip_attr$rel.angle), gseq=seq(0,1,0.25))
  vngr <- tempD(fattr=trip_attr$norm.accel, gattr=abs(trip_attr$rel.angle), gseq=seq(0,1,0.25))
  # %age of the time an attribute is in an interval given rel.angle in a given interval.
  sgr <- tempB(fattr=trip_attr$speed, gattr=abs(trip_attr$rel.angle), fseq=seq(0,40,1), gseq=seq(0,1,0.25))
  tgr <- tempB(fattr=trip_attr$tang.accel, gattr=abs(trip_attr$rel.angle), fseq=seq(-5,5,0.2), gseq=seq(0,1,0.25))
  ngr <- tempB(fattr=trip_attr$norm.accel, gattr=abs(trip_attr$rel.angle), fseq=seq(0,3,0.1), gseq=seq(0,1,0.25)) 
  f <- c(f, msgr, mtgr, mngr, vsgr, vtgr, vngr, sgr, tgr, ngr)

  return(f)
}

trip.feat.topo <- function(trip_attr){
  # smooth=T, topo=T
  # Rel.angle(-1,1) \neq NA
  # %age of the tdist rel.angle is in a given interval.
  trip_attr <- trip_attr[!(trip_attr$rel.angle > 1 | trip_attr$rel.angle < -1), ]
  trip_attr <- trip_attr[ !( is.na(trip_attr$rel.angle) ), ]
  #
  r <- tempA(attr=abs(trip_attr$rel.angle), seqn=seq(0,1,0.1))
  return(r)
}
