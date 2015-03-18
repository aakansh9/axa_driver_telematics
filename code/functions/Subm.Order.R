#
# Given a submission number (i.e submission file in submission/ directory),
# this functions sorts the trips according to increasing probability.
#
# overall is used to select if all the trips of all the drivers are to be sorted together (overall=T)
# or if trips from each driver are to be sorted independently (overall=F).
# In the second case a list of data frames is returned.
#

subm.order <- function(subm.no, overall=T){
  loc = paste0('~/Projects/Kaggle-Driver-Telemetics-Analysis/submissions/sub-',subm.no,'.csv')
  subm <- fread(loc)
  subm$driveri <- rep(c(1:2736), each=200)
  temp <- data.frame(matrix(unlist(strsplit(subm$driver_trip, '_')), ncol=2, byrow=T),
                     stringsAsFactors=F)
  subm$driver <- temp$X1
  subm$trip <- temp$X2
  
  if (overall==F){
    subm <- split(subm, subm$driveri)
    subm <- lapply(1:2736, function(i){ subm[[i]][order(subm[[i]]$prob),] })
    return (subm)
  } else {
    subm <- subm[order(subm$prob),]
    return(subm)
  }
}
