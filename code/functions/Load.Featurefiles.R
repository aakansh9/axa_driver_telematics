#
# Two options are provided by best.trips
# 1. Load features of all trips of a given driver
# 2. Load features of best 10 trips of a given driver based on previous submissions.
#
# subm.no refers to submission files saved in 'submissions/' folder
#

load.featurefiles <- function(featloc, best.trips=F, subm.no){
  alldrivers <- list.files(path=feature_data_path, pattern ='\\.csv$',full.names=TRUE)
  allfeat <- lapply(1:2736, function(i) {fread(alldrivers[i])})
  if (best.trips==T){
    subm_order <- subm.order(subm.no=subm.no, overall=F)
    allfeat_top10 <- lapply(1:2736, function(i){
      trips <- as.numeric(subm_order[[i]]$trip[191:200])
      allfeat[[i]][trips]
      })
    return(allfeat_top10)
  } else{
    return(allfeat)
  }
}
