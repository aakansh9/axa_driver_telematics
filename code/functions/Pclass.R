# For selecting trips in positive class (1)
#
pclass <- function(driveri, match.list, short.list=NULL, location, featlist){
  drivers <- list.files(path=location, pattern ='\\.csv$',full.names=TRUE)
  res <- fread(drivers[driveri])
  #res <- featlist[[driveri]]
  list <- c(match.list,short.list)
  res <- res[list, ]
  rownames(res) <- NULL
  res$class <- rep(1, nrow(res))
  return(res)
}


