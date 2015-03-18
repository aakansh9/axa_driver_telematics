# For selecting trips in negative class (0)
#

nclass <- function(not.include, d=5, t=100, featlist){ #  t*d samples
  #alldrivers <- list.files(path=location, pattern ='\\.csv$',full.names=TRUE)
  
  # choose d random drivers (without replacement)
  prob=rep(1,2736)
  prob[not.include]=0
  prob <- prob/sum(prob)
  chosen_drivers <- sample(1:2736, d, replace = F, prob=prob)
  
  # choose t trips
  if (t==10){
    # choose top t=10 trips
    res <- featlist[chosen_drivers]
  } else {
    # choose t random trips
    res <- lapply(1:d, function(i){
      temp <- featlist[[chosen_drivers[i]]]
      temp <- temp[sample(nrow(temp), t), ]
      rownames(temp) <- NULL
      return(temp)
    })
  }
  
  res <- do.call ("rbind", res)
  
  res$class <- rep(0, nrow(res))
  return(list('drivers'=chosen_drivers, 'data'=res))
}