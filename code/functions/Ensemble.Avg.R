# ensemble
ensemble.avg <- function(subm.list){
  prob_list <- lapply(subm.list, function(i){
    loc = paste0('~/Projects/Kaggle-Driver-Telemetics-Analysis/submissions/sub-',i,'.csv')
    subm <- fread(loc)$prob
  })
  loc = paste0('~/Projects/Kaggle-Driver-Telemetics-Analysis/submissions/sub-',subm.list[1],'.csv')
  subm <- fread(loc)
  n <- length(subm.list)
  subm$prob <- rowMeans(matrix(unlist(prob_list), ncol=n, byrow=F))
  return(subm)
}