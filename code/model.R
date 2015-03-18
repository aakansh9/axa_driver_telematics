# Load featurized data into memory
source('functions/Load.Featurefiles.R')
source('functions/Pclass.R')
source('functions/Nclass.R')
library('gbm')

# Load into memory, the top 10 trips of each driver based on previous submission
featurized.best10 <- load.featurefiles(featloc, best.trips=T, subm.no=7)

# subm.create function for driveri
subm.create <- function(i, d=50, t=10, location){
  
  # training data for driver i
  p <- pclass(driveri=i, match.list=1:200, short.list=NULL, location=feature_data_path)
  n <- nclass(not.include=i, d=d, t=t, featlist=featurized.best10)$data
  train <- rbind.data.frame(p,n)
  train <- train[sample(nrow(train)), ] #  shuffle
  rownames(train) <- NULL
  
  # features
  feat <- c('tdist', 'st', 'msd', 'ttime',
            paste0('s',1:40), paste0('t',1:50), paste0('n',1:30),
            'avgs', 'avgt', 'avgn',
            paste0('msgr',1:4), paste0('mtgr',1:4), paste0('mngr',1:4),
            paste0('vsgr',1:4), paste0('vtgr',1:4), paste0('vngr',1:4),
            paste0('sgr',1:160), paste0('tgr',1:200), paste0('ngr',1:120),
            paste0('r',1:10))
  
  # model
  frmla <- as.formula(paste('class ~', paste(feat, collapse='+')))
  mygbm <- gbm( formula=frmla,# formula
                distribution='bernoulli',
                data=train, #  dataset
                n.trees=1000, #  iterations
                shrinkage=0.006, #  learning rate
                interaction.depth=3,
                n.minobsinnode=10,
                bag.fraction=0.5, #  subsampling p
                cv.folds=0,
                keep.data=F,
                n.cores=1)
  # predict
  pred <- predict.gbm(
    mygbm,
    newdata=p,
    n.trees=1000,
    #n.trees=gbm.perf(mygbm,method="OOB", plot.it=F, oobag.curve=F)
    type='response'
  )
  
  # save result
  df <- cbind.data.frame(driver_trip=p$file_id, prob=pred)
  return(df)
}

# create submission for all drivers
subm_path ='submissions/'

ncore=4 # parallel processing
drivers <- list.files(path=raw_data_path,pattern ='\\.csv$',full.names=TRUE)
registerDoMC(ncore)

# run and write progress log in log.txt in 'submissions/' directory
writeLines(c(""), paste0(subm_path, "log.txt"))
output <- foreach(i=1:length(drivers)) %dopar% {
  sink(paste0(subm_path, "log.txt"), append=TRUE)
  cat(paste("Starting iteration",i,"\n"))
  sink()
  subm.create(i, location=loc, d=50, t=10)
}
subm <- do.call ("rbind", output)
write.table(subm, paste0(subm_path,'subm-8.csv'), quote=F, sep= ",", row.names =F)


# OPTIONAL
# boost by increasing probability of short trips
source('functions/short.trips.R')
short100 <- lapply(1:2736, short.trips, threshold=100)

shorts.boost <- function(shorts.list,  subm.no, boost=0.1){
  loc = paste0(subm_path,'sub-',subm.no,'.csv')
  subm <- fread(loc)
  subm$driveri <- rep(c(1:2736), each=200)
  subm <- split(subm, subm$driveri)
  subm <- lapply(1:2736, function(i){
    p <- subm[[i]][shorts.list[[i]]]$prob
    subm[[i]][shorts.list[[i]]]$prob <- ifelse(p>(1-boost), 1, p+boost)
    return(subm[[i]])
  })
  subm <- do.call('rbind', subm)
  subm$driveri <- NULL
  return(subm)
}

subm <- shorts.boost(shorts.list=short100, subm.no=8)
write.table(subm, paste0(subm_path,'subm-9.csv'), quote=F, sep= ",", row.names =F)
