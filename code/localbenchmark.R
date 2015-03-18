# Assuming list featlist has all features of all trips of all drivers loaded
# and, featlist.best10 has all features of best 10 trips of all drivers.
#
# Modify pclass() function to activate featlist attribute first.
# 
# match1 is a list of matched trips of driveri=1 (driver 1)
#

loc = feature_data_path


# confp, n, dummyp
i=1
set.seed(9)
confp <- pclass(driveri=i, match.list=match1,
                short.list=NULL, featlist=featlist) # class1

ddummyp <- nclass(not.include= c(i), d=13, t=1, featlist=featlist) # class0
dummyp <- ddummyp$data

nn <- nclass(not.include=c(i,ddummyp$drivers), d=50, t=10, featlist=featlist.best10) # class0
n <- nn$data


# train data
dummyp$class <- rep(1, nrow(dummyp))
train <- rbind.data.frame(confp, dummyp, n)
train <- train[sample(nrow(train)), ] #  shuffle
rownames(train) <- NULL

# test data
dummyp$class <- rep(0, nrow(dummyp))
test <- rbind.data.frame(confp, dummyp)

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

modelle <- gbm( formula=frmla,
               data=train,
               distribution='bernoulli',
               n.trees=1000,
               shrinkage=0.006, #  learning rate
               interaction.depth=3,
               n.minobsinnode=10,
               bag.fraction=0.5, #  subsampling p
               cv.folds=0,
               keep.data=T,
               n.cores=4)

# save(modelle, file='modelle.RData')

# predict
best.iter <- gbm.perf(modelle,method="OOB", plot.it=T)
pred <- predict.gbm(modelle,newdata=test,
                    #n.trees=best.iter,
                    n.trees=1000,
                    type='response')

# roc
roc(test$class, pred, plot=T)

# combined result
res <- cbind.data.frame(file_id=c(as.character(confp$file_id), as.character(dummyp$file_id)), class=test$class, prob=pred)

# result ordered by prob
res <- res[order(res$prob),]

# important features
impfeat<- data.frame(imp=relative.influence(modelle))
impfeat$feat <- rownames(impfeat)
impfeat <- impfeat[order(impfeat$imp, decreasing=T),]

top=220
impvars <- sort(impfeat$feat[1:top])

