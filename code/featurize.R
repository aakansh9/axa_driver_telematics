
#
# Create (641) features and save files.
#

source('functions/Driver.Attr.Behav.R')
source('functions/Driver.Discretize.R')
source('functions/Driver.Attr.Topo.R')
source('functions/Trip.Feat.R')
source('functions/Driver.Feat.R')
feature_data_path = 'data/featurized/all-641/'

ncore = 3
library(foreach) # parallel computing to increase speed
library(doMC)
registerDoMC(ncore)
drivers <- list.files(path=raw_data_path, pattern ='\\.csv$',full.names=TRUE)

# run and write progress log in log.txt in feature_data_path
writeLines(c(""), paste0(feature_data_path,'log.txt'))
foreach( i = 1:length(drivers)) %dopar% {
  sink(paste0(feature_data_path,'log.txt'), append=TRUE)
  cat(paste("Starting iteration",i,"\n"))
  sink()
  driver.feat(driveri=i, saveloc=feature_data_path)
}
