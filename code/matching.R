#
# This code has not been verified and takes several minutes 
# for each driver.
# Thus may take several days for all 2736 drivers on 4 cores.
#


# driver.match() function matches all 200 trips of a driver with each other 
# using discrete time warping.
#

library(dtw)
driver.match <- function(driveri, mindegree=1, thres=0.008, topo.ratio=5, topo.dist=10, ncore=1){

  driver_attr <- driver.attr.topo(driveri, ratio=topo.ratio, dist=topo.dist, smooth=F)
  trip_list <- split(driver_attr$rel.angle, driver_attr$id)
  
  # find dtw(t1,t2)
  
  trip.match <- function(i,j, thres=thres){
    t1 <- trip_list[[i]]
    t2 <- trip_list[[j]]
    t1 <- t1[!is.na(t1)]
    t2 <- t2[!is.na(t2)]
    if (length(t1) <= 100 | length(t2) <= 100){
      return(0)
    } else if (abs(length(t1)-length(t2)) >= 300){
      return(0)
    } else {
      t1 <- roll_mean(t1, 51)
      t2 <- roll_mean(t2, 51)
      dist1 <- dtw(t1, t2, keep=F, step=asymmetric, 
                   open.end=T, open.begin=T, distance.only=T)
      dist2 <- dtw( t1,-t2 ,keep=F,step=asymmetric, 
                    open.end=T,open.begin=T, distance.only=T)
      #if (plot == T){ plot(dist, type='two',off=1) }
      ans <- min(dist1$normalizedDistance, dist2$normalizedDistance)
      ans <- ifelse(ans<=thres, 1, 0)
      return(ans)
    }
  }

  # find ith column of matrix
  col <- function(i){
    temp <- lapply(trip_list, trip.match, trip_list[[i]])
    temp <- unlist(temp)
    return(cbind(temp)$temp)
  }
  
  
  library(foreach)
  library(doMC)
  registerDoMC(ncore)
  output <- foreach( i = 1:200 ) %dopar% { col(i) }
  
  output <- do.call(rbind, lapply(output, unlist))
  output <- as.matrix((output < dtwthreshold) + 0)
  g <- graph.adjacency(output, mode="directed", diag=F, add.colnames=NA)
  matches <- V(g)[degree(g)>=mindegree]
  return(list('graph'=g, 'matches'=matches)) 
}



######
library(igraph)
output <- trips.match(driveri=1, topo.ratio=5, topo.dist=10, ncore=4) #120s
output <- as.matrix((output < 0.5) + 0)


g <- graph.adjacency(output, mode="directed", diag=F, add.colnames=NA)
#one <- V(g)[degree(g)>0]
#delete.vertices(g,c(1,2))
g <- set.vertex.attribute(g, "name", value=paste("v",1:200,sep=""))
g <- g - V(g)[degree(g)<1]
g <- g - V(g)[degree(g)!=20]
g <- g - V(g)[degree(g)>20]
#V(g)$color <- ifelse(degree(g)>4, 'red', 'black')
V(g)$label.cex <- 0.5
plot(g, vertex.size=0, edge.arrow.size=0 )
plot(g, vertex.size=0, vertex.label=NA, edge.arrow.size=0 )


