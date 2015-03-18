#
# To visualize the various behaviour and topological attributes (smoothed/unsmoothed)
# of a single/group of/all trips of a driver.
#

library(ggplot2)
visualize.attr <- function(driveri, tripnum=NULL, xattr, yattr, single=T,
                           topo=F, topo.ratio=5, topo.dist=10, 
                           smooth=F, loc.variance=2, spd.variance=0.1, 
                           atitle=NULL, psize=0.5, ylimit=NULL, xlimit=NULL){
  
  if (topo==F){
    driver <- driver.attr.behav(driveri=driveri, see.R2n=T, smooth=smooth, loc.variance=loc.variance, spd.variance=spd.variance)
  } else{
    driver <- driver.attr.topo(driveri=driveri, ratio=topo.ratio, dist=topo.dist, smooth=smooth)
  }

  if (single==T){
    driver <- driver[driver$id %in% c(tripnum),]
    driver$xattr <- driver[,xattr]
    driver$yattr <- driver[,yattr]
    #driver$xattr <- rollapply(driver$xattr, width = 20, FUN = median, na.pad = TRUE)
    #driver$yattr <- rollapply(driver$yattr, width = 20, FUN = median, na.pad = TRUE)
    
    p <- ggplot(driver, aes(xattr, yattr)) + geom_blank() + xlab(xattr) + ylab(yattr)
    p <- p + geom_point(data = driver, size = psize, aes(colour = factor(id), group = factor(id))) +
        ggtitle( paste0('driveri = ',driveri, atitle) ) + scale_y_continuous(limits = ylimit) +scale_x_continuous(limits = xlimit)
    p <- p  + guides(colour = guide_legend(override.aes = list(size=4))) + theme( legend.title=element_blank())
    
  } else{
    driver$xattr <- driver[,xattr]
    driver$yattr <- driver[,yattr]
    
    p <- ggplot( driver, aes(xattr, yattr) ) + geom_point(size=psize) + xlab(xattr) + ylab(yattr)+
      scale_y_continuous(limits = ylimit) + scale_x_continuous(limits = xlimit)
    p <- p + geom_point(data=driver[driver$id %in% c(tripnum),], size=psize, aes(colour = factor(id), group = factor(id)))+ 
      ggtitle( paste0('driveri=',driveri, atitle) )
    p <- p + guides(colour = guide_legend(override.aes = list(size=4))) + theme( legend.title=element_blank())
    
  }
  plot(p)
  #return(p)
}