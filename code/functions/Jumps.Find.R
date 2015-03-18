###
### 13 Feb 2015, 14:40
### Edited: 9 March 2015, 15:47
###

jumps.find <- function(driveri, threshold=40){
  driver_attr <- driver.attr.create.behav(driveri)
  driver_attr <- driver_attr[(driver_attr$speed > threshold), ] # speed > 40
  driver_attr <- driver_attr[!is.na(driver_attr$speed), ] # remove NA's
  driver_attr <- driver_attr[!duplicated(driver_attr[,c('id')]),]
  return(driver_attr$id)
}

