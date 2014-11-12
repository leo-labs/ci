library("som")

randomData <- function() { 
  m <- matrix(runif(400),100)
  return(m)
}

trainsom <- function(data, iterations, rad) {
  plot(0:27,0:27,type="n")

  map <- som(data, 10, 10, init="random", rlen=iterations, radius=rad)
  
  dim(data)<-c(10,10,4)
  text(2,11,"input data")
  rasterImage(data, xleft=0, xright=10, ybottom=0, ytop=10)
  
  codevectors<-map[["code"]]
  
  dim(codevectors)<-c(10,10,4)
  
  codevectors<-unclass(codevectors)
  
  text(2,26,"code vectors")
  rasterImage(codevectors, xleft=0, xright=10, ybottom=15, ytop=25)
}
