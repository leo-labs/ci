library("som")

randomData <- function() { 
  a <- runif(100)
  b <- runif(100)
  c <- runif(100)
  d <- runif(100)
  m <- matrix(c(a,b,c,d),100)
  return(m)
}

trainsom <- function(data, iterations) {
  plot(0:27,0:27,type="n")

  map5 <- som(data, 10, 10, init="random", rlen=iterations, radius=5)
  map3 <- som(data, 10, 10, init="random", rlen=iterations, radius=3)
  map1 <- som(data, 10, 10, init="random", rlen=iterations, radius=1)
  
  dim(data)<-c(10,10,4)
  text(2,11,"input data")
  rasterImage(data, xleft=0, xright=10, ybottom=0, ytop=10)
  
  codevectors5<-map5[["code"]]
  codevectors3<-map3[["code"]]
  codevectors1<-map1[["code"]]
  dim(codevectors5)<-c(10,10,4)
  dim(codevectors3)<-c(10,10,4)
  dim(codevectors1)<-c(10,10,4)
  codevectors5<-unclass(codevectors5)
  codevectors3<-unclass(codevectors3)
  codevectors1<-unclass(codevectors1)
  text(2,26,"radius=5")
  rasterImage(codevectors5, xleft=0, xright=10, ybottom=15, ytop=25)
  text(17,26,"radius=3")
  rasterImage(codevectors3, xleft=15, xright=25, ybottom=15, ytop=25)
  text(17,11,"radius=1")
  rasterImage(codevectors1, xleft=15, xright=25, ybottom=0, ytop=10)
  
  
}
