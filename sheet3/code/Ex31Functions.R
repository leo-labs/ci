news <- function(x){
  if(x < 500) return(0)
  if(x > 5000) return(0)
  -((x-500)*(x-5000))/(4*1265625)
}
poem <- function(x){
  return (-((x-0)*(x-1000))/(4*62500))
}
book <- function(x){
  if(x >10000) return(1)
  if(x <3500) return(0)
  else
    return((x/6500)-7/13)
}

child <- function(x){
  
  if(x < 0) return(0)
  if(x > 14) return(0)
  if(x < 8)  return((x/8))
  if(x >=8) return(-(x/6)+7/3)
}

youngperson <- function(x){
  
  if (x < 8) return(0)
  if (x > 30) return(0)
  if(x <22) return((x/14)-4/7)
  if(x >= 22) return(-(x/8)+15/4)
  
}

oldperson <- function(x){
  
  if(x< 40) return(0)
  if(x > 70)return(1)
  
  return((x/30)-4/3)
  
}

grownup <- function(x){
  
  if(x < 18) return(0)
  if(x > 30) return (1)
  
  return((x/12)-3/2)
  
}

group <- function(x){
  if(x < 5)return (0)
  if(x > 20) return(0)
  if(x < 10) return((x/5)-1)
  if(x >= 10) return(-(x/10)+2)
  
}

class <- function(x){
  
  if(x < 20) return (0)
  if(x > 50)return(0)
  if(x < 30) return ((x/10)-2)
  if(x >= 30) return ((-x/20 )+5/2)  
}

crowd <- function(x){
  
  if(x<30)return(0)
  if(x > 100) return (1)
  
  return((x/70)-3/7)
  
}




plotPeople<-function(){
  x <- seq(from=0,to=100)
  y <- sapply(x,group)
  plot(x,y,type="l",xlab="Red = Group, Blue = School Class, Green = Crowd",col="red")
  x <- seq(from=0,to=100)
  y <- sapply(x,class)
  points(x,y,type="l",col="blue")
  x <- seq(from=0,to=100)
  y <- sapply(x,crowd)
  points(x,y,type="l",ylim=c(0,1),col="green",xlab="crowd")
  
  
}

plotWords<-function(){
  x <- seq(from=0,to=20000)
  y <- sapply(x,book)
  plot(x,y,type="l",xlab="Red = Book, Green = Poem, Blue = Newspaper Article",col="red")
  x <- seq(from=500,to=5000)
  y <- sapply(x,news)
  points(x,y,type="l",col="blue")
  x <- seq(from=0,to=1000)
  y <- sapply(x,poem)
  points(x,y,type="l",ylim=c(0,1),col="green",xlab="crowd")
  
  
}

plotAge<-function(){
  x <- seq(from=0,to=100)
  y <- sapply(x,child)
  plot(x,y,type="l",ylim=c(0,2),xlab="Red = Child, Green = YoungPerson, Blue = GrownUp, Purple = OldPerson",col="red")
  x <- seq(from=0,to=100)
  y <- sapply(x,grownup)
  points(x,y,type="l",col="blue")
  x <- seq(from=0,to=100)
  y <- sapply(x,youngperson)
  points(x,y,type="l",ylim=c(0,2),col="green",xlab="crowd")
  x <- seq(from=0,to=100)
  y <- sapply(x,oldperson)
  points(x,y,type="l",ylim=c(0,2),col="purple",xlab="crowd")
  
  
}

complement<-function(x){
  return (1-x)
}

union<-function(x,y){
  return(max(x,y))
}

intersection<-function(x,y){
  return(min(x,y))
}

plotComplementWords<-function(){
  x <- seq(from=0,to=20000)
  y <- sapply(x,book)
  y <- sapply(y,complement)
  plot(x,y,type="l",xlab="Red = Book, Blue = Newspaper Article",col="red")
  x <- seq(from=0,to=20000)
  y <- sapply(x,news)
  y <- sapply(y,complement)
  points(x,y,type="l",col="blue")
 
}

plotComplementAge<-function(){
  x <- seq(from=0,to=100)
  y <- sapply(x,child)
  y <- sapply(y,complement)
  plot(x,y,type="l",xlab="Red = Child, Blue = Young Person",col="red")
  x <- seq(from=0,to=100)
  y <- sapply(x,youngperson)
  y <- sapply(y,complement)
  points(x,y,type="l",col="blue")
  
}

plotUnionWords<-function(){
  x <- seq(from=0,to=20000)
  y <- sapply(x,book)
  z <- sapply(x,news)
  z <- mapply(union,y,z)
  plot(x,z,type="l",xlab="Number of Words",col="red")
}

plotUnionAge<-function(){
  x <- seq(from=0,to=100)
  y <- sapply(x,child)
  z <- sapply(x,youngperson)
  z <- mapply(union,y,z)
  plot(x,z,type="l",xlab="Age",col="red")
}

plotIntersectionWords<-function(){
  x <- seq(from=0,to=20000)
  y <- sapply(x,book)
  z <- sapply(x,news)
  z <- mapply(intersection,y,z)
  plot(x,z,type="l",xlab="Number of Words",col="red")
}

plotIntersectionAge<-function(){
  x <- seq(from=0,to=100)
  y <- sapply(x,child)
  z <- sapply(x,youngperson)
  z <- mapply(intersection,y,z)
  plot(x,z,type="l",xlab="Age",col="red")
}