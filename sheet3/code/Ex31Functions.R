news <- function(x){
 -((x-500)*(x-5000))/(4*1265625)
}
poem <- function(x){
  return (-((x-0)*(x-1000))/(4*62500))
}
book <- function(x){
  if(x >150000) return(1)
  if(x <50000) return(0)
  else
  return((x/100000)-0.5)
}

child <- function(x){
  
  if(x < 0) return(0)
  if(x > 14) return(0)
  if(x < 8)  return((x/8))
  if(x >=8) return(-(x/6)+7/3)
}

youngperson <- function(x){
  
  if (x < 14) return(0)
  if (x > 30) return(0)
  if(x <25) return((x/11)-14/11)
  if(x >= 25) return(-(x/5)+6)
  
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




plotFuzzy<-function(){
  x <- seq(from=0,to=200000)
  y <- sapply(x,book)
  plot(x,y,type="l")
  x <- seq(from=500,to=5000)
  y <- sapply(x,news)
  dev.new()
  plot(x,y,type="l")
  x <- seq(from=0,to=1000)
  y <- sapply(x,poem)
  dev.new()
  plot(x,y,type="l",ylim=c(0,1))

  
}

