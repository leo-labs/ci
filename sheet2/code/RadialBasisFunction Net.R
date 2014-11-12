################################################################
#Gaussian Radial Basis Function
#Arugments : r = ||x-c_i|| (radius) and sigma = value taken from User
################################################################

RBF <- function (r,sigma){
  result <- exp(-r^2/sigma^2)
  return(result)
}
################################################################
#Radial Basis Functio Net
#Computes Weight Vector to given number of Neurons, Input and Output and some constant Sigma 
################################################################
RBFnet <- function(path,neurons,sigma){
  
  readdata <- read.table(path)
  input <- readdata[,1]
  output <- readdata[,2]
  center <- c()
  
################################################################
#Compute the centers for the RBF Net Neurons. 
#Divide the distances between max(input) and min(input) by the number of neurons 
#and distribute the centers between both points.
#Another solution to this problem might be another algorithm like k-means Clustering
################################################################  
  for(i in 1:neurons){
    
    center <- c(center,min(input) + i*((max(input)-min(input))/neurons))
  }
################################################################  
#Compute RBF Matrix P
#With Dimenion length Input Vector * neurons
#The Distance between two one-dimensional and b points is just abs(a-b)
################################################################
  p<- c()
  matrix <- matrix(0,length(input),neurons)
  for(i in 1:length(input)){
    for(k in 1:neurons){
      matrix[i,k] <- RBF(abs(input[i]-center[k]),sigma)
    }
  }
  ################################################################
  #Pw = y
  #case N = q
  ################################################################
  if(ncol(matrix) == nrow(matrix)){
    solve(matrix) %*% output
  }
  ################################################################
  #case N < q
  ################################################################1
  if(ncol(matrix) > nrow(matrix)){
    print("This Matrix has many solutions and therefore is of no practical use")
    return()
    
  }
  ################################################################
  #case N > q
  ################################################################
  if(ncol(matrix) < nrow(matrix)){
    #Compute Pseudo Inverse
    result <- (solve(t(matrix)%*%matrix)%*%t(matrix))%*%output
  }
  
  yval <- rep(0,length(result))
  plot(result,yval,xlim=c(-300,300),ylim=c(-300,300))
  return (result)
}
