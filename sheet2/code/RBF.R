###
#Gaussian Radial Basis Function
#Arugments : r = ||x-c_i|| (radius) and sigma = value taken from User
###

RBF <- function (r,sigma){
  result = exp(-r^2/sigma^2)
}
###
#Radial Basis Functio Net
#Computes Weight Vector to given number of Neurons , Input and Output and some constant Sigma 
###
RBFnet <- function(path,neurons,sigma){
  
  readdata = read.table(path)
  input <- readdata[,1]
  output <- readdata[,2]
  centroidsX <- c()
  for(i in 1:neurons){
    
    centroidsX <- c(centroidsX,min(input) + i*((max(input)-min(input))/neurons))
  }
  
  ###
  #Compute RBF Matrix P
  #With Dimenion length Input Vector * neurons
  #The Distance between two one-dimensional and b points is just abs(a-b)
  ###
  p<- c()
  matrix <- matrix(0,length(input),neurons)
  for(i in 1:length(input)){
    for(k in 1:neurons){
      matrix[i,k] <- RBF(abs(input[i]-centroidsX[k]),sigma)
    }
  }
  
  print(ncol(matrix))
  print(nrow(matrix))
  ###
  #Pw = y
  #case N = q
  ###
  if(ncol(matrix) == nrow(matrix)){
    solve(matrix) %*% output
  }
  ###
  #case N < q
  ###1
  if(ncol(matrix) > nrow(matrix)){
    print("This Matrix has many solutions and therefore is of no practical use")
    return()
    
  }
  ###
  #case N > q
  ###
  if(ncol(matrix) < nrow(matrix)){
    #Compute Pseudo Inverse
    temp <- t(matrix)%*%matrix
    print("Dim and Det of Matrix")
    print(dim(temp))
    print(det(temp))
    result <- (solve(t(matrix)%*%matrix)%*%t(matrix))%*%output
  }
  
  yval <- rep(0,length(result))
  plot(result,yval,xlim=c(-300,300),ylim=c(-300,300))
  return (result)
}

SOMTrain <- function(n){
  
  ###
  #Generate Matrix of 4-dimensional vectors
  #
  ###
  a <- runif(n)
  b <- runif(n)
  c <- runif(n)
  d <- runif(n)
  m <- matrix(c(a,b,c,d),n)
  
  ###
  #Train matrix m in SOM and save the result
  #
  ###
  sthing <- som(m, n*2, n)
  
  res <- sthing[[2]]
  plot(res)
  
  ###
  #Values in the 
  #
  ###
  #  for(i in 1:nrow(res)){
  #    for (j in 1:ncol(res)){
  #     
  #    if(res[i,j] < 0)
  #      res[i,j] <- 0
  #    if(res[i,j] >1)
  #      res[i,j] <- 1
  #  }
  
  
  
  
  return(res)
  
}