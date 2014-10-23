Perceptron<-function(path){
  #Read File from Path as Matrix
  print("Intiliazing Matrix and Data please wait...")
  exprs <-read.table(path)


  # Initialize Data for plotting tp = true positive , tn = true negative , fp = false positive , fn = false negative examples = number of repeats
  t  <- 0
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  examples <- 0
  x <- 0
  
  #Intitialize gatthering of precision and recall data for every reapat in accessablle arrays
  recall <- c()
  precision <- c()
  #Break Condition - if x elements were correctly classified stop the algorithm
  breaker <- 0
  
  #Choose random w0 weight to start the algorithm
  w <- c(runif(1,-1,1),runif(1,-1,1))
  
  
    
  print (paste0("Number of Rows ",nrow(exprs)))
  print("Data Initializes begin learning of pattern...")
  repeat{
    
    #choose arbitrary x in P u N  - might be better to just choose from 1:nrows(exprs) over and over
    x <- sample(1:nrow(exprs),1)
  
   
    
    
      # Detect True Positive Values
      if(exprs[x,3]== 1 && (w[1]*exprs[x,1]+w[2]*exprs[x,2] > 0)){
        tp <- tp +1
        breaker <- breaker + 1
      }
      
      #Detect True Negative Values
      if(exprs[x,3]== 0 && (w[1]*exprs[x,1]+w[2]*exprs[x,2] <= 0)){
        tn <- tn +1
        breaker <- breaker + 1
      }
      
      #Detect False Negative Values
      if(exprs[x,3]== 1 && (w[1]*exprs[x,1]+w[2]*exprs[x,2] <= 0)){
        fn <- fn +1
        t <- t +1
        w[1] = w[1] + exprs[x,1]
        w[2] = w[2] + exprs[x,2]
        breaker <- 0
      }
      
      #Detect False Positive Values
      if(exprs[x,3]== 0 && (w[1]*exprs[x,1]+w[2]*exprs[x,2] > 0)){
        fp <- fp +1
        t <- t +1
        w[1] = w[1] - exprs[x,1]
        w[2] = w[2] - exprs[x,2]
        breaker <- 0
      }  
    examples <- examples + 1
    
    #Compute Precision and Recall
    
    if(tp != 0 || fn != 0){
      recall[examples] <- tp/(tp+fn)
    }else{
      recall[examples]<- 0
    }
    if(tp != 0 || fp != 0){
      precision[examples] <- tp/(tp+fp)
    }else{
      precision[examples] <- 0
    }
    if(breaker == nrow(exprs)){break}
  }
  
  print("Pattern learned ! Plotting Precision Recall Curve...")    

  #convert the results to a matrix
  recallres <- matrix(recall,nrow = examples,ncol = 1)
  precisionres <- matrix(precision,nrow = examples,ncol = 1)
  plot(recallres,precisionres)
  
  #combine the matrix to a return value
  result <- cbind(recallres,precisionres)
 
  return(result)
}
  
  



