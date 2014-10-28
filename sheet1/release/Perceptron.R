last <- function(x) { tail(x, n = 1) }

Perceptron<-function(path,maxInit){
  
  if(maxInit <= 0){
    print("Only positive maximal Iterations allowed")
    return(maxInit)
  }
  #Read File from Path as Matrix
  print("Intiliazing Matrix and Data please wait...")
  trainingData <-as.matrix(read.table(path))
  trainingWeightData <- trainingData[,-ncol(trainingData)]
  
  # Initialize Data for plotting
  falseClassified  <- 0
  truePositive <- 0
  trueNegative <- 0
  falsePositive <- 0
  falseNegative <- 0
  rowOfTrainingElement <- 0
  Iteration <- 0

  #Arrays for gathering Recall and Precision Values
  recallValues <- c()
  precisionValues <- c()
  
  #Condition to stop the algorithm
  breakCondition <- 0
  
  ##########################################################################################
  #Choose random weight to start the algorithm depending on the number of Colums of the Data
  #Initially every element of weight has the same value, but that should not pose a problem
  #There must be another way to instantiate an array with a dynamic number of elements
  ##########################################################################################
  #weight <- rep(runif(1,0,1),ncol(trainingWeightData))
  weight <- rep(0,ncol(trainingWeightData))
    
  print (paste0("Number of Rows ",nrow(trainingData)))
  print("Data initialized begin learning of pattern...")
  
  ##########################################################################################
  #Implementiation of the Perceptron Learning Algorithm
  #Choose an arbitrary rowOfTrainingElement in P u N 
  #Check if correctly classified or not and take action respectively
  ##########################################################################################
  repeat{
    rowOfTrainingElement <- rowOfTrainingElement + 1
      # Detect True Positive Values
      if(trainingData[rowOfTrainingElement,ncol(trainingData)]== 1 && (sum(weight*trainingWeightData[rowOfTrainingElement,]) > 0)){
        truePositive <- truePositive +1
        breakCondition <- breakCondition + 1
      }
      
      #Detect True Negative Values
      if(trainingData[rowOfTrainingElement,ncol(trainingData)]== 0 && (sum(weight*trainingWeightData[rowOfTrainingElement,]) <= 0)){
        trueNegative <- trueNegative +1
        breakCondition <- breakCondition + 1
      }
      
      #Detect False Negative Values
      if(trainingData[rowOfTrainingElement,ncol(trainingData)]== 1 && (sum(weight*trainingWeightData[rowOfTrainingElement,]) <= 0)){
        falseNegative <- falseNegative +1
        falseClassified <- falseClassified +1
        weight <- weight + trainingWeightData[rowOfTrainingElement,]
        breakCondition <- 0
      }
      
      #Detect False Positive Values
      if(trainingData[rowOfTrainingElement,ncol(trainingData)]== 0 && (sum(weight*trainingWeightData[rowOfTrainingElement,]) > 0)){
        falsePositive <- falsePositive +1
        falseClassified <- falseClassified +1
        weight <- weight - trainingWeightData[rowOfTrainingElement,]
        breakCondition <- 0
      } 
    ##########################################################################################
    #Compute Precision and Recall after one Iteration(All Examples classsified once)
    #Recall = truePositive/(truePositive+falseNegative) 
    #Precision = truePositive/(truePositive+falsePositive)
    ##########################################################################################
    if(rowOfTrainingElement == nrow(trainingWeightData)){
        Iteration <- Iteration + 1
        recallValues[Iteration] <- truePositive/(truePositive+falseNegative)
        precisionValues[Iteration] <- truePositive/(truePositive+falsePositive)
  
      
        print(paste0("Error count: ",falseClassified," recall: ", last(recallValues), " prec: ",last(precisionValues)))
        rowOfTrainingElement <- 0
        falseClassified <- 0
    }
     
    ##########################################################################################
    #Stop Algorithm if all Examples are consecutive correctly classified or
    #if the maximum number of iterations has been reached (input from user)
    ##########################################################################################
    if(breakCondition == nrow(trainingData) || Iteration == maxInit){break}
  }
  
  ##########################################################################################
  #Plot Precision Recall Curve and return P/R Matrix for further use
  ##########################################################################################
  
  print("Pattern learned ! Plotting Precision Recall Curve...")    
  
  recallValueMatrix <- matrix(recallValues,nrow = Iteration,ncol = 1)
  precisionValueMatrix <- matrix(precisionValues,nrow = Iteration,ncol = 1)
  plot(recallValueMatrix , precisionValueMatrix , xlab = "Recall" , ylab = "Precision")
  
  result <- cbind(recallValueMatrix,precisionValueMatrix)
  print(weight)
  return(result)
}
  


