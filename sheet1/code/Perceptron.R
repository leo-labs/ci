Perceptron<-function(path){
  
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
  overallClassifications <- 0
  rowOfTrainingElement <- 0

  #Arrays for gathering Recall and Precision Values
  recallValues <- c()
  precisionValues <- c()
  
  #Condition to stop the algorithm
  breakCondition <- 0
  breakConditionMax <- 0
  
  ##########################################################################################
  #Choose random weight to start the algorithm depending on the number of Colums of the Data
  #Initially every element of weight has the same value, but that should not pose a problem
  #There must be another way to instantiate an array with a dynamic number of elements
  ##########################################################################################
  
  weight <- rep(runif(1,-1,1),ncol(trainingWeightData))
  
    
  print (paste0("Number of Rows ",nrow(trainingData)))
  print("Data initialized begin learning of pattern...")
  
 
  ##########################################################################################
  #Implementiation of the Perceptron Learning Algorithm
  #Choose an arbitrary rowOfTrainingElement in P u N 
  #Check if correctly classified or not and take action respectively
  ##########################################################################################
  repeat{
    
    
    rowOfTrainingElement <- sample(1:nrow(trainingWeightData),1)
    
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
    
    overallClassifications <- overallClassifications + 1
    
    ##########################################################################################
    #Compute Precision and Recall values and store them in their respective arrays
    #Recall = truePositive/(truePositive+falseNegative) 
    #Precision = truePositive/(truePositive+falsePositive)
    ##########################################################################################
    
    if(truePositive != 0 || falseNegative != 0){
      recallValues[overallClassifications] <- truePositive/(truePositive+falseNegative)
    }else{
      recallValues[overallClassifications]<- 0
    }
    
    if(truePositive != 0 || falsePositive != 0){
      precisionValues[overallClassifications] <- truePositive/(truePositive+falsePositive)
    }else{
      precisionValues[overallClassifications] <- 0
    }
     
    ##########################################################################################
    #Show Progress by printing number of consecutive correctly classified Examples
    #Stop Algorithm if there are 500? consecutive correctly classified Examples
    ##########################################################################################
    
    if(breakCondition > breakConditionMax){
      breakConditionMax <- breakCondition
      print(breakConditionMax)
    }
    
    if(overallClassifications %% 25000 == 0){ print (paste0("Overall classified examples ",overallClassifications))}
    if(breakCondition == nrow(trainingData)){break}
  }
  
  ##########################################################################################
  #Plot Precision Recall Curve and return P/R Matrix for further use
  ##########################################################################################
  
  print("Pattern learned ! Plotting Precision Recall Curve...")    
  
  recallValueMatrix <- matrix(recallValues,nrow = overallClassifications,ncol = 1)
  precisionValueMatrix <- matrix(precisionValues,nrow = overallClassifications,ncol = 1)
  plot(recallValueMatrix , precisionValueMatrix , xlim=c(0,1) , ylim=c(0,1) , xlab = "Recall" , ylab = "Precision")
  
  result <- cbind(recallValueMatrix,precisionValueMatrix)
 
  return(result)
}
  
  



