last <- function(x) { tail(x, n = 1) }

perceptron<-function(path){
  
  #Read File from Path as Matrix
  print("Intiliazing Matrix and Data please wait...")
  trainingData <- as.matrix(read.table(path))
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
  
  # init weight vector
  weight <- rep(0,ncol(trainingWeightData))
  
    
  print (paste0("Number of Rows ",nrow(trainingData)))
  print("Data initialized: begin learning of pattern...")
  
 
  ##########################################################################################
  #Implementiation of the Perceptron Learning Algorithm
  #Choose an arbitrary rowOfTrainingElement in P u N 
  #Check if correctly classified or not and take action respectively
  ##########################################################################################
  errorcount <- 1
  while(errorcount > 0 && overallClassifications < 1000000) {
    
    errorcount <- 0

    for(i in 1:nrow(trainingData)) {
      rowOfTrainingElement <- i
      
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
        errorcount <- errorcount + 1
        breakCondition <- 0
      }
      
      #Detect False Positive Values
      if(trainingData[rowOfTrainingElement,ncol(trainingData)]== 0 && (sum(weight*trainingWeightData[rowOfTrainingElement,]) > 0)){
        errorcount <- errorcount + 1
        falsePositive <- falsePositive +1
        falseClassified <- falseClassified +1
        weight <- weight - trainingWeightData[rowOfTrainingElement,]
        breakCondition <- 0
      }  
      
      overallClassifications <- overallClassifications + 1
    }
  
    recallValues <- c(recallValues,truePositive/(truePositive+falseNegative))
    precisionValues <- c(precisionValues,truePositive/(truePositive+falsePositive))
    
    print(paste0("Error count: ",errorcount," recall: ", last(recallValues), " prec: ",last(precisionValues)))
    if(overallClassifications %% 25000 == 0) { 
      print (paste0("Overall classified examples ",overallClassifications))
    }
  }
  
  print("Pattern learned ! Plotting Precision Recall Curve...")    

  plot(recallValues, precisionValues, xlab = "Recall" , ylab = "Precision")
  result <- cbind(recallValues,precisionValues)
  print("Weigh vector:")
  print(weight)
  return(result)
}
  
# helper funtion for 2-Dim data sets
dataPlot<-function(path){
  trainingData <- as.matrix(read.table(path))
  trainingWeightData <- trainingData[,-ncol(trainingData)]
  color <- c("red","green")
  plot(trainingData,col=color[trainingData[,ncol(trainingData)]+1])
}
