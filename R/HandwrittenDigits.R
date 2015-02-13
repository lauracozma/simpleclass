# ----------------------------------------------------------------------
# Handwritten Digit Recognition
# ----------------------------------------------------------------------
#' Handwritten Digit Recognition based on kNN classification.
#'
#' Classify the input using a kNN classification.
#'
#' @param data A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this is training dataset, and if it is "predict" it is test dataset.
#' @param trueClasses A vector with labels for each row in \code{data} if \code{type} is "train", and with labels for each row in \code{memory} if \code{type} is "predict".
#' @param memory A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this argument is not needed, and if it is "predict" it is a training dataset.
#' @param k Number of neighbors that the classifier should use. It has to be an odd number.
#' @param p Distance metric the classifier should use, the value can be either 1, 2 or Inf.
#' @param type Whether the goal is to train the classifier or predict classes of new observations based on past ones. The value can be either "train" or "predict".
#' @return A list with following elements: predictedClasses, prob, accuracy and errorCount.
#' @export
#' @import assertthat
#' @examples
#' # create artificial dataset
#' inputsTrain  <- cbind(sample(0:9, 500, replace=TRUE), matrix(rep(runif(256,min = -1, max = 1), 500), 500))
#' inputsTest  <- matrix(rep(runif(256,min = -1, max = 1), 200), 200)
#' classesTrain <- inputsTrain[,1]
#'
#' # get the kNN predictions for the train set
#' trainResults <- HandwrittenDigits(inputsTrain[,2:257], classesTrain, k=15, p=2, type="train")
#' # get the kNN predictions for the test set
#' testResults <- HandwrittenDigits(data=inputsTest[,1:256], trueClasses=classesTrain, memory=inputsTrain[,2:257], k=15, p=2, type="predict")



HandwrittenDigits <- function(data, trueClasses=NULL, memory=NULL,
                           k=1, p=2, type="train") {
  # test the inputs
  library(assertthat)
  not_empty(data); not_empty(trueClasses);
  if (type=="train") {
    assert_that(nrow(data)==length(trueClasses))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.count(k);
  assert_that(p %in% c(1, 2, Inf))
  if (type=="predict") {
    assert_that(not_empty(memory) &
                  ncol(memory)==ncol(data) &
                  nrow(memory)==length(trueClasses))
  }



  # mode function
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Compute the distance between each point and all others
  noObs <- nrow(data)

  # if we are making predictions on the test set based on the memory,
  # we compute distances between each test observation and observations
  # in our memory
  if (type=="train") {
    predictionId <- 1
    distMatrix <- matrix(NA, noObs, noObs)
    for (obs in 1:noObs) {

      # getting the probe for the current observation
      probe <- as.numeric(data[obs,])
      probeExpanded <- matrix(rep(probe, each=noObs), nrow=noObs)

      # computing distances between the probe and exemplars in the
      # training data
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(data -
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(data - probeExpanded), 1, max)
      }
    }
  } else if (type == "predict") {
    predictionId <- 0
    noMemory <- nrow(memory)
    distMatrix <- matrix(NA, noObs, noMemory)
    for (obs in 1:noObs) {

      # getting the probe for the current observation
      probe <- as.numeric(data[obs,])
      probeExpanded <- matrix(rep(probe, each=noMemory), nrow=noMemory)

      # computing distances between the probe and exemplars in the memory
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(memory -
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
      }
    }
  }

  # Sort the distances in increasing numerical order and pick the first
  # k elements
  neighbors <- apply(distMatrix, 1, order)

  # Compute and return the most frequent class in the k nearest neighbors

  prob <- predictedClasses <-  rep(NA, noObs)

  for (obs in 1:noObs) {
    prob[obs] <- mode(trueClasses[neighbors[(1+predictionId):
                                              (k+predictionId), obs]])



    if(prob[obs] ==1) { predictedClasses[obs] <- 1}
    else {
      if(prob[obs] == 2) {predictedClasses[obs] <- 2} else {
        if(prob[obs] == 3) {predictedClasses[obs] <- 3} else {
          if(prob[obs] == 4) {predictedClasses[obs] <- 4} else {
            if(prob[obs] == 5) {predictedClasses[obs] <- 5} else {
              if(prob[obs] == 6) {predictedClasses[obs] <- 6} else {
                if(prob[obs] == 7) {predictedClasses[obs] <- 7} else {
                  if(prob[obs] == 8) {predictedClasses[obs] <- 8} else {
                    if(prob[obs] == 9) {predictedClasses[obs] <- 9} else {
                      if(prob[obs] == 0) {predictedClasses[obs] <- 0}
                    }}}}}}}}}}



  # examine the performance, available only if training
  if (type=="train") {
    errorCount <- table(predictedClasses, trueClasses)
    accuracy <- mean(predictedClasses==trueClasses)
  } else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }

  # return the results
  return(list(predictedClasses=predictedClasses,
              prob=prob,
              accuracy=accuracy,
              errorCount=errorCount))
}

