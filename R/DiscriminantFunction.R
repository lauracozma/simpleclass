# ----------------------------------------------------------------------
# Simple Discriminant Function
# ----------------------------------------------------------------------
#' Simple Discriminant Function
#'
#' Classify the input using a simple Discriminant Function (based on the argmax criterion).
#'
#' @param dataset A two-dimensional data frame with 3 categories where rows are observations and columns are features.
#' @return A list with following elements: predictedLabels, performanceProp.
#' @export
#' @examples
#' # create artificial dataset
#' # loading in required packages
#' if (!require("knitr")) install.packages("knitr")
#' if (!require("rmarkdown")) install.packages("rmarkdown")
#' if (!require("mvtnorm")) install.packages("mvtnorm")
#' if (!require("ggplot2")) install.packages("ggplot2")
#' if (!require("pdist")) install.packages("pdist")
#' create_dataset <- function(noCat1, noCat2, noCat3, muCat1, muCat2, muCat3, sdCat1,
#'          sdCat2,sdCat3, rhoCat1, rhoCat2, rhoCat3,seed=1111) {
#'
#'  sigmaXY <- function(rho, sdX, sdY) {
#'    covTerm <- rho * sdX * sdY
#'    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
#'                       2, 2, byrow = TRUE)
#'    return(VCmatrix)
#'  }
#'
#'  genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
#'    if(!is.na(seed)) set.seed(seed)
#'    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
#'    return(rdraws)
#'  }
#'
#'
#'  sigmaCat1 <- sigmaXY(rho=rhoCat1, sdX=sdCat1[1], sdY=sdCat1[2])
#'  sigmaCat2 <- sigmaXY(rho=rhoCat2, sdX=sdCat2[1], sdY=sdCat2[2])
#'  sigmaCat3 <- sigmaXY(rho=rhoCat3, sdX=sdCat3[1], sdY=sdCat3[2])
#'
#'  cat1 <- genBVN(noCat1, muCat1, sigmaCat1, seed = seed)
#'  cat2 <- genBVN(noCat2, muCat2, sigmaCat2, seed = seed+1)
#'  cat3 <- genBVN(noCat3, muCat3, sigmaCat3, seed = seed+2)
#'
#'  dataSet <- as.data.frame(rbind(cat1,cat2, cat3))
#'  Category <- c(rep("Cat1", noCat1), rep("Cat2", noCat2), rep("Cat3", noCat3))
#'  dataSet <- cbind(dataSet, Category)
#'  colnames(dataSet) <- c("weight", "height", "Category")
#'  return(dataSet)
#' }
#'
#' noCat1 <- 50; noCat2 <- 50; noCat3 <- 50;
#' dataSet <- create_dataset(noCat1, noCat2, noCat3, c(5, 200), c(20, 100),c(20, 300), c(2,20), c(4,30), c(5,15), -0.5, 0.3, 0.2)
#' result <-discriminantFunction(dataSet)

discriminantFunction <- function(dataSet){


  # analytical solution
  X <- as.matrix(cbind(ind=rep(1, nrow(dataSet)),
                       dataSet[,c("weight", "height")]))

  Y <- cbind(target1 = c(rep(1, noCat1), rep(0, noCat2), rep(0, noCat3)),
             target2 = c(rep(0, noCat1), rep(1, noCat2), rep(0, noCat3)),
             target3 = c(rep(0, noCat1), rep(0, noCat2), rep(1, noCat3))
  )
  weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

  # compute predictions
  predictions <- X %*% weightsOptim
  predictions

  # classify according to the argmax criterion
  predictedLabels <- rep("Cat1", nrow(dataSet))
  predictedLabels[(predictions==apply(predictions, 1, max))[,2]] <- "Cat2"
  predictedLabels[(predictions==apply(predictions, 1, max))[,3]] <- "Cat3"
  predictedLabels

  # classification algorithm performance
  performance <- table(dataSet$Category, predictedLabels)
  performance
  performanceProp <- prop.table(performance, 1)
  performanceProp



  # grabbing the coefficients
  weights1 <- (weightsOptim[,1]-weightsOptim[,2])[2:3]
  bias1 <- (weightsOptim[,1]-weightsOptim[,2])[1]

  weights2 <- (weightsOptim[,1]-weightsOptim[,3])[2:3]
  bias2 <- (weightsOptim[,1]-weightsOptim[,3])[1]

  weights3 <- (weightsOptim[,2]-weightsOptim[,3])[2:3]
  bias3 <- (weightsOptim[,2]-weightsOptim[,3])[1]


  # return the results
  return(list(predictedLabels,
              performanceProp))

}
