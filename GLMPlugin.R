library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")


input <- function(inputfile) {
	pfix = prefix()
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
   # Need to get the three files
   csvfile <<- paste(pfix, parameters["csvfile", 2], sep="/")

   training <<- read.csv(csvfile)
   fitControl <<- readRDS(paste(pfix, parameters["fitControl", 2], sep="/"))
   myType <<- "Regular"
  if ("type" %in% rownames(parameters)) {
  myType  <<- as.integer(parameters["type", 2])
  }

}

run <- function() {}

output <- function(outputfile) {
set.seed(825)
if (myType == "bayes") {
result <- train(Class ~ ., data = training, 
                 method = "bayesglm", 
                 trControl = fitControl)

}

else if (myType == "stepAIC") {
result <- train(Class ~ ., data = training, 
                 method = "glmStepAIC", 
                 trControl = fitControl)

}

else if (myType == "net") {
result <- train(Class ~ ., data = training, 
                 method = "glmnet", 
                 trControl = fitControl)

}
else {
result <- train(Class ~ ., data = training, 
                 method = "randomGLM", 
                 trControl = fitControl)
}
print(result)
saveRDS(result, outputfile)
}
