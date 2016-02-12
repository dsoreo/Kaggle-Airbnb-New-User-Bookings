#setwd('CURRENT WORKING DIRECTORY')

cat("config.R - Get NDCG score function\n")
source('ndcg.R')
library(xgboost)
library(caret) #for nearZeroVar
library(sqldf)
library(readr)

cat("config.R - Restructure data before running models\n")
source('format_session.R')
gc(reset=TRUE)

#Total 6 models.
model_names <- c("m1","m2","m3","m4","m5","m6")
#Each model is run 5 times and output probabilities are averaged.
seeds <- c(123,1,2,3,4)

PCA_INDEX_1 <- 434 #Starting row index of variables that are found not usable after PCA.
PCA_INDEX_2 <- 452 #Ending row index of variables that are found not usable after PCA.
DT_2014 <- FALSE #Variable to decide if we are using only 2014 data.
DT_3M <- FALSE #Variable to decide if we are using only last 3 months data.
SAMPLE_SIZE <- 0.99 #Variable to see what percent of data would be used for training
XGB_DEPTH <- 10 #Depth of xgboost classifier.
USE_MODEL <- "" #variable flag to run model - m1-m6
USE_SEED <- "" #variable flag for model seed.
NEW_IMPUTE_AGE <- FALSE #flag to re-impute age.

for (itr1 in 1:length(model_names)){
  for (itr2 in 1:length(seeds)){

    PCA_INDEX_1 <- 434 
    PCA_INDEX_2 <- 452 
    DT_2014 <- FALSE 
    DT_3M <- FALSE
    USE_MODEL <- model_names[itr1]
    USE_SEED <- seeds[itr2]
    SAMPLE_SIZE <- 0.99
    XGB_DEPTH <- 10
    NEW_IMPUTE_AGE <- FALSE
    
    if(USE_MODEL %in% c("m2","m4","m6")){
      NEW_IMPUTE_AGE <- TRUE
    }
    
    if(USE_MODEL %in% c("m3","m4")){
      PCA_INDEX_1 <- 404
      PCA_INDEX_2 <- 422
      DT_2014 <- TRUE
      SAMPLE_SIZE <- 0.999
    }
    
    if(USE_MODEL %in% c("m5","m6")){
      PCA_INDEX_1 <- 380
      PCA_INDEX_2 <- 400
      DT_3M <- TRUE
    }
    
    if(USE_MODEL %in% c("m3","m4")){
      XGB_DEPTH <- 8
    }
    if(USE_MODEL %in% c("m5","m6")){
      XGB_DEPTH <- 9
    }
    
    cat ("config.R - Running model ", model_names[itr1], "with seed", seeds[itr2],"\n")    
    source('multi_prob.R')
  }
}

cat("Ensembling outputs and generating final submission file\n")
source('ensemble.R')
cat("Done.... Output in submission_file.csv\n")