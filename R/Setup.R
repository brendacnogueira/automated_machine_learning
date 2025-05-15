#' Setup Library
#' 
#' Includes, definitions, sources, data loading, ...
#' 
#' @author Nuno Moniz
#' 

### LOADING DATA
# load("../../../../Datasets/Classification/ClassificationDatasets_FULL.RData")
#load(".Data/ClassificationDatasets_FULL.RData")

#vec.aux <- sapply(lapply(DSs, FUN=function(x) {
#  tbl.aux <- table(x@data[,ncol(x@data)])
#  min.col <- which(tbl.aux == min(tbl.aux))
#  max.col <- which(tbl.aux == max(tbl.aux))
#  as.numeric(tbl.aux[max.col]/tbl.aux[min.col])
#}), FUN=function(x) x[1])

#DSs <- DSs[which(vec.aux>=1.5)]

########################################################

### IMPORT

# GENERAL
library(foreign) # read arff files
library(Metrics) # evaluation metrics
library(tidyverse) # Grid search
library(foreach) # parallel computing

# LEARNING ALGORITHMS
library(nnet) # multinomial logistic regression
library(RWeka) # various classification algorithms
library(ranger) # random forest

# META FEATURES
library(moments)
library(infotheo)
library(ECoL)

# RESAMPLING STRATEGIES
library(UBL)
library(smotefamily)
library(ROSE)

# AUTO ML FRAMEWORKS
library(autokeras)
library(h2o)
library(rBayesianOptimization)
library(rHyperband)

# EVALUATION
library(Metrics)
library(AUC)

# ESTIMATION FRAMEWORK
library(performanceEstimation)

########################################################

# CLASS DEFINITIONS

# Definition of class Dataset for database
setClass("Dataset", representation(data="data.frame",ID="numeric",formula="formula",nomvars="numeric",numvars="numeric",cases="numeric",source="character"))


########################################################

# SOURCES

# Load user-defined functions
source("R/Utils.R")

# Load Learning Algorithms
source("R/Workflows.R")

# Resampling Strategies
source("R/ResamplingStrategies.R")

# Meta functions
source("R/Metafunctions.R")

# Get metadata vector
source("R/Metafeatures.R")

# Load Optimisation Methods
source("R/WorkflowsOptimisation.R")

# Evaluation framework (based on the work of V. Cerqueira (https://github.com/vcerqueira/performance_estimation))
source("R/EvalFramework.R")

########################################################

# GMAIL NOTIFICATIONS

# library(gmailr)
# gm_auth_configure(path = "Creds/credentials.json")

# sendmail <- function(subject,body) {
#   test_email <- mime(
#     To = "nunompmoniz@gmail.com",
#     From = "rnotifications2020@gmail.com",
#     Subject = subject,
#     body = body)
#   gm_send_message(test_email)
# }
