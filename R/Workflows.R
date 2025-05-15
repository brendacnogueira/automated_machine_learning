#' Learning Workflows
#' 
#' Includes all learning workflows used to build and evaluate the classification models.
#' The algorithms used in the workflows are imported from multiple existing R packages (see below)
#' 
#' @author Nuno Moniz
#'

########################################################

#' Random Forest
wf.RandomForest <- function(form,train,test,ntrees=500,rstrategy=NULL,rs.pars=list(),...) {
  
  require(ranger)
  
  ind.y <- which(colnames(train)==as.character(form[[2]]))
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }
  
  if(!is.null(rstrategy) && rstrategy!="") {
    train <- do.call(what = rstrategy, args = c(list(form=form, train=train), rs.pars))
  }
  
  m <- ranger::ranger(formula = form, data = train, num.trees = ntrees, ...)
  
  p <- predict(m, test)$predictions
  
  eval <- eval.func(form,train,test,p)
  
  eval
  
}

#' Decision Tree (Landmarker)
wf.Tree <- function(form,train,test,maxdepth=4,rstrategy=NULL,rs.pars=list(),...) {
  
  require(rpart)
  
  ind.y <- which(colnames(train)==as.character(form[[2]]))
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }
  
  if(!is.null(rstrategy)) {
    train <- do.call(what = rstrategy, args = c(list(form=form, train=train), rs.pars))
  }
  
  m <- rpart::rpart(formula = form, data = train, control = rpart.control(maxdepth = maxdepth, ...))
  
  p <- predict(m, test, type="class")
  
  eval <- eval.func(form,train,test,p)
  
  eval
  
}

#' Decision Tree (Landmarker)
wf.NaiveBayes <- function(form,train,test,rstrategy=NULL,rs.pars=list(),...) {
  
  require(e1071)
  
  ind.y <- which(colnames(train)==as.character(form[[2]]))
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }
  
  if(!is.null(rstrategy)) {
    train <- do.call(what = rstrategy, args = c(list(form=form, train=train), rs.pars))
  }
  
  m <- e1071::naiveBayes(formula = form, data = train, control = rpart.control(maxdepth = maxdepth, ...))
  
  p <- predict(m, test, type="class")
  
  eval <- eval.func(form,train,test,p)
  
  eval
  
}

#' AutoML - H2O
wf.AutoH2O <- function(form, train, test, maxmodels=10, ...) {
  
  require(h2o)
  
  h2o.init()
  
  ind.y <- which(colnames(train)==as.character(form[[2]]))
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }
  
  tr <- as.h2o(train)
  ts <- as.h2o(test)
  
  y <- as.character(form[[2]])
  x <- setdiff(colnames(train), y)
  
  m <- h2o::h2o.automl(x = x, y = y,
                  training_frame = tr,
                  max_models = maxmodels)
  
  p <- as.vector(h2o.predict(m, ts)$predict)
  
  eval <- eval.func(form,train,test,p)
  
  eval
  
}

#' AutoML - Keras
wf.AutoKeras <- function(form, train, test, ...) {
  
  require(autokeras)
  require(keras)
  
  ind.y <- which(colnames(train)==as.character(form[[2]]))
  
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }
  
  if(!is.factor(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }
  
  train_file <- paste0(tempdir(), "/train.csv")
  write.csv(train, train_file, row.names = FALSE)
  
  test_file_to_predict <- paste0(tempdir(), "/predict.csv")
  write.csv(test[, -ind.y], test_file_to_predict, row.names = FALSE)
  
  test_file_to_eval <- paste0(tempdir(), "/eval.csv")
  write.csv(test, test_file_to_eval, row.names = FALSE)
  
  clf <- model_structured_data_classifier(max_trials = 10)  %>%
    fit(train_file, colnames(train)[ind.y], ...)
  
  (predicted_y <- clf %>% predict(test_file_to_predict))
  
  p <- as.factor(predicted_y[,1]); levels(p) <- levels(train[,ind.y])
  
  eval <- eval.func(form,train,test,p)
  
  eval
  
}
