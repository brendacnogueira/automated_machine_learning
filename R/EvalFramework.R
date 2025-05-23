#' Randomly shuffle the data
#'
#' @param x data
#'
#' @export
cv.shuffle <- function(x) x[sample(NROW(x)), ]

#' Create cross validation folds
#'
#' @param x data
#' @param nfolds no of folds
#'
#' @export
cv.folds <- function(x, nfolds) {
  cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
}

#' k-fold cross validation
#' The standard cross validation procedure.
#'
#' @param x data: embedded time series
#' @param nfolds no of folds
#' @param FUN function to apply to each iteration's train and validation. Typically
#' \strong{FUN} is a workflow where a predictive model is applied in a training set
#' and the model is evaluated in the validation set.
#' @param ... further parameters to \code{FUN}
#'
#' @export
kf_xval <- function(x, form, nfolds, FUN, shuffle.rows = TRUE, average_results = FALSE, seedlock=FALSE, percTest=NULL, ...) {
  
  if(seedlock) set.seed(123)
  
  x.test <- NULL
  
  if(!is.null(percTest) && is.numeric(percTest) && percTest>0 && percTest<1) {
    ts.id <- sample(1:nrow(x),nrow(x)*percTest)
    x.test <- x[ts.id,]
    x <- x[-ts.id,]
  }
  
  ind.y <- which(colnames(x)==as.character(form[[2]]))
  
  if (shuffle.rows) x <- cv.shuffle(x)
  f <- cv.folds(x, nfolds)
  
  checkClasses <- foreach(nf=1:nfolds, .combine=c) %do% { length(table(x[f==nf,ind.y])) }
  while(!any(checkClasses)<2) {
    x <- cv.shuffle(x)
    f <- cv.folds(x, nfolds)
    checkClasses <- foreach(nf=1:nfolds, .combine=c) %do% { length(table(x[f==nf,ind.y])) }
    
  }
  
  # This needs to be documented
  sampleBug <- any(sapply(1:10,FUN=function(z) length(table(x[which(f==z),ind.y])))!=2)
  
  # This needs to be documented
  it <- 1000
  while(sampleBug & it>0) { 
    x <- cv.shuffle(x)
    f <- cv.folds(x, nfolds)
    sampleBug <- any(sapply(1:10,FUN=function(z) length(table(x[which(f==z),ind.y])))!=2) 
    it <- it - 1
  } 
  
  cv.res <- c()
  for (i in seq_len(nfolds)) {
    
    #cat("X VAL iter no. ", i, "\n")
    ts.id <- which(f == i)
    
    train <- x[-ts.id, ]
    #print(dim(train))
    val  <- x[ ts.id, ]
    #print(dim(val))
    
    new.res <- FUN(form, train, val, ...)
    cv.res <- rbind(cv.res,new.res)
    
  }
  
  rownames(cv.res) <- 1:nrow(cv.res)
  
  if(average_results) {
    cv.res <- colMeans(cv.res)
  }
  
  x.test.res <- c()
  
  if(!is.null(x.test)) {
    
    message("Commencing with out-of-sample testing...")
    #print("teste")
    
    x.test.res <- FUN(form, train, x.test, ...)
    
    message("Out-of-sample testing completed!")
    
    cv.res <- list(cv.res=cv.res,test=x.test.res)
  }
  
  cv.res
  
}
