#' Resampling Strategies for Imbalanced Classification
#' 
#' Provides an interface to resampling strategies for imbalanced classification tasks from other R packages.
#' 
#' @author Nuno Moniz
#'

########################################################

#' Edited Nearest Neighbour
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param k Number of nearest neighbours to consider (default is 3)
#' @param ... Additional parameters
#'
#' @return
#' @export
#' 
#' @importFrom UBL ENNClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.ENN(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.ENN(form, PimaIndiansDiabetes, k=1)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.ENN <- function(form, train, k=3, ...) {
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::ENNClassif(form = form, dat = train, k = k, dist = distance, ...)[[1]]
  
  new.ds
  
}

########################################################

#' Undersampling with Tomek links
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param rem Indicates if both parts of the Tomek link should be removed ("both") or solely the case from the majority class ("maj")
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL TomekClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.TomekUnder(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.TomekUnder(form, PimaIndiansDiabetes, rem = "maj")
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.TomekUnder <- function(form, train, rem = "maj", ...) {
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::TomekClassif(form = form, dat = train, dist = distance, rem = rem, ...)[[1]]
  
  new.ds
  
}

########################################################

#' Random Undersampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL RandUnderClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.RandUnder(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.RandUnder(form, PimaIndiansDiabetes, und.perc=0.95)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.RandUnder <- function(form, train, und.perc=0.5, ...) {
  
  nms <- classNames(form, train)
  
  lst <- list(und.perc,ifelse(exists("ove.perc"),ove.perc,0)); names(lst) <- nms
  
  new.ds <- UBL::RandUnderClassif(form = form, dat = train, C.perc = lst[1], ...)
  
  new.ds
  
}

########################################################

#' Random Oversampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param ove.perc Oversampling percentage for the minority class (e.g. 20% oversampling corresponds to 0.2)
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL RandOverClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.RandOver(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.RandOver(form, PimaIndiansDiabetes, ove.perc=0.95)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.RandOver <- function(form, train, ove.perc=0.5, ...) {
  
  nms <- classNames(form, train)
  
  lst <- list(ifelse(exists("und.perc"),und.perc,1),(1+ove.perc)); names(lst) <- nms
  
  new.ds <- UBL::RandOverClassif(form = form, dat = train, C.perc = lst[2], ...)
  
  new.ds
  
}

########################################################

#' ADASYN
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param beta Balance level after synthetic case generation - 1 corresponds to fully balanced classes
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL AdasynClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.Adasyn(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.Adasyn(form, PimaIndiansDiabetes, beta=0.95)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.Adasyn <- function(form, train, beta=0.5, ...) {
  
  nms <- classNames(form,train)
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::AdasynClassif(form = form, dat = train, beta = beta, dist = distance, baseClass = nms[1], ...)
  
  new.ds
  
}

########################################################

#' Importance Sampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class (default is 1)
#' @param ove.perc Oversampling percentage for the minority class (default is 1)
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL ImpSampClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.ImpSamp(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.ImpSamp(form, PimaIndiansDiabetes, und.perc = 0.5)
#' new.ds3 <- rs.ImpSamp(form, PimaIndiansDiabetes, ove.perc = 0.5)
#' new.ds4 <- rs.ImpSamp(form, PimaIndiansDiabetes, und.perc = 0.5, ove.perc = 0.5)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' table(new.ds3$diabetes)
#' table(new.ds4$diabetes)
#' 
rs.ImpSamp <- function(form, train, und.perc = 1, ove.perc = 0, ...) {
  
  nms <- classNames(form, train)
  
  lst <- list(und.perc, (1+ove.perc)); names(lst) <- nms
  
  new.ds <- UBL::WERCSClassif(form = form, dat = train, C.perc = lst, ...)
  
  new.ds
  
}

########################################################

#' Synthetic Minority Over-sampling Technique (SMOTE)
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class (default is 1)
#' @param ove.perc Oversampling percentage for the minority class (default is 1)
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL SmoteClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.SMOTE(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.SMOTE(form, PimaIndiansDiabetes, und.perc = 0.5)
#' new.ds3 <- rs.SMOTE(form, PimaIndiansDiabetes, ove.perc = 0.5)
#' new.ds4 <- rs.SMOTE(form, PimaIndiansDiabetes, und.perc = 0.5, ove.perc = 0.5)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' table(new.ds3$diabetes)
#' table(new.ds4$diabetes)
#' 
rs.SMOTE <- function(form, train, und.perc = 1, ove.perc = 0, ...) {
  
  nms <- classNames(form, train)
  
  lst <- list(und.perc, (1+ove.perc)); names(lst) <- nms
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::SmoteClassif(form = form, dat = train, C.perc = lst, dist = distance, ...)
  
  new.ds
  
}

