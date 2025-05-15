#' 
#' Hyperband
#' 

hyperband <- function(form, train, test, strategy="", learner.bounds, resampling.bounds, nmodels=1, ...) {
  
  lrn_cv_fun <- NULL
  
  if(strategy=="rs.ENN") {
    
    lrn_cv_fun <- function(nrounds, ntrees, k) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(k=k), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  } else if(strategy=="rs.RandUnder") {
    
    lrn_cv_fun <- function(nrounds, ntrees, und.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(und.perc=und.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  } else if(strategy=="rs.RandOver") {
    
    lrn_cv_fun <- function(nrounds, ntrees, ove.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(ove.perc=ove.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  } else if(strategy=="rs.ImpSamp") {
    
    lrn_cv_fun <- function(nrounds, ntrees, und.perc, ove.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(und.perc=und.perc, ove.perc=ove.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  } else if(strategy=="rs.SMOTE") {
    
    lrn_cv_fun <- function(nrounds, ntrees, und.perc, ove.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(und.perc=und.perc, ove.perc=ove.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  } else if(strategy=="rs.TomekUnder") {
    
    lrn_cv_fun <- function(nrounds, ntrees) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=NULL, seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  } else {
    
    lrn_cv_fun <- function(nrounds, ntrees) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy="", seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      median(lrn_cv[,2])
      
    }
    
  }
  
  OPT_Res <- Hyperband(lrn_cv_fun, maximize = TRUE,
                       bounds = c(learner.bounds,resampling.bounds),
                       R = 10, R_unit = 1L, eta = 3, verbose = TRUE)
  
  res.aux <- c()
  
  foreach(i=1:nmodels) %do% {
    
    if(strategy=="rs.ENN") {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = "rs.ENN",
                              rs.pars = list(k=as.numeric(OPT_Res[i,3])))
      res.aux <- rbind(res.aux,eval)
      
    } else if(strategy=="rs.RandUnder") {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = "rs.RandUnder",
                              rs.pars = list(und.perc=as.numeric(OPT_Res[i,3])))
      res.aux <- rbind(res.aux,eval)
      
    } else if(strategy=="rs.RandOver") {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = "rs.RandOver",
                              rs.pars = list(ove.perc=as.numeric(OPT_Res[i,3])))
      res.aux <- rbind(res.aux,eval)
      
    } else if(strategy=="rs.ImpSamp") {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = "rs.ImpSamp",
                              rs.pars = list(und.perc=as.numeric(OPT_Res[i,3]),ove.perc=as.numeric(OPT_Res[i,4])))
      res.aux <- rbind(res.aux,eval)
      
    } else if(strategy=="rs.SMOTE") {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = "rs.SMOTE",
                              rs.pars = list(und.perc=as.numeric(OPT_Res[i,3]),ove.perc=as.numeric(OPT_Res[i,4])))
      res.aux <- rbind(res.aux,eval)
      
    } else if(strategy=="rs.TomekUnder") {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = "rs.TomekUnder",
                              rs.pars = NULL)
      res.aux <- rbind(res.aux,eval)
      
    } else {
      
      eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res[i,2]),
                              rstrategy = NULL,
                              rs.pars = NULL)
      res.aux <- rbind(res.aux,eval)
      
    }
    
    
  } # EVALUATION
  
  t(res.aux[as.numeric(which(res.aux[,2]==max(res.aux[,2]))[1]),])
  
}




########

bayesoptim <- function(form, train, test, strategy="", learner.bounds, resampling.bounds, nmodels=1, ...) {
  
  lrn_cv_fun <- NULL
  
  if(strategy=="rs.ENN") {
    
    lrn_cv_fun <- function(nrounds, ntrees, k) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(k=k), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      
      list(Score = median(lrn_cv[,2]),
           Pred = 0)
      
    }
    
  } else if(strategy=="rs.RandUnder") {
    
    lrn_cv_fun <- function(nrounds, ntrees, und.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(und.perc=und.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      
      list(Score = median(lrn_cv[,2]),
           Pred = 0)
    }
    
  } else if(strategy=="rs.RandOver") {
    
    lrn_cv_fun <- function(nrounds, ntrees, ove.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(ove.perc=ove.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      
      list(Score = median(lrn_cv[,2]),
           Pred = 0)
    }
    
  } else if(strategy=="rs.ImpSamp") {
    
    lrn_cv_fun <- function(nrounds, ntrees, und.perc, ove.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(und.perc=und.perc, ove.perc=ove.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      
      list(Score = median(lrn_cv[,2]),
           Pred = 0)
    }
    
  } else if(strategy=="rs.SMOTE") {
    
    lrn_cv_fun <- function(nrounds, ntrees, und.perc, ove.perc) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=strategy, rs.pars=list(und.perc=und.perc, ove.perc=ove.perc), seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      
      list(Score = median(lrn_cv[,2]),
           Pred = 0)
    }
    
  } else {
    
    lrn_cv_fun <- function(nrounds, ntrees) {
      
      lrn_cv <- kf_xval(train, form, 10, wf.RandomForest, average_results = FALSE, 
                        rstrategy=NULL, seedlock=FALSE, ntrees=ntrees)
      
      lrn_cv[is.na(lrn_cv)] <- 0
      
      list(Score = median(lrn_cv[,2]),
           Pred = 0)
    }
    
  }
  
  OPT_Res = tryCatch({
    BayesianOptimization(lrn_cv_fun,
                         bounds = c(learner.bounds,resampling.bounds),
                         init_points = 10, n_iter = 2,
                         acq = "ucb", kappa = 2.576, eps = 0.0,
                         verbose = TRUE)
  }, error = function(e) {
    NA
  })
  
  if(is.na(OPT_Res)) {
    OPT_Res = tryCatch({
      BayesianOptimization(lrn_cv_fun,
                           bounds = c(learner.bounds,resampling.bounds),
                           init_points = 50, n_iter = 2,
                           acq = "ucb", kappa = 2.576*3, eps = 0.0,
                           verbose = TRUE)
    }, error = function(e) {
      NA
    })
  }
  
  res.aux <- c()
  
  if(length(OPT_Res)!=1) {
    
    OPT_Res$History <- OPT_Res$History[order(OPT_Res$History$Value,decreasing=TRUE)]
    
    foreach(i=1:nmodels) %do% {
      
      if(strategy=="rs.ENN") {
        
        eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res$History[i,2]),
                                rstrategy = "rs.ENN",
                                rs.pars = list(k=as.numeric(OPT_Res$History[i,3])))
        res.aux <- rbind(res.aux,eval)
        
      } else if(strategy=="rs.RandUnder") {
        
        eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res$History[i,2]),
                                rstrategy = "rs.RandUnder",
                                rs.pars = list(und.perc=as.numeric(OPT_Res$History[i,3])))
        res.aux <- rbind(res.aux,eval)
        
      } else if(strategy=="rs.RandOver") {
        
        eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res$History[i,2]),
                                rstrategy = "rs.RandOver",
                                rs.pars = list(ove.perc=as.numeric(OPT_Res$History[i,3])))
        res.aux <- rbind(res.aux,eval)
        
      } else if(strategy=="rs.ImpSamp") {
        
        eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res$History[i,2]),
                                rstrategy = "rs.ImpSamp",
                                rs.pars = list(und.perc=as.numeric(OPT_Res$History[i,3]),ove.perc=as.numeric(OPT_Res$History[i,4])))
        res.aux <- rbind(res.aux,eval)
        
      } else if(strategy=="rs.SMOTE") {
        
        eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res$History[i,2]),
                                rstrategy = "rs.SMOTE",
                                rs.pars = list(und.perc=as.numeric(OPT_Res$History[i,3]),ove.perc=as.numeric(OPT_Res$History[i,4])))
        res.aux <- rbind(res.aux,eval)
        
      } else {
        
        eval <- wf.RandomForest(form, train, test, ntrees = as.numeric(OPT_Res$History[i,2]),
                                rstrategy = NULL)
        res.aux <- rbind(res.aux,eval)
        
      }
      
      
    } # EVALUATION
    
  } else {
    
    message("Could not optimise. Inserting NA's.")
    res.aux <- rbind(res.aux,c(Accuracy=NA,F1=NA,AUC=NA))
    
  }
  
  t(res.aux[as.numeric(which(res.aux[,2]==max(res.aux[,2]))[1]),])
  
}

