#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

ds.name <- NA

dataset<-NA
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied - the id of the dataset", call.=FALSE)
} else {
  ds.name <- as.character(args[1])
}

if(file.exists(ds.name)) {
  #load(ds.name) # should import an object named 'dataset'
  dataset<-readRDS(ds.name)
  
}

ds <- dataset$data
form <- dataset$formula


#setwd("~/AutoResamplingClassification")

source("R/Setup.R")


load("MetaDF.RData")
meta.df$Dataset <- NULL

metadb <- getMetaFeatures(ds, form)

meta.df.aux <- meta.df[,names(metadb)]

meta.df.aux.unique <- unique(meta.df.aux)

res_comparison <- apply(meta.df.aux.unique, 1, FUN=function(x) sum(x[1:5]==metadb[1,1:5]))

if(length(which(res_comparison == 5)>0)) {
  stop("Already in our database.")
} 

#ds <- 1

gs.RF <- as.data.frame(list(ntrees=c(100,250,500)))
gs.ENN <- as.data.frame(list(Strategy="rs.ENN",k=c(1,3,5)) %>% cross_df())
gs.RU <- as.data.frame(list(Strategy="rs.RandUnder",und.perc=seq(0.1,0.9,by=0.1)) %>% cross_df())
gs.RO <- as.data.frame(list(Strategy="rs.RandOver",ove.perc=c(seq(0.25,1,by=0.25),2,3,4)) %>% cross_df())
gs.IS <- as.data.frame(list(Strategy="rs.ImpSamp",und.perc=seq(0.1,0.9,by=0.1),ove.perc=c(seq(0.25,1,by=0.25),2,3,4)) %>% cross_df()); gs.IS <- gs.IS[-10,]
gs.SM <- as.data.frame(list(Strategy="rs.SMOTE",und.perc=seq(0.1,0.9,by=0.1),ove.perc=c(seq(0.25,1,by=0.25),2,3,4)) %>% cross_df()); gs.SM <- gs.SM[-10,]

str.vec <- c("","rs.ENN","rs.RandUnder","rs.RandOver","rs.ImpSamp","rs.SMOTE","rs.TomekUnder")

gridlist <- list(gs.ENN,gs.RU,gs.RO,gs.IS,gs.SM)

res.final <- c()

res.raw <- c()

#'
#'  RUN WORKFLOWS
#'  

# Import CSV file



for(xi in 1:length(gridlist)) {
  
  x <- gridlist[[xi]]
  
  v <- 1
  
  foreach(r=1:nrow(gs.RF), .combine = rbind) %do% {
    
    foreach(i=1:nrow(x), .combine = rbind) %do% {
      
      message(paste0("(Grid Search) in grid ", xi, " > " , r,": ",i," in ",nrow(x)))
      
      res.df <- kf_xval(ds, form, 10, wf.RandomForest, average_results = FALSE, rstrategy=x[i,1], rs.pars=x[i,-1], seedlock=TRUE, percTest=0.3, ntrees=gs.RF[r,])
      res.df[is.na(res.df)] <- 0
      df.mean <- apply(res.df$cv.res,2,FUN=function(x) mean(x,na.rm=TRUE))
      df.median <- apply(res.df$cv.res,2,FUN=function(x) median(x,na.rm=TRUE))
      
      parms.row <- data.frame(Model=paste0(x[i,1],paste0(".v",v)), ntrees=gs.RF[r,], RStrategy=x[i,1], k=-1, und.perc=-1, ove.perc=-1)
      
      parm.names <- colnames(x)[-1]
      if(length(parm.names)>0) {
        for(p in 1:length(parm.names)) {
          parms.row[,which(colnames(parms.row)==parm.names[p])] <- x[i,parm.names[p]]
        }
      }
      
      res.row <- data.frame(parms.row,mean=t(df.mean),median=t(df.median),test=t(res.df$test))
      res.final <- rbind(res.final, res.row)
      
      res.raw <- rbind(res.raw,cbind(Model=as.character(parms.row[1,]$Model),as.data.frame(res.df$cv.res)))
      
      v<-v+1
      
    }
    
  }
  
}

#'
#'  RUN WORKFLOWS CONCERNING THE APPLICATION OF THE UNDERSAMPLING STRATEGY BASED ON TOMEK LINKS
#'  
res.tl <- foreach(r=1:nrow(gs.RF), .combine = rbind) %do% {
  
  message("(Grid Search) in TomekLinks > ", r, " in ", nrow(gs.RF))
  
  res.df <- kf_xval(ds, form, 10, wf.RandomForest, average_results = FALSE, rstrategy="rs.TomekUnder", rs.pars=NULL, seedlock=TRUE, percTest=0.3, ntrees=gs.RF[r,])
  res.df[is.na(res.df)] <- 0
  
  df.mean <- apply(res.df$cv.res,2,FUN=function(x) mean(x,na.rm=TRUE))
  df.median <- apply(res.df$cv.res,2,FUN=function(x) median(x,na.rm=TRUE))
  
  parms.row <- data.frame(Model=paste0("rs.TomekUnder",paste0(".v",r)), ntrees=gs.RF[r,], RStrategy="rs.TomekUnder", k=-1, und.perc=-1, ove.perc=-1)
  
  res.row <- data.frame(parms.row,mean=t(df.mean),median=t(df.median),test=t(res.df$test))
  res.final <- rbind(res.final, res.row)
  
  cbind(Model=as.character(parms.row[1,]$Model),as.data.frame(res.df$cv.res))
  
}

res.raw <- rbind(res.raw,res.tl)

#'
#'  RUN WORKFLOWS WITHOUT APPLICATION OF RESAMPLING STRATEGIES
#'  
res.none <- foreach(r=1:nrow(gs.RF), .combine = rbind) %do% {
  
  message("(Grid Search) without Resampling Strategies > ", r, " in ", nrow(gs.RF))
  
  res.df <- kf_xval(ds, form, 10, wf.RandomForest, average_results = FALSE, rstrategy=NULL, rs.pars=NULL, seedlock=TRUE, percTest=0.3, ntrees=gs.RF[r,])
  res.df[is.na(res.df)] <- 0
  
  df.mean <- apply(res.df$cv.res,2,FUN=function(x) mean(x,na.rm=TRUE))
  df.median <- apply(res.df$cv.res,2,FUN=function(x) median(x,na.rm=TRUE))
  
  parms.row <- data.frame(Model=paste0("RF",paste0(".v",r)), ntrees=gs.RF[r,], RStrategy="None", k=-1, und.perc=-1, ove.perc=-1)
  
  res.row <- data.frame(parms.row,mean=t(df.mean),median=t(df.median),test=t(res.df$test))
  res.final <- rbind(res.final, res.row)
  
  cbind(Model=as.character(parms.row[1,]$Model),as.data.frame(res.df$cv.res))
  
  
}

res.raw <- rbind(res.raw,res.none)

###

# write.csv(res.raw,file=paste0("Results/exp",ds,".csv"),row.names=FALSE)
# write.csv(res.final,file=paste0("Results/Final/exp",ds,".csv"),row.names=FALSE



res.final$Model <- paste0(res.final$Model,".",res.final$ntrees)
res.final$Model <- gsub("rs.","",res.final$Model)
# res.final$RStrategy <- gsub("rs.","",res.final$RStrategy)

res.final <- res.final[,-which(colnames(res.final) %in% c("beta","mean.Accuracy","mean.F1","median.Accuracy","mean.AUC","median.AUC"))]

res.final["Rank"] <- rank(-res.final$median.F1, ties.method="random")
res.final <- res.final[with(res.final,order(res.final$Rank)),]

#


# #-------------------------------
metadb <- cbind(metadb, res.final)

#

# if(any(colnames(metadb)=="und.perc")) colnames(metadb)[which(colnames(metadb)=="und.perc")] <- "PercU"
# if(any(colnames(metadb)=="ove.perc")) colnames(metadb)[which(colnames(metadb)=="ove.perc")] <- "PercO"
if(any(colnames(metadb)=="median.F1") && any(is.na(metadb$median.F1))) metadb[is.na(metadb$median.F1),]$median.F1 <- 0

metadb <- metadb[,colnames(meta.df)]
meta.df <- rbind(meta.df, metadb)

save(meta.df,file="MetaDF.RData")


# write.csv(res.raw, file="exp.csv")
# write.csv(res.raw, file="exp_final.csv")



#devtools::install_github("b0rxa/scmamp")


#load("MetaDF.RData")
library(xgboost)
library(scmamp)

train <- meta.df

#cnames <- colnames(train)
#cnames[which(cnames=="PercU")] <- "und.perc"
#cnames[which(cnames=="PercO")] <- "ove.perc"
#colnames(train) <- cnames


avg.r <- train[,c("ntrees","RStrategy","k","und.perc","ove.perc","Rank")]

agg.avg <- aggregate(avg.r,by=list(avg.r$RStrategy,avg.r$ntrees,avg.r$und.perc,avg.r$ove.perc,avg.r$k),FUN=function(x) mean(as.numeric(x)))
agg.sd <- aggregate(avg.r,by=list(avg.r$RStrategy,avg.r$ntrees,avg.r$und.perc,avg.r$ove.perc,avg.r$k),FUN=function(x) sd(as.numeric(x)))
agg.var <- aggregate(avg.r,by=list(avg.r$RStrategy,avg.r$ntrees,avg.r$und.perc,avg.r$ove.perc,avg.r$k),FUN=function(x) var(as.numeric(x)))
agg.min <- aggregate(avg.r,by=list(avg.r$RStrategy,avg.r$ntrees,avg.r$und.perc,avg.r$ove.perc,avg.r$k),FUN=function(x) min(as.numeric(x)))
agg.max <- aggregate(avg.r,by=list(avg.r$RStrategy,avg.r$ntrees,avg.r$und.perc,avg.r$ove.perc,avg.r$k),FUN=function(x) max(as.numeric(x)))

colnames(agg.avg)[c(1:5)] <- c("RStrategy","ntrees","und.perc","ove.perc","k"); agg.avg[,6:(ncol(agg.avg)-1)] <- NULL
agg <- agg.avg; colnames(agg)[ncol(agg)] <- "AvgRank"
agg["StdDevRank"] <- agg.sd$Rank; agg["VarRank"] <- agg.var$Rank; agg["MinRank"] <- agg.min$Rank; agg["MaxRank"] <- agg.max$Rank;

train <- merge(train,agg,by=c("ntrees","RStrategy","k","und.perc","ove.perc"),all.x=TRUE)

tr <- train[,-which(colnames(train) %in% c("test.F1","Rank"))]; tr.y <- train[,which(colnames(train)=="test.F1")]; tr$RStrategy <- as.factor(tr$RStrategy)

# xgbTrain <- xgb.DMatrix(data=data.matrix(tr),label=tr.y,group=as.numeric(table(meta.df[,which(colnames(meta.df)=="Dataset")])))
xgbTrain <- xgb.DMatrix(data=data.matrix(tr),label=tr.y)

params <- list(booster = 'gbtree', objective = 'reg:squarederror')

rankModel <- xgb.train(params, xgbTrain, 200, watchlist = list(tr = xgbTrain), eval_metric = 'mae', early_stopping_rounds=5)

####

load("sysdata.rda")

sysdata[["agg.class"]] <- agg
sysdata[["metamodel.class"]]<-rankModel

save(sysdata,file="sysdata.rda")

