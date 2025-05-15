getMetaFeatures <- function(ds,form) {
  
  tgt <- which(colnames(ds)==as.character(form[[2]]))
  nms <- classNames(form, ds)
  
  # Number of cases in the dataset
  ncases <- nrow(ds)
  
  # Imbalance Ratio
  imbRatio <- nrow(ds[ds[,tgt]==nms[1],]) / nrow(ds[ds[,tgt]==nms[2],])
  
  # Number of Attributes
  nattributes <- ncol(ds)-1
  
  # Number of numeric and nominal variables
  numvars <- length(as.numeric(which(sapply(ds[,-tgt],is.numeric))))
  nomvars <- nattributes - numvars
  
  # Ratio of cases per attributes
  ratioCasesAttr <- ncases/nattributes
  
  # Fraction of numerical attributes showing outliers
  attributesWithOutliers <- attrWithOutliers(ds,tgt) / nattributes
  
  # IQR
  statsIQR <- statsIQR_NumAttrs(ds,tgt)
  
  # Coefficient of Variation
  statsCoefVar <- statsCoV_NumAttrs(ds,tgt)
  
  # Correlation between Numerical Attributes
  statsCorNumAttrs <- statsCor_NumAttrs(ds,tgt)
  
  # Geary's Kurtosis of Numerical Attributes
  statsGKurNumAttrs <- statsGKur_NumAttrs(ds,tgt)
  
  # Pearson's Kurtosis of Numerical Attributes
  statsPKurNumAttrs <- statsPKur_NumAttrs(ds,tgt)
  
  # Skewness of Numerical Attributes
  statsSkewNumAttrs <- statsSkew_NumAttrs(ds,tgt)
  
  # Maximal Information Coefficient (MIC)
  statsMICNumAttrs <- statsMIC_NumAttrs(ds,tgt)
  
  # Maximum Asymmetry Score (MAS)
  statsMASNumAttrs <- statsMAS_NumAttrs(ds,tgt)
  
  # Maximum Edge Value (MEV)
  statsMEVNumAttrs <- statsMEV_NumAttrs(ds,tgt)
  
  # Minimum Cell Number (MCN)
  statsMCNNumAttrs <- statsMCN_NumAttrs(ds,tgt)
  
  # Total Information Coefficient (TIC)
  statsTICNumAttrs <- statsTIC_NumAttrs(ds,tgt)
  
  # Entropy
  statsEntropy <- statsEnt(ds,tgt)
  
  # Mutual Information of Attributes
  statsMUI <- statsMuI(ds,tgt)
  
  # Measures of Overlapping
  mOverlap <- measOverlap(ds,tgt)
  
  # Percentual Difference between Classes
  pdc <- ClassPD(ds,form)
  
  landmarkers.tree1 <- landmarker.tree(ds, form, maxdepth = 1)
  landmarkers.tree2 <- landmarker.tree(ds, form, maxdepth = 2)
  landmarkers.tree3 <- landmarker.tree(ds, form, maxdepth = 3)
  landmarkers.nb <- landmarker.nb(ds, form)
  
  #landmarkers.rpart <- landmarker.rpart(ds,form)
  #landmarkers.mars <- landmarker.mars(ds,form)
  #landmarkers.knn <- landmarker.knn(ds,form)
  
  df <- data.frame(NumCases=ncases,
                   imbRatio=imbRatio,
                   NumAttributes=nattributes,
                   NumericVars=numvars,
                   NominalVars=nomvars,
                   ratioCasesAttributes=ratioCasesAttr, 
                   attrWithOutliers=attributesWithOutliers,
                   StatsIQR=statsIQR, 
                   StatsCoefVar=statsCoefVar, 
                   StatsCorBetweenNumVars=statsCorNumAttrs, 
                   StatsGKurBetweenNumVars=statsGKurNumAttrs,
                   StatsPKurBetweenNumVars=statsPKurNumAttrs,
                   StatsSkewBetweenNumVars=statsSkewNumAttrs,
                   StatsMICBetweenNumVars=statsMICNumAttrs,
                   StatsMASBetweenNumVars=statsMASNumAttrs,
                   StatsMEVBetweenNumVars=statsMEVNumAttrs,
                   StatsMCNBetweenNumVars=statsMCNNumAttrs,
                   StatsTICBetweenNumVars=statsTICNumAttrs,
                   StatsEntropy=statsEntropy,
                   StatsMutualInfoBetweenNumVars=statsMUI,
                   OverlapMeasures=mOverlap,
                   ClassDiff=pdc,
                   lmkTree1=landmarkers.tree1,
                   lmkTree2=landmarkers.tree2,
                   lmkTree3=landmarkers.tree3,
                   lmkNB=landmarkers.nb)
  
  
  df
  
}
