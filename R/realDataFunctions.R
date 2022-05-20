
neumayerDraws <- function(param, nObs){neumayerData$multish}
drehJenDraws <- function(param, nObs){drehJenData$un_per_l}

neumayerPDF <- neumayerParamTransform <- neumayerPlotDistr <- neumayerLikelihoodFun <- neumayerLatex <- function(...){NULL}
drehJenPDF <- drehJenParamTransform <- drehJenPlotDistr <- drehJenLikelihoodFun <- drehJenLatex <- function(...){NULL}
  
singleChartDomain <- list(from = .01,to = 1,by = .01)
neumayerChartDomain <- list(singleChartDomain)
drehJensChartDomain <- list(singleChartDomain)

realDataSummaryTable <- function(dataset, maincol, colnameList, descrList ){
  
  #TODO: assert lengths of colnameList, descrList equal
  colnameList <- eval(parse(text = colnameList))
  descrList <- eval(parse(text = descrList))
  dataLong <- dataset %>% select(all_of(maincol), everything()) %>% 
    rowid_to_column( "ID") %>%  select((1:(1+length(colnameList))))
  if(!is.null(colnameList)){colnames(dataLong) <- c("ID", colnameList)}
  
  dataLong <- dataLong %>% pivot_longer(cols = -c("ID"), names_to = c("Variable")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(Variable = factor(Variable, levels = colnameList))
  
  dataSummary <- dataLong %>% group_by(Variable) %>% 
    summarize(
      Mean = mean(value, na.rm = TRUE) %>%  round(2), 
      `Std Dev` = sd(value, na.rm = TRUE) %>%  round(2),
      Min = min(value, na.rm = TRUE) %>%  round(2),
      p50 = quantile(value, probs = .50, na.rm = TRUE) %>%  round(1),
      Max = max(value, na.rm = TRUE) %>%  round(2)
    ) %>%  mutate(Description = descrList) %>% 
    select(Variable, Description, everything())
}


realDataXVals <- function(dataset, xCols){
  
  nRows <- nrow(dataset)
  cbind(`allXConstant (1)`[1:nRows,1],
    dataset %>%  select(xCols))
  
}
