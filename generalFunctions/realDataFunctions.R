
neumayerDraws <- function(param, nObs){neumayerData$multish}
drehJenDraws <- function(param, nObs){drehJenData$un_per_l}

neumayerPDF <- neumayerParamTransform <- neumayerPlotDistr <- neumayerLikelihoodFun <- neumayerLatex <- function(...){NULL}
drehJenPDF <- drehJenParamTransform <- drehJenPlotDistr <- drehJenLikelihoodFun <- drehJenLatex <- function(...){NULL}
  
singleChartDomain <- list(from = .01,to = 1,by = .01)
neumayerChartDomain <- list(singleChartDomain)
drehJensChartDomain <- list(singleChartDomain)

realDataSummaryTable <- function(dataset, maincol, colnameList = NULL ){
  
  # dataset <- neumayerData
  # maincol <- "multish"
  # colnameList <- c("Multilateral Aid", "Log Population",
  #               "Log Pop Squared", "Log GDP", "Log Colony",
  #               "Log Distance", "Freedom", "Military Exp")
  
  dataLong <- dataset %>% select(all_of(maincol), everything()) %>% 
    rowid_to_column( "ID") %>%  select((1:(1+length(colnameList))))
  if(!is.null(colnameList)){colnames(dataLong) <- c("ID", colnameList)}
  
  dataLong <- dataLong %>% pivot_longer(cols = -c("ID"), names_to = c("Variable")) %>%
    mutate(value = as.numeric(value))
  
  dataSummary <- dataLong %>% group_by(Variable) %>% 
    summarize(
      Mean = mean(value, na.rm = TRUE), 
      `Std Dev` = sd(value, na.rm = TRUE),
      Min = min(value, na.rm = TRUE),
      p50 = quantile(value, probs = .50, na.rm = TRUE),
      Max = max(value, na.rm = TRUE)
    )
}