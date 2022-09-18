
############################################################
# Distribution Plotter
############################################################
#' @import ggplot2

continuousDistrPlotter <- function(distrDF, paramVal, paramTex,
                                   annotate = TRUE,
                                   annotationX = NULL,
                                   arrow = TRUE,
                                   roundDigits = 1,
                                   discreteOutput = FALSE,
                                   xlims = NULL,
                                   ylims = NULL,
                                   plotColor = baseColor){


  if(is.null(annotationX)){annotationX <- mean(distrDF$drawVal)}
  if(is.null(xlims)){xMinVal <- min(distrDF$drawVal); xMaxVal <- max(distrDF$drawVal)
  } else {xMinVal <- xlims[1]; xMaxVal <- xlims[2]}
  if(is.null(ylims)){yMinVal <- 0; yMaxVal <- max(distrDF$prob)*1.1
  } else {yMinVal <- ylims[1]; yMaxVal <- ylims[2]}
  paramVal <- as.numeric(paramVal)
  annotationX <- as.numeric(annotationX)

  distrDF <- distrDF %>%  filter(drawVal <= xMaxVal, drawVal >= xMinVal, prob <= yMaxVal, prob >= yMinVal)

  p <- ggplot2::ggplot() +
    geom_line(mapping = aes(x = distrDF$drawVal, y = distrDF$prob), color = plotColor , size = 1) +
    labs(x= "y", y = latex2exp::TeX(paste0("P$(y|", paramTex, ")$"))) +
    xlim(xMinVal, xMaxVal) +
    ylim(yMinVal, yMaxVal) +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))


  if(annotate){p <- p +
    annotate("text", x = annotationX, y = .15*max(distrDF$prob,na.rm = TRUE),
             label  = parse(
               text=latex2exp::TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
             parse = TRUE, color = plotColor)}
  if(arrow){p <- p +
    annotate("segment", x = annotationX, y = .1*max(distrDF$prob,na.rm = TRUE), xend = annotationX,
             yend = 0, arrow = arrow(length = unit(0.2, "cm")), color = plotColor)}

  if(discreteOutput){
    p <- p + geom_point(
      mapping = aes(x = distrDF$drawVal, y = distrDF$prob),
                                         color = plotColor,  size = 3, shape = "square")}

  return(p)


}


binaryDistrPlotter <- function(distrDF, paramVal, paramTex,
                               roundDigits = 1,
                               plotColor1 = baseColor,
                               plotColor2 = baseColor){

  p <- ggplot2::ggplot(distrDF, aes(x = drawVal, y = prob, fill = drawVal)) + geom_bar(stat="identity", alpha = .5, color = baseColor) +
    scale_fill_manual(values=c(plotColor1, plotColor2)) +
    labs(x= "y", y = latex2exp::TeX(paste0("P$(y|", paramTex, ")$"))) +
    theme_minimal() +
    ylim(0,1.25) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5)
    ) + annotate("text", x = 2, y = max(distrDF$prob[1]) + .1,
                 label  = parse(
                   text=latex2exp::TeX(paste0("$",paramTex,"$","=",round(paramVal, roundDigits)), output = "character")),
                 parse = TRUE, color = "black", size = 6, fontface = "bold")

  suppressWarnings({ggplot_build(p)})
}

############################################################
# Histogram Maker
############################################################

### takes a vector

histogramMaker <- function(
    data,
    title = "",
    greaterThan = 999,
    annotate = F,
    captionText = NULL,
    ci = NULL,
    border = T,
    nBinsOverride = 40,
    xlims = NULL){

  errMessage <- "No data received. Please refresh or change incorrect parameters and try again."
  if(!is.numeric(data) ||! is.null(ncol(data))){
    return(ggplot2::ggplot() + annotate("text", x = 4, y = 1, size=4, label = paste(errMessage, collapse = " ")) + theme_void())}

  bordColor <- baseColor #if(border){"black"} else{baseColor}

  histData <- data.frame(value = data)
  scaleFUN <- function(x) sprintf("%.0f%%", x)
  # make sure bins include 1
  nBins <- min(nBinsOverride, length(unique(histData$value)))
  breaks <- unique(round(pretty.default(data, nBins-1),2))
  binwidthTmp <- diff(breaks)[1]
  breaks <- c(breaks[1] - binwidthTmp, breaks) # dealing with an off
  tmpVar <- 0
  while(length(breaks) != 0 && length(which(breaks==1)) ==0 && (max(breaks) > greaterThan)) {
    tmpVar <- tmpVar+1
    breaks <- breaks + (tmpVar*(-1)^(tmpVar-1)/100)
  }

  histData <- histData %>%  dplyr::mutate(grtFlag = (value > greaterThan)) %>%
    dplyr::group_by(grtFlag)
  if(length(breaks) == 1) {breaks <- NULL}


  p <- ggplot2::ggplot() +
    aes(x = histData$value, fill = histData$grtFlag) +
    geom_histogram(
      data = histData,
      aes(y=100*..count../sum(..count..)),
      breaks = breaks,bins = 30, alpha = 0.5,color = bordColor, position = "identity") +
    scale_y_continuous(labels = scaleFUN, breaks = seq(0, 100, 10))  +
    scale_fill_manual(values = c(baseColor,baseColor2)) +
    theme_minimal()+
    labs(x = latex2exp::TeX(title)) +
    ylab(element_blank()) +
    labs(title = "", caption = "") +
    theme(legend.position = "none",
          plot.caption = element_text(size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))


  dataMean <- mean(data, na.rm = TRUE)
  dataMin <- min(data, na.rm = TRUE)
  dataMax <- max(data, na.rm = TRUE)
  dataRange <- abs(dataMin- dataMax)
  dataSD <- sd(data, na.rm = TRUE)
  # TODO: harmonize
  ydata <- data %>%
    as.data.frame() %>%  dplyr::mutate(bin = cut(data, breaks, right = F)) %>%
    dplyr::count(bin) %>%
    dplyr::mutate(percent = 100*n/sum(n, na.rm = T))
  yMax <- max(ydata$percent, na.rm = T)


  if(annotate){


    p <- p + annotate(
      "text", x = dataMean, y = .1*yMax, vjust = 1, hjust = "center",
      label  = paste0(round(dataMean,2),"(", round(dataSD,2 ),")"), color = baseColor2) +
      annotate("polygon",
               x = c(dataMean, dataMean + .025*dataRange, dataMean - .025*dataRange),
               y = c(0,.05*yMax,.05*yMax), fill = baseColor2, alpha = .5)
  }

  if(!is.null(captionText)){
    p <- p + labs(caption = captionText)
  }

  if(!is.null(ci)){

    p <- p +
      annotate("segment", x = ci[1], xend = ci[2], y = .05*yMax, yend = .05*yMax, linetype=2, color = baseColor2, alpha = .75) +
      annotate("text", x =  ci[1]-.05*dataRange, y = .1*yMax, vjust = 1, hjust = "left:", label = "80% CI", color = baseColor2)
  }

  if(!is.null(xlims)) {p <- p + xlim(xMinVal, xMaxVal)}

  return(p)

}

############################################################
# Special Ordered Dist Plot
############################################################


orderedDistSpecialPlot <- function(unobsPDF, param){
  # make y star
  if(is.na(unobsPDF)){return(ggplot2::element_blank())}
  muParam <- param[,1]
  thresh <- param[,2:ncol(param)]

  yStar <- seq(-4, 8, .01)

  allModels <- sapply(muParam, function(a){
    function(b){unobsPDF(drawVal = b, param = a)}
  })
  # for each model, here are our y values
  allDensities <- lapply(allModels, function(m){m(yStar)})
  allDensitiesMat <- allDensities %>%  unlist %>%  matrix(ncol = length(yStar), byrow = T)
  sumDensities <- colMeans(allDensitiesMat)

  # browser()
  hjustVal = if(abs(thresh[1,][1] - thresh[1,][2]) > 1.25){1.15} else{}

  densData <- data.frame(
    xAxis = yStar,
    probs = sumDensities
  ) %>% dplyr::mutate(tau = cut(xAxis, breaks = c(-999, thresh[1,], 999),
                         labels = FALSE))

  p <- ggplot2::ggplot(densData, aes(x = xAxis, y = probs)) +
    geom_area(aes(fill = as.character(tau)), alpha = .5) +
    scale_fill_manual(values = cbPalette) +
    geom_vline(mapping = aes(xintercept =  c(-999, thresh[1,], 999)[tau], color = as.character(tau))) +
    geom_text(aes(c(-999, thresh[1,], 999)[tau],.45,
                  label = paste0("Tau",as.character(tau-2)),
                  hjust = ifelse(..x.. > 0, -.25, 1.15), color = as.character(tau))) +
    scale_color_manual(values = cbPalette) +
    xlim(-4,8) +
    ylim(0, .5) +
    labs(x = "Y*", y = "P(y*)") +
    theme_minimal() +
    theme(text = element_text(family = "sans"),
          legend.position = "none",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5)
    )

  suppressWarnings({ggplot_build(p)})
}



############################################################
# MLE by Hand
############################################################


histAndDensity <- function(data, domain, pdfFun, assumedParam, binWidthVal = .5, multiModel = F, range){

  histData <- tibble(value = data)

  if(multiModel == F){
    functionFun <- pdfFun; assumedParam <- assumedParam[1];dIntegral <- 1} else{
      # TODO: remove code repetition
      allModels <- apply(assumedParam , 1, function(a){
        function(b){pdfFun(drawVal = b, param = a)}
      })
      # for each model, here are our y values
      drawVals <- seq(domain[1], domain[2], .01)

      allDensities <- lapply(allModels, function(m){m(drawVals)})
      allDensitiesMat <- allDensities %>%  unlist %>%
        matrix(ncol = length(drawVals), byrow = T)
      sumDensities <- colMeans(allDensitiesMat)


      analyticalDistr <- data.frame(drawVal = drawVals, prob = sumDensities)
      dIntegral <- mean(analyticalDistr$prob[is.finite(analyticalDistr$prob)])*
        (domain[2]-domain[1])
      functionFun <- function(q, z){
        # TODO: why is this so ugly
        sapply(q, function(r) {
          analyticalDistr$prob[which.min(abs(analyticalDistr$drawVal-r))]})
      }
    }

  breaks <- seq(domain[1], domain[2], 1)

  histData <- histData %>%
    filter(value <= domain[2], value >= domain[1])

  ggplot2::ggplot(histData, aes(x = value)) +
    geom_histogram(aes(y=..count../sum(..count..)), breaks = breaks,
                   color = baseColor, fill = baseColor) +
    stat_function(aes(x =seq(domain[1], domain[2], length = nrow(histData))),
                  fun = function(a){
      1/dIntegral *functionFun(a,assumedParam)
      },
                  color = baseColor2, size = 1) +
    labs(x = "y", y = "Observed Density")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.caption = element_text(
            size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 16,
                                      margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16,
                                      margin = unit(c(4, 4, 4, 4), "mm"), color = baseColor)
    ) +
    coord_cartesian(xlim = c(domain[1], domain[2]),
                    ylim = c(range[1], range[2]),
                    expand= F)


}



histAndDensityDiscrete <- function(data, domain, pdfFun, assumedParam, binWidthVal = .5, multiModel = F, range = NA){

  xAxis <- seq(domain[1], domain[2], 1)
  observed <- data.frame(data = xAxis) %>% left_join(
    as.data.frame(table(data)/length(data)) %>%
      dplyr::mutate(data = as.integer(as.character(data))), by = "data") # relative frequency of the data
  colnames(observed) <- c("drawVal", "oprobs")
  observed$oprobs[is.na(observed$oprobs)] <- 0
  hprobs <- sapply(xAxis, function(a){
    if(is.null(dim(assumedParam))){
      pdfFun(a, assumedParam)
    } else {
      mean(sapply(1:nrow(assumedParam), function(b){
        pdfFun(a, assumedParam[b,])}))
    }
  })

  # sapply(1:nrow(assumedParam), )
  hypothesized <- tibble(drawVal = xAxis,hprobs = hprobs)

  histData <- left_join(observed, hypothesized, by = "drawVal")
  scaleFUN <- function(x) sprintf("%.0f%%", x)

  p <- ggplot2::ggplot(histData)  +
    geom_bar(mapping = aes(x = drawVal, y = oprobs),
             stat="identity", alpha = .25, position = "identity",
             fill = baseColor,
             color = baseColor)

  for(j in 1:length(hprobs)){
    p <- p + eval(parse(text = paste0(
      "geom_segment(aes(x = -.5+",xAxis[j],", xend = .5+",xAxis[j],
      ", y = hprobs[",j,"], yend = hprobs[",j,"]),size = 1.2, color = baseColor2)"
    ))) #if GGplot wasn't so goddamn 'clever'....
  }

  yRangeMax <- if(nrow(histData) < 4){max(1, max(histData$oprobs) + .2)} else {.1}

  p <- p + theme_minimal() +
    labs(x = "y", y = "Observed Probability") +
    ylim(0,yRangeMax) +
    theme(legend.position = "none",
          plot.caption = element_text(
            size=12, margin = ggplot2::margin(t = 10), hjust = 0.5),
          axis.text.x = element_blank(),#element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 16,
                                      margin = unit(c(4, 0, 0, 0), "mm")),
          axis.title.y = element_text(size = 16,
                                      margin = unit(c(4, 4, 4, 4), "mm"), color = baseColor)
    )
  ggplot_build(p)

}

multiModelDensity <- function(param, domain, pdf, ...){
  # PDF takes arguments (drawVal, param)

  # Here, param is a 1 or 2 x nObs vector
  # Use it to create a list of models
  allModels <- apply(param, 1, function(a){
    function(b){pdf(drawVal = b, param = a)}
  })
  # for each model, here are our y values
  drawVals <- seq(domain[1], domain[2], .01)

  allDensities <- lapply(allModels, function(m){m(drawVals)})
  allDensitiesMat <- allDensities %>%  unlist %>%  matrix(ncol = length(drawVals), byrow = T)
  sumDensities <- colMeans(allDensitiesMat)

  analyticalDistr <- data.frame(drawVal = drawVals, prob = sumDensities)

  continuousDistrPlotter(analyticalDistr, xlims = domain, ...)
}

############################################################
# MLE
############################################################
MLEPlotFun <- function(MLEVars, paramTex){
  likelihoodDB <- MLEVars$data
  paramHat <- MLEVars$paramHat
  paramSE  <- MLEVars$paramSE


  likelihoodDB <- likelihoodDB[is.finite(likelihoodDB$LogLikelihood),]

  # Set y bounds based on the derivative of chart
  # trying to cut off the asymptotes
  paramRange <- abs(max(likelihoodDB$param) - min(likelihoodDB$param))
  maxLL <- max(likelihoodDB$LogLikelihood)
  rightBound <- maxLL - likelihoodDB$LogLikelihood[which.max(likelihoodDB$LogLikelihood) +1]
  leftBound <- maxLL-likelihoodDB$LogLikelihood[which.max(likelihoodDB$LogLikelihood)-2]

  maxDiff <- max(rightBound, leftBound) *500
  LLDiffs <- diff(likelihoodDB$LogLikelihood)
  likelihoodDB <- likelihoodDB[abs(LLDiffs) < maxDiff,]


  uniqueLL<-sort(unique(likelihoodDB$LogLikelihood))
  maxY <- stats::quantile(likelihoodDB$LogLikelihood,.99, na.rm = TRUE)
  minY <- uniqueLL[2]
  rangeY <- abs(maxY - minY)
  maxY <- minY + 1.2 * rangeY

  likelihoodDB <- likelihoodDB %>%  dplyr::filter(LogLikelihood > minY, QuadraticApprox > minY )

  retPlot <- ggplot2::ggplot() +
    geom_line(data = likelihoodDB, mapping =  aes(x = param, y = LogLikelihood), color = baseColor, size = 1.75, alpha = .5) +
    geom_line(data = likelihoodDB, mapping =  aes(x = param, y = QuadraticApprox),
              color = baseColor3, size = 1, linetype = "dashed")  +
    theme_minimal() +
    xlab(latex2exp::TeX(paste0("Parameter ", paramTex))) +
    ylim(minY,maxY) +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(size = 17),
          axis.text.y = element_text(size = 17),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 17, margin = unit(c(4, 4, 4, 4), "mm"), color = baseColor))  +
    annotate("text", x = stats::quantile(likelihoodDB$param,.3), y = minY + 1.1*rangeY,
             label  = "Quadratic Approx. (from optim)", color = baseColor3, size = 4, fontface = "bold")
  return(retPlot)
}

############################################################
# Functional Forms
############################################################

functionalFormPlot <- function(
    transformFun, paramRange, paramTex = "",
    intrParamTex = "", fixValues = NULL,
    multi = F,margNum = NULL,  xVals = NULL,
    xChoice = NULL, funcRange = NULL, pdfFun = NULL, DGP = T){

  if(length(xChoice) == 0){multi <- F}
  if(length(margNum) ==0){margNum <- 1}
  if(is.na(margNum)){margNum <- 1}
  if(multi){
    if(length(xVals) == 0){return(ggplot2::element_blank())}
    ### code for X vs transformed parameter
    tmpFun <- function(a){
      tmpParams <- fixValues
      tmpX <- colMeans(xVals)
      tmpX[margNum+1] <- a
      transformFun(tmpParams, tmpX, DGP)
    }
    xAxis <-if(substr(xChoice[margNum],0 , stringr::str_length(xChoice[margNum])-2) == "Normal"){seq(-5,5,length.out = length(xVals))
    } else if(substr(xChoice[margNum],0 , stringr::str_length(xChoice[margNum])-2) == "Poisson"){seq(0,10,length.out = length(xVals))
    } else {seq(0, 1, length.out = length(xVals))}


    yVals <- sapply(xAxis, tmpFun)
    if(!is.null(dim(yVals))){
      yVals <- (yVals %>%  t())[,1]
    }

    tmpDF <- tibble(xAxis = xAxis, yVals = yVals)
    if(length(unique(xAxis)) == 2){
      tmpDF <- tmpDF %>%
        filter(yVals <=1, yVals >= 0)
      ggplot2::ggplot(tmpDF,  aes(x = xAxis, y = yVals)) + geom_bar(stat = "identity") + theme_minimal() +
        labs( y = latex2exp::TeX(paste0("$", intrParamTex, "$")))  +
        ylim(0,1)+
        theme(text = element_text(family = "sans"),
              legend.position = "none",
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))
    } else{
      tmpDF <- tmpDF %>%
        filter(yVals <=funcRange[2], yVals >= funcRange[1])

      ggplot2::ggplot() +
        geom_line(data = tmpDF,mapping =  aes(x = xAxis, y = yVals)) +
        geom_rug(aes(x = xVals[,margNum+1]), inherit.aes = F, color = "steelblue", alpha = .2, size = 1) +
        theme_minimal()  +
        labs( y = latex2exp::TeX(paste0("$", intrParamTex, "$")))  +
        ylim(funcRange[1],funcRange[2]) +
        theme(text = element_text(family = "sans"),
              legend.position = "none",
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))
    }


  } else{
    ### code for parameter vs transformed parameter

    tmpFun <- function(a){
      transformFun(a, xVals, DGP)}
    xAxis <- seq(from = paramRange$from, to = paramRange$to, by = paramRange$by)

    yVals <- sapply(xAxis, tmpFun)
    if(!is.null(dim(yVals))){
      yVals <- (yVals %>%  t())[,1]
    }

    tmpDF <- tibble(xAxis = xAxis, yVals = yVals)
    tmpDF <- tmpDF %>%
      filter(yVals <=funcRange[2], yVals >= funcRange[1])

    ggplot2::ggplot(tmpDF, aes(x = xAxis, y = yVals)) + geom_line() + theme_minimal()  +
      labs(x= latex2exp::TeX(paste0("$", paramTex, "$")), y = latex2exp::TeX(paste0("$", intrParamTex, "$"))) +
      ylim(funcRange[1],funcRange[2]) +
      theme(text = element_text(family = "sans"),
            legend.position = "none",
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 16, margin = unit(c(4, 0, 0, 0), "mm")),
            axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))
  }
}



functionalFormWithCI <- function(transformFun, fixValuesX,
                                 paramTildes, funcRange, margNum, intrParamTex = "" ){
  # browser()
  xAxis <- seq(-5,5,.1)
  if(length(margNum) ==0){margNum <- 1}
  if(is.na(margNum)){margNum <- 1}

  # we get 1000 parameters
  nSims <- nrow(paramTildes)
  nXs <- length(xAxis)
  # for each x, turn that x into 1000 mus
  tmpFunA <- function(i,j){
    tmpParams <- paramTildes[i,]
    tmpX <- c(fixValuesX)
    tmpX[margNum+1] <- xAxis[j]
    transformFun(tmpParams, tmpX, DGP = F)
  }

  tmpFunB <- function(a){
    vec <- sapply(1:nSims, function(b){tmpFunA(b,a)})
    data.frame(row.names = F,
               mean = mean(vec),
               bottom = stats::quantile(vec, c(.1), na.rm = T),
               top = stats::quantile(vec, c(.9), na.rm = T))
  }

  plotVals <- cbind(xAxis, bind_rows(lapply(1:nXs, tmpFunB))) %>%
    rowwise() %>%  dplyr::mutate(bottom = max(bottom, funcRange[1])) %>%
    rowwise() %>%  dplyr::mutate(top = min(top, funcRange[2])) %>%
    filter(mean >= funcRange[1], mean <= funcRange[2])

  ggplot2::ggplot(plotVals, aes(x = xAxis, y = mean)) +
    geom_line(color = baseColor, size =1) +
    geom_ribbon(aes(ymin = bottom, ymax = top), color = baseColor2, alpha = .1, linetype = 0)   +
    theme_minimal() +
    labs( y = latex2exp::TeX(paste0("$", intrParamTex, "$"))) +
    ylim(funcRange[1],funcRange[2]) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_blank(),
          axis.title.y = element_text(
            size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5, color = baseColor))

}



functionalFormPlotOrdered <- function(transformFun, paramRange, paramTex = "", intrParamTex = "", fixValues = NULL,
                                      multi = F,margNum = NULL,  xVals = NULL, xChoice = NULL, funcRange = NULL, pdfFun = NULL,
                                      DGP = T){
  if(length(xChoice) ==0){return(element_blank())}
  if(any(is.na(fixValues))){return(element_blank())}
  if(length(margNum) ==0){margNum <- 1}
  if(is.na(margNum)){margNum <- 1}



  ### code for X vs transformed parameter
  tmpFun <- function(a){
    tmpParams <- fixValues
    tmpX <- colMeans(xVals)
    tmpX[margNum+1] <- a
    transfParams <- transformFun(tmpParams, tmpX, DGP)
    sapply(1:3, function(b){pdfFun(b, transfParams)
    })
  }
  xAxis <-if(substr(xChoice[margNum],0 , stringr::str_length(xChoice[margNum])-2) == "Normal"){seq(-5,5,.01)
  } else if(substr(xChoice[margNum],0 , stringr::str_length(xChoice[margNum])-2) == "Poisson"){seq(0,10,.01)
  } else {seq(0, 1, .1)}

  yVals <- sapply(xAxis, tmpFun) %>%  t()
  tmpDF <- data.frame(cbind(xAxis, yVals))
  colnames(tmpDF) <- c("xAxis", 1:ncol(yVals))
  tmpDFMelted <- tmpDF %>% reshape2::melt(id.vars = c("xAxis")) %>%
    filter(value >= funcRange[1], value <= funcRange[2])


  ggplot2::ggplot(tmpDFMelted, aes(x = xAxis, y = value, group = variable, color= variable)) +
    geom_line(size = 1.2) + theme_minimal()  +
    scale_color_manual(values = cbPalette) +
    labs( y = latex2exp::TeX(paste0("$\\pi$")))  +
    ylim(funcRange[1],funcRange[2]) +
    theme(text = element_text(family = "sans"),
          legend.position = "none",
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16, margin = unit(c(4, 4, 4, 4), "mm"), angle = 0, vjust = .5))

}
