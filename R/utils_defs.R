#' colors
#' @description saved colors we like
iqOrangeStr <- "#BF5803"
iqBlueStr <- "#3E77BB"
iqGrayStr <- "#2f2f2f"
cbPalette <- c("#56B4E9", "#009E73","#E69F00","#0072B2", "#D55E00", "#CC79A7", rep("#999999", 5))
baseColor <- cbPalette[1]
baseColor2 <- cbPalette[2]
baseColor3 <- cbPalette[3]

pkgEnv <- new.env()

#' selected
#' @description global constants
selectedDist <- "Normal (X)"
selectedQOI <- "Predicted Values"
paramSliderWidth <- "225px"
xGenerationChoices <- c("Bernoulli A", "Bernoulli B", "Bernoulli C", "Uniform A",
                        "Uniform B", "Uniform C", "Normal A", "Normal B",
                        "Normal C", "Poisson A", "Poisson B", "Poisson C")

regMin <- -5
regMax <- 5
regStep <- .5
regStart <- 3




utils::globalVariables(
  c("drawVal", "xAxis", "LogLikelihood",
    "QuadraticApprox", "param", "Name", "V2", "x", "realY",
    "regLine", "distrList", "prob","value", "variable", "bottom", "top",
    "..count..", "oprobs", "grtFlag", "bin", "xMinVal", "xMaxVal", "tau", "probs",
    "..x..", "i"))
