

############################################################
# independent variables. generated once for each run
############################################################

# menu of X choices
xGenerationChoices <- c("None", "Constant", "Binary", "Uniform", "Normal")

allXNone <- matrix(0, 200, 10)
allXConstant <- matrix(1, 200, 10)

allXBinary <- matrix(rbinom(n = 2000, size = 1, prob = .5), 200, 10)
allXUniform <- matrix(runif(n = 2000, min = 0, max =1), 200, 10)
allXNormal <- matrix(rnorm(n = 2000, mean = 0, sd = 1), 200, 10)

# returns first nRow rows and nCol cols
# where nRow shd be n and nCol shd be k
# first col always 1
xValGenerator <- function(nRow, type=c("Constant")){
  
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      allXConstant[1:nRow,1],
      if(length(unique(type)) == 1){
        eval(parse(text = paste0("allX",type[1],"[1:nRow, 1:length(type)]")))
      } else {
        lapply(
          type,
          function(t){
            eval(parse(text = paste0("allX",t,"[1:nRow, 1]")))}) %>% 
          unlist() %>% matrix(nRow, length(type)) 
      })
  } else {allXConstant[1:nRow,1]}
}



xDistrLatex <- function(type, col){
  
  if(type == "None"){
    tags$p()
  } else if(type == "Constant"){
    tags$p(withMathJax("\\(X_{i,",col,"} = X = 1 \\)"))
  } else if(type == "Binary"){
    tags$p(withMathJax("\\(X_{i,",col,"} \\sim \\text{Bernoulli}(.5) \\)"))
  } else if(type == "Uniform"){
    tags$p(withMathJax("\\(X_{i,",col,"} \\sim \\text{Uniform}([0,1]) \\)"))
  } else if(type == "Normal"){
    tags$p(withMathJax("\\(X_{i,",col,"} \\sim \\mathcal{N}(0,1) \\)"))
  } else if(type == "Real Data"){
    tags$p()
  }
  
  
}

xUIElement <- function(type, vals, col){
  
  if(all(is.na(vals[,col])) || all(vals[,col]==0)){
    return(div())
  } else{
    texTmp <- xDistrLatex(type, col)
    numsTmp <- paste(lapply(vals[,col], function(a){round(a, 2)}), collapse = " ")
    
    return(div(hr(), 
        fluidRow(
          column(4, texTmp),
          column(6, numsTmp),
        )))
  }
}


xSummaryStats <- function(vals){
  
  if(is.null(vals)){return(div())}
  
  lst <- as.list(as.data.frame(vals))
  
  strs <- lapply(lst, function(a){round(mean(a, na.rm = F),2)}) %>%  unlist() %>%  as.numeric()
  
  tex1 <- if(strs[2] ==0){""} else {tags$small(withMathJax(paste0("\\(\\bar{X}_{i,1} = ", strs[2],"\\hspace{60px} \\) ")))}
  tex2 <- if(strs[3] ==0){""} else {tags$small(withMathJax(paste0("\\(\\bar{X}_{i,2} = ", strs[3],"\\)")))}
  
  div(tex1, tex2)
  
}

