

############################################################
# independent variables. generated once for each run
############################################################

# menu of X choices

allXNone <- matrix(0, 200, 10)
`allXConstant (1)` <- matrix(1, 200, 10)

`allXBernoulli(.5)` <- matrix(rbinom(n = 2000, size = 1, prob = .5), 200, 10)
`allXUniform(0,1)` <- matrix(runif(n = 2000, min = 0, max =1), 200, 10)
`allXNormal(0,1)` <- matrix(rnorm(n = 2000, mean = 0, sd = 1), 200, 10)

# returns first nRow rows and nCol cols
# where nRow shd be n and nCol shd be k
# first col always 1
xValGenerator <- function(nRow, type=c("Constant (1)")){
  
  # TODO: make extensible to more than 2 cases
  if(!any(is.null(type))){
    cbind(
      `allXConstant (1)`[1:nRow,1],
      if(length(unique(type)) == 1){
        eval(parse(text = paste0("`allX",type[1],"`[1:nRow, 1:length(type)]")))
      } else {
        lapply(
          type,
          function(t){
            eval(parse(text = paste0("`allX",t,"`[1:nRow, 1]")))}) %>% 
          unlist() %>% matrix(nRow, length(type)) 
      })
  } else {`allXConstant (1)`[1:nRow,1]}
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
  } else (tags$p())
  
  
}

# xUIElement <- function(type, vals, col){
#   
#   if(all(is.na(vals[,col])) || all(vals[,col]==0)){
#     return(div())
#   } else{
#     texTmp <- xDistrLatex(type, col)
#     numsTmp <- paste(lapply(vals[,col], function(a){round(a, 2)}), collapse = " ")
#     
#     return(div(hr(), 
#         fluidRow(
#           column(4, texTmp),
#           column(6, numsTmp),
#         )))
#   }
# }



xChoiceDivFun <- function(
  vals = matrix(rep(NA, 60), 20,3),
  nObs = 20, 
  choice1 = "Bernoulli(.5)",
  choice2 = "Uniform(0,1)"){  
  div(column(12, 
             tags$p(withMathJax("\\(X_1\\)")),
             fluidRow(
               div(selectInput(
                 inputId = "xChoice1",
                 label = NULL,
                 choices = xGenerationChoices,
                 selected = choice1,
                 width = "150px"), style = "float:left;"),
               div(tags$small(paste0(
                 paste(lapply(vals[1:5,2], function(a){round(a, 2)}), collapse = ", "),
                 " ... (n =", nObs,")"), style = "overflow-wrap: break-word; hyphens: auto;"),
                 style = "
                        float:left;
                        width: 150px;
                        padding-left:15px;
                        word-wrap: break-word;"
               )
             ),
             tags$p(withMathJax("\\(X_2\\)")),
             fluidRow(
               div(selectInput(
                 inputId = "xChoice2",
                 label = NULL,
                 choices = xGenerationChoices,
                 selected = choice2,
                 width = "150px"), style = "float:left;"),
               div(tags$small(paste0(
                 paste(lapply(vals[1:5,3], function(a){round(a, 2)}), collapse = ", "),
                 " ... (n =", nObs,")"), style = "overflow-wrap: break-word; hyphens: auto;"),
                 style = "
                        float:left;
                        width: 150px;
                        padding-left:15px;
                        word-wrap: break-word;"
               )
             ),
  ))}



# xChoiceDivMaker <- function(vals = matrix(1:9,3,3)){
#   
#   div(tags$p(tags$b("Observation"), style = "font-size:12px;"),
#       fluidRow(
#         column(width = 5,
#                selectInput(
#                  inputId = "xChoice1",
#                  label = NULL,
#                  choices = xGenerationChoices,
#                  selected = "Binary",
#                  width = "110px")),
#         column(width = 5, paste(lapply(vals[1:10,1], function(a){round(a, 2)}), collapse = " "))
#       ),
#       fluidRow(column(width = 5,
#                       selectInput(
#                         inputId = "xChoice2",
#                         label = NULL,
#                         choices = xGenerationChoices,
#                         selected = "Uniform",
#                         width = "110px"))
#       )
#       
#   )
#   
# }
