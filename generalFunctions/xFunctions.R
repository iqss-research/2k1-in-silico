

############################################################
# independent variables. generated once for each run
############################################################

# menu of X choices

allXNone <- matrix(0, 200, 10)
`allXConstant (1)` <- matrix(1, 200, 10)

`allXBernoulli(.5)` <- matrix(rbinom(n = 2000, size = 1, prob = .5), 200, 10)
`allXUniform(0,1)` <- matrix(runif(n = 2000, min = 0, max =1), 200, 10)
`allXNormal(0,1)` <- matrix(rnorm(n = 2000, mean = 0, sd = 1), 200, 10)
`allXPoisson(1)`<- matrix(rpois(n = 2000,lambda = 1), 200, 10)

# FOR NOW: only works for biv case
correlatedX <- function(nRow, rho = 0.75){
  mu1 <- 0; s1 <- 1
  mu2 <- 0; s2 <- 1
  
  mu <- c(mu1,mu2) # Mean
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) #VCov 
  
  biv <- mvrnorm(nRow, mu = c(mu1, mu2), Sigma = sigma )
  cbind(`allXConstant (1)`[1:nRow,1], biv)
}



# returns first nRow rows and nCol cols
# where nRow shd be n and nCol shd be k
# first col always 1
xValGenerator <- function(nRow, type=c("Bernoulli(0.5)")){
  
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


xChoiceDivFun <- function(
  vals = matrix(rep(NA, 60), 20,3),
  nObs = 20, 
  choice1 = "Bernoulli(.5)",
  choice2 = "Uniform(0,1)"){  
  div(column(12, 
             fluidRow(
               tags$p(withMathJax("\\(X_1\\)"), style = "float:left; padding-right:10px;"),
               div(selectInput(
                 inputId = "xChoice1",
                 label = NULL,
                 choices = xGenerationChoices,
                 selected = choice1,
                 width = "150px"), style = "float:left;"),
               div(tags$small(
                 paste0(
                   paste(lapply(vals[1:5,2], function(a){round(a, 2)}), collapse = ", "),
                   " ... "),
                 tags$p(paste0("(n =", nObs,")"), style = "color:#ff0000"),
                 style = "overflow-wrap: break-word; hyphens: auto;"),
                 style = "
                        float:left;
                        width: 150px;
                        padding-left:15px;
                        word-wrap: break-word;"
               )
             ),
             fluidRow(
               tags$p(withMathJax("\\(X_2\\)"), style = "float:left; padding-right:10px;"),
               div(selectInput(
                 inputId = "xChoice2",
                 label = NULL,
                 choices = xGenerationChoices,
                 selected = choice2,
                 width = "150px"), style = "float:left;"),
               div(tags$small(
                 paste0(
                   paste(lapply(vals[1:5,3], function(a){round(a, 2)}), collapse = ", "),
                   " ... "),
                 tags$p(paste0("(n =", nObs,")"), style = "color:#ff0000"),
                 style = "overflow-wrap: break-word; hyphens: auto;"),
                 style = "
                        float:left;
                        width: 150px;
                        padding-left:15px;
                        word-wrap: break-word;"
               )
             ),
  ))}



