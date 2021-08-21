
############################################################
# Notation page LaTeX
############################################################


notation1 <<- "\\(X\\) is an \\( n \\times k \\) matrix of explanatory variables"
notation2 <<- tags$ul(
  tags$li("\\(X_{i,j}\\) represents the \\(i\\)th observation of the \\(j\\)th variable "),
  tags$li("Greek Letters \\( \\pi, \\beta, \\lambda \\) are fixed, unknown vectors of parameters (sometimes of length 1)"),
  tags$li("The first column of \\(X\\) is 1 by convention"),
  tags$li("The rest of \\(X\\) is generated when the app starts, and then remains persistent "),
  tags$li("The means of the columns \\(X\\) are drawn from a normal distribution with \\( \\mu = 5, \\sigma = 2 \\)"),
  tags$li("The columns of \\(X\\) are drawn from a normal distribution with \\( \\sigma = 2 \\)"),
  )
notation3 <<- "Greek letters \\( \\pi, \\beta, \\lambda\\) are vectors of parameters (sometimes of length 1)"
notation4 <<- "\\( y \\) is the outcome  variable, with observations \\( y_1, \\ldots, y_n \\)"
notation5 <<- "\\( y \\) is drawn from various distributions with parameters based on the above"


############################################################
# simulation LaTeX
############################################################


simMathJax1 <<- 
  div(
    withMathJax("Estimation Uncertainty: \\begin{array} 
                \\, \\tilde{\\theta} \\sim \\mathcal{N}(\\hat{\\theta}, \\hat{V}\\hat{\\theta}) \\\\
                \\, \\{ \\tilde{\\beta}, \\tilde{\\sigma}^2\\} = \\tilde{\\theta}  \\\\
                \\end{array}")
  )

simMathJax2 <<- 
  div(
    withMathJax("Fundamental Uncertainty: \\begin{array} 
                \\, \\tilde{\\mu}_i = X_i \\tilde{\\beta} \\\\
                \\, \\tilde{y}  \\sim \\mathcal{N}(\\tilde{\\mu}_i, \\tilde{\\sigma}^2) \\\\
                \\end{array}")
  )

simMathJax3 <<- 
  div(
    withMathJax("Simulating Quantities of Interest (Examples): \\begin{array} 
                \\small \\text{Pr}(y > 0.5) = \\small \\text{Proportion}(\\tilde{y}_j > 0.5)\\\\
                \\, \\tilde{\\hat{\\mu}} = \\frac{1}{n} \\sum_{j=1}^n \\tilde{y}_j \\\\
                \\, \\text{SE}(\\hat{\\mu}) = \\sqrt{\\frac{1}{n} \\sum (\\tilde{y}_j - \\tilde{\\mu})^2 }\\\\
                \\end{array}")
  )


