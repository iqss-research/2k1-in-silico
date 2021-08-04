

notation1 <- "\\(X\\) is an \\( n \\times k \\) matrix of independent variables"
notation2 <- tags$ul(
  tags$li("\\(X_{i,j}\\) represents the \\(i\\)th observation of the \\(j\\)th variable "),
  tags$li("The first column of \\(X\\) is 1 by convention"),
  tags$li("The rest of \\(X\\) is generated when the app starts, and then remains persistent "),
  tags$li("The means of the columns \\(X\\) are drawn from a normal distribution with \\( \\mu = 5, \\sigma = 2 \\)"),
  tags$li("The columns of \\(X\\) are drawn from a normal distribution with \\( \\sigma = 2 \\)"),
  )
notation3 <- "Greek letters \\( \\pi, \\beta, \\lambda\\) are vectors of parameters (sometimes of length 1)"
notation4 <- "\\( y \\) is the dependent variable, with observations \\( y_1, \\ldots, y_n \\)"
notation5 <- "\\( y \\) is drawn from various distributions with the parameters based on the above"

