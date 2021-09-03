
############################################################
# Notation page LaTeX
############################################################

## TODO: reorganize for legibility. Create subheads

basicNotation1 <<- "\\(X\\) is an \\( n \\times k \\) matrix of explanatory variables"
basicNotation2 <<- tags$ul(
  tags$li("\\(X_{i,j}\\) represents the \\(i\\)th observation of the \\(j\\)th variable "),
  tags$li("Greek Letters \\( \\pi, \\beta, \\lambda \\) are fixed vectors of parameters (sometimes of length 1)."),
  tags$li("The first column of \\(X\\) is 1 by convention. The other columns you can choose"),
  tags$li("\\(X\\) is generated when the app starts, and then remains persistent."),
  tags$li("The means of the columns \\(X\\) are drawn from a normal distribution with \\( \\mu = 5, \\sigma = 2 \\)."),
  tags$li("The columns of \\(X\\) are drawn from a normal distribution with \\( \\sigma = 2 \\)"),
  )
basicNotation3 <<- "Greek letters \\( \\pi, \\beta, \\lambda\\) are vectors of parameters (sometimes of length 1)"
basicNotation4 <<- "\\( y \\) is the outcome  variable, with observations \\( y_1, \\ldots, y_n \\)"
basicNotation5 <<- "\\( y \\) is drawn from various distributions with parameters based on the above"
MLENotation1 <<- "A profile likelihood for one parameter is a likelihood function with all other parameters fixed at their MLE."
MLENotation2 <<- tags$p("The maximum likelihood estimation is derived using numerical optimization (the ",tags$i('optim')," function). The quadratic approximation is a Taylor approximation of the log-likelihood around the MLE, using the numerically determined Hessian. For details on this calculation, see the slides.")


SimNotation1 <<- tags$p("The expected value of \\(y_c\\) is computed by drawing 1000 values of \\(\\tilde{\\theta}\\). Then for each  \\(\\tilde{\\theta}\\) draw 1000  \\(\\tilde{y}_c\\). Then get the average for each  \\(\\tilde{\\theta}\\), and plot the averages" )