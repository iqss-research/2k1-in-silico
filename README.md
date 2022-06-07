[Our logo, which reads 2k1 in Silico](https://projects.iq.harvard.edu/files/2k1-in-silico/files/2k1silicologo_v2_0.png)


by <a data-url="/2k1-in-silico/garyking.org" href="/2k1-in-silico/garyking.org" title="">Gary King</a> 
and <a data-url="/2k1-in-silico/zagrebmukerjee.com" href="/2k1-in-silico/zagrebmukerjee.com" title="">Zagreb Mukerjee</a>


## About

<p>Most of us who teach social science statistics work to get students past the static picture of probability 
and statistics portrayed in textbooks by simultaneously teaching them a statistical programming language like R. 
Programming enables students to make changes and immediately see the consequences, but learning technical details 
can be a big time sink that delays conceptual understanding of probability and statistics. <strong>2K1 in Silico 
</strong>separates concepts from details by giving students dynamic control of the math, probability, statistics, 
and empirical results, before, separately from, or instead of learning R.</p>

<p>The <strong>2K1 in Silico </strong>app was developed as a companion to "Gov 2001, Introductory 
Quantitative Social Science Methods, I", the introductory Graduate Methods class in the Political 
Science Ph.D. program&nbsp;at Harvard University. Gov 2001&nbsp;is open to all (even those not at Harvard) 
for credit, via the Harvard Extension School as Stat E-200. All the lectures and class materials are also 
available for other instructors to use in their courses. See <a data-url="https://projects.iq.harvard.edu/gov2001" 
href="https://projects.iq.harvard.edu/gov2001" title="">the course website</a> for more information.
</p>

<p>This app is also built to be easily extensible, allowing anyone to adapt or augment its functionality 
with only basic knowledge of R.&nbsp;<strong>2K1 in Silico</strong> was developed  
at Harvard's <a href="https://iq.harvard.edu" title="">
Institute for Quantitiative Social Science</a>. Comments, bug reports, and suggestions are welcome: 
please leave them on our <a data-url="https://github.com/iqss-research/2k1-in-silico/issues" 
href="https://github.com/iqss-research/2k1-in-silico/issues" title="">Github issues page</a>.

## Running Gov 2k1 in Silico

There are three ways to run the app:
1. Zero startup costs: just click <a data-url="in-silico.herokuapp.com/" href="in-silico.herokuapp.com/" title="">here</a>
2. Run on your computer:
	- <a href = "https://www.r-project.org/">Install R</a> (optionally <a href = "https://www.rstudio.com/products/rstudio/download/">with RStudio</a>) on your computer
	- Install the shiny library: `install.packages("shiny")`.
	- Enter: `shiny::runGitHub("2k1-in-silico", "iqss-research")`.
3. Install the app locally as an R package: 
	- <a href = "https://www.r-project.org/">Install R</a> (optionally <a href = "https://www.rstudio.com/products/rstudio/download/">with RStudio</a>) on your computer
	- Get the R library `devtools` with `install.packages("devtools")`.
	- Install the package locally with `devtools::install_github(“iqss-research/2k1-in-silico”)`, downloading dependencies as needed
	- Load the package with `library("Gov2k1inSilico")`.
	- Run the app with `run_app()`.