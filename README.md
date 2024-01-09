![Our logo, which reads 2k1 in Silico](https://projects.iq.harvard.edu/files/2k1-in-silico/files/2k1silicologo_v2_0.png)

by <a href="garyking.org" title="">Gary King</a>, <a href="https://politicalscience.yale.edu/people/zagreb-mukerjee" title="">Zagreb Mukerjee</a>, <a href="https://natalie-ayers.github.io/home/" title="">Natalie Ayers</a>, and <a href="https://dskinnion.github.io/">Dominic Skinnion</a>

## What is 2k1-in-Silico?

2k1-in-Silico teaches statistics -- the big picture and the details -- without having to simultaneously learn or use a statistical programming language. We focus on three concepts, each corresponding to its own tab (at the top of the screen):

* **Data Generation Processes:** how an assumed statistical model can produce data, based on the probability model of uncertainty;

* **Likelihood:** the theory of statistical inference, which is almost the inverse of probability where we try to infer the data generation process from the observed data;

* **Simulation:** enabling you (rather than some computer program) to completely control how statistical results are computed and presented, so you can understand your results and impactfully present them.

## Why use it?

Learning while doing -- controlling inputs and watching how outputs change -- is more helpful than static textbooks, until now, programming lessons wind up interrupting learning (not much different than trying to take swimming lessons during calculus lectures). 2k1-in-Silico keeps learning interactive without requiring that you also find that bug on line 57. We also provide extensive and automated in-context assistance if, when, and where you need it (simply click on the little tooltips marked **i** whenever needed).

## Where can you find more information?

Please see our paper “<a target="_blank" href="https://garyking.org/2k1">Statistical Intuition Without Coding (or Teachers)</a>”. The paper and our app parallels some of the core, model-based content of <a target="_blank" href="https://projects.iq.harvard.edu/gov2001/">Gov 2001</a>, the first course in the Harvard Government Department's social science methods sequence (taught for many years by <a target="_blank" href="http://garyking.org">Gary King</a>). All the lectures, videos, and many other teaching materials, including this app, are available for other instructors and students to use in their courses as well from the course website, <a target="_blank" href="https://projects.iq.harvard.edu/gov2001/">j.mp/G2001</a>, many parts of which are linked to in the tooltips in the app. (Thanks to generations of Gov2001 students for helping us improve the ideas behind this app.) <a target="_blank" href="https://youtu.be/qs2uCuDL2OQ?t=2416">This lecture video</a> gives an overview of the course.

To learn more about 2K1-in-Silico, to send comments or suggestions, or to contribute to this open source package, see the <a target="_blank"  href = "https://projects.iq.harvard.edu/2k1-in-silico/home">app's website</a>.

## Running 2k1-in-Silico

There are two ways to run the app: 
1. Use the app online: 
   - <a href = "https://2k1.iq.harvard.edu">2k1-in-Silico</a> 
   - Follow the bouncing arrow! 
2. Install the app locally as an R package: 
   - <a href = "https://www.r-project.org/">Install R</a> (optionally <a href = "https://www.rstudio.com/products/rstudio/download/">with RStudio</a>) on your computer.
   - Open a new R session, with no packages loaded.
   - Make sure your R is at version 4.0.0 or later.
   - Enter: `options(pkgType="binary")`.
   - Install the `devtools` library: `install.packages("devtools")`. 
   - Install the package locally with `devtools::install_github(“iqss-research/2k1-in-silico”, upgrade = T, quiet = T)`, downloading dependencies as needed 
   - Load the package with `library("Gov2k1inSilico")`. 
   - Run the app with `runGov2k1()`.
  
## Documentation

<div style="text-align: center"><iframe src="https://docs.google.com/document/d/1tpIl1o8iZB6jQKyyOAEGQLIfs-wRY2lYGte7qJ9ylYE" width="100%" height="1000px" ></iframe></div>
