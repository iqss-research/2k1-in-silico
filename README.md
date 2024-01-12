![Our logo, which reads 2k1 in Silico](./logos/2k1silicologo_v3b.png)

by <a href="https://natalie-ayers.github.io/home/" title="">Natalie Ayers</a>, <a href="garyking.org" title="">Gary King</a>, <a href="https://politicalscience.yale.edu/people/zagreb-mukerjee" title="">Zagreb Mukerjee</a>, and <a href="https://dskinnion.github.io/" title="">Dominic Skinnion</a>

## What is this?

``2k1-in-Silico: An Interactive Methods Non-Textbook'' teaches statistics -- the big picture and the details -- without having to simultaneously learn or use a statistical programming language. We focus on three concepts, each corresponding to its own tab (at the top of the screen):

* **Data Generation Processes:** using probability models (models for how data can be produced);

* **Inference:** using likelihood models (almost the inverse of probability, where we try to infer the data generation process from the data);

* **Quantities of Interest:** using statistical simulation, enabling you (rather than some computer program) to control how statistical results are presented, so you can understand your results and impactfully present them.

## Why use it?

Learning while doing -- controlling inputs and watching how outputs change -- tends to be more helpful than static textbooks.  However, programming lessons can interrupt learning (for the same reason as you probably don't listen to calculus lectures during swimming lessons, even if you need to learn both skills). We also provide extensive and automated in-context assistance if, when, and where you need it (simply click on the little tooltips marked **i** whenever needed).

## How do you use it?

There are two ways to run the app: 
1. Click <a href = "https://2k1.iq.harvard.edu">here</a> and follow the bouncing arrow.
2. <details><summary>Install 2K1-in-Silico on your computer as an R package:</summary>
   <ul>
      <li><a href = "https://www.r-project.org/">Install R</a> (optionally <a href = "https://www.rstudio.com/products/rstudio/download/">with RStudio</a>) on your computer.</li>
      <li>Open a new R session, with no packages loaded.</li>
      <li>Make sure your R is at version 4.0.0 or later.</li>
      <li>Enter: <code>options(pkgType="binary")</code>.</li>
      <li>Install the <code>devtools</code> library: <code>install.packages("devtools")</code>. </li>
      <li>Install the package locally with <code>devtools::install_github(“iqss-research/2k1-in-silico”, upgrade = T, quiet = T)</code>, downloading dependencies as needed </li>
      <li>Load the package with <code>library("Gov2k1inSilico")</code>. </li>
      <li>Run the app with <code>runGov2k1()</code>.</li>
   </ul>
</details>

## Where can you find more information?

Please see our paper “<a target="_blank" href="https://garyking.org/2k1">Statistical Intuition Without Coding (or Teachers)</a>”. The paper and our app parallels some of the core, model-based content of <a target="_blank" href="https://projects.iq.harvard.edu/gov2001/">Gov 2001</a>, a key course in the Harvard Government Department's social science methods sequence (taught for many years by <a target="_blank" href="http://garyking.org">Gary King</a>). All the lectures, videos, and many other teaching materials, including this app, are available for other instructors and students to use in their courses as well from the course website, <a target="_blank" href="https://projects.iq.harvard.edu/gov2001/">j.mp/G2001</a>, many parts of which are linked to in the tooltips in the app. (Thanks to generations of Gov2001 students and teaching fellows for helping us improve the ideas reflected here.) <a target="_blank" href="https://youtu.be/qs2uCuDL2OQ?t=2416">This lecture video</a> gives an overview of the course.

To learn more about 2K1-in-Silico, to send comments or suggestions, or to contribute to this open source package, see the <a target="_blank"  href = "https://iqss-research.github.io/2k1-in-silico/">app's website</a> and <a target="_blank" href="https://github.com/iqss-research/2k1-in-silico">GitHub repository</a>.
  
## Documentation

<details><summary>Math Documentation:</summary>
<div style="text-align: center"><iframe src="https://docs.google.com/document/d/1tpIl1o8iZB6jQKyyOAEGQLIfs-wRY2lYGte7qJ9ylYE" width="100%" height="1000" ></iframe></div>
</details>

<details><summary>X Values:</summary>
<div style="text-align: center"><iframe src="https://docs.google.com/spreadsheets/d/1iLBqVaGuLxXyPF4LfuggeGfTZC2roSSaF-cnqSD7TEU" width="100%" height="1000" ></iframe></div>
</details>

