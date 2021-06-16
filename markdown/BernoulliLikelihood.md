<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?...">
</script>

Likelihood given (*y* = (*y*<sub>1</sub>, …, *y*<sub>*n*</sub>)) :  
$$ P(\\pi|y) = k(y) \\cdot \\prod\_{i = 1}^{n} \\pi^{y\_i}(1-\\pi)^{(1-y\_i)}$$

Log Likelihood:
$$ \\ln P(\\pi|y) \\, \\dot{=} \\,  \\sum\_{i=1}^{n} y\_i \\ln(\\pi) $$
$$ + \\sum\_{i=1}^{n} (1-y\_i) \\ln(1-\\pi)$$
