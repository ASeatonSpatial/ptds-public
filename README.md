# Point distance transect sampling using iterated integrated nested Laplace approximations

This repository contains the data and code to reproduce the results and figures in the Chapter 2 of my thesis "Expanding the use of spatial models in statistical ecology" (link to thesis will go here once it is available online to download).

This repository has attempted to make the code reproducible
by using the [`renv`](https://github.com/rstudio/renv/) package which can be installed by running

```R
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")
```
`renv` can create a local cache of all package versions used for the analysis.

If you have `Rscript` installed you can run the analysis by running the following commands from the project root directory:

```
Rscript Analysis/fit_model.R
Rscript Analysis/eval_model.R
Rscript Analysis/eval_spde.R
Rscript Analysis/posterior_N.R
Rscript Analysis/excursions.R
```

This code will produce results very similar to the figures in the thesis.  However, there is a technical difference in the approximate inference technique used here which does not seem to affect the results in this example.  In the time since this the thesis was written `inlabru` has been updated and the criteria for convergence of the iterated INLA approach is different. 

See the iterated INLA vignette [available here](https://inlabru-org.github.io/inlabru/articles/method.html), in particular the footnote which contains the method used for the thesis and the new criteria.
