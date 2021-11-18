# Point distance transect sampling using iterated integrated nested Laplace approximations

This repository contains the data and code to reproduce the results and figures in the Chapter 2 of my thesis "Expanding the use of spatial models in statistical ecology" (link to thesis will go here once it is available online to download).

In order to run the analysis you need the following packages installed:

remotes::install_github("inlabru-org/inlabru", ref="stable")

INLA testing version

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

Using the renv package:

if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv stuff here.  Maybe this means I can get rid of the above package list stuff? including inlabru and INLA testing version?

If you have Rscript installed you can run the analysis by running the following commands:

list correct order to run the scripts

```
fit_model.R
```

This code will produce results very similar to the figures in the thesis.  However, there is a technical difference in the approximate inference technique used here.  Since the research was initially completed inlabru has been updated and the criteria for convergence of the iteratd INLA approach is different.  

Add details here.
