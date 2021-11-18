# Point distance transect sampling using iterated integrated nested Laplace approximations

This repository contains the data and code to reproduce the results and figures in the Chapter 2 of my thesis "Expanding the use of spatial models in statistical ecology" (link to thesis will go here once it is available online to download).

I have attempted to make the code reproducible
by using the [`renv`](https://github.com/rstudio/renv/) package which can be installed by running

```R
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")
```

`renv` creates a local cache of all packages used for the analysis.
Details of these package versions and the `R` version used for the analysis can be read in the `renv/renv.lock` file. `renv` will install the required packages in a local repository and all R sessions within the repository will use these packages, leaving your other packages (and their versions) unchanged, so you can run this analysis without changing packages you use for other projects. 

Note that `renv` does not guarantee reproducibility as different machines will have different local operating system and compilers.

If you have `Rscript` installed you can run the analysis by running the following commands from the project root directory:

```
Rscript Analysis/fit_model.R
Rscript Analysis/eval_model.R
Rscript Analysis/eval_spde.R
Rscript Analysis/posterior_N.R
Rscript Analysis/excursions.R
```

This code will produce results very similar to the figures in the thesis.  However, there is a technical difference in the approximate inference technique used in this analysis. This does not seem to affect the results in this example.  `inlabru` has been updated in the time since this the thesis was written and the criteria for convergence of the iterated INLA approach in this repo uses the more recent version. 

See the iterated INLA vignette [available here](https://inlabru-org.github.io/inlabru/articles/method.html), which details the new approach and the footnote which contains the older method used for the thesis.
