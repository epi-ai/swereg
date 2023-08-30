# covidnor <a href="https://www.csids.no/covidnor/"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[covidnor](https://www.csids.no/covidnor/) cleans and merges the publicly available datasets on COVID-19 in Norway into analysis-ready datasets. 

The following data are provided in this package: 

* case numbers
* ICU/hospital admissions
* testing
* vaccinations


Read the introduction vignette [here](https://www.csids.no/covidnor/articles/covidnor.html) or run `help(package="covidnor")`.

## csverse

<a href="https://www.csids.no/packages.html"><img src="https://www.csids.no/packages/csverse.png" align="right" width="120" /></a>

The [csverse](https://www.csids.no/packages.html) is a set of R packages developed to help solve problems that frequently occur when performing disease surveillance.

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. 

Then write in:

```
options(
  repos = structure(c(
    CSVERSE = "https://www.csids.no/drat/",
    CRAN    = "https://cran.rstudio.com"
  ))
)
```

Save the file and restart R.

You can now install [csverse](https://www.csids.no/packages.html) packages from our [drat repository](https://www.csids.no/drat/).

```
install.packages("covidnor")
```

