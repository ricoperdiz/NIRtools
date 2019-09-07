<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# NIRtools
**************

# Overview

NIRTools was specifically designed to help plant taxonomist and ecologists in documenting and building near infrared (NIR) spectroscopy subsets from a dataset based on parameter files that will serve as a metadata of each subset.

# What is NIR?

Read this [paper](http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0103-50532003000200006).

# Installation
**************

You can install the development version from GitHub with the package `devtools`

```
install.packages("devtools")
library(devtools)
devtools::install_github("ricoperdiz/NIRtools", build_vignettes = TRUE)
```

After installing NIRtools, type the command below to see the vignette with NIRtools tutorial.

```
vignette("NIRTools_tutorial", package = "NIRtools")
```
