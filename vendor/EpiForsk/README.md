# EpiForsk <img src="man/figures/Logo_EpiForsk.png" align="right" height="300" />

<!-- badges: start -->
[![CRANstatus](https://www.r-pkg.org/badges/version/EpiForsk)](https://cran.r-project.org/package=EpiForsk)
[![R-CMD-check](https://github.com/Laksafoss/EpiForsk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Laksafoss/EpiForsk/actions/workflows/R-CMD-check.yaml)
![CRAN Downloads overall](https://cranlogs.r-pkg.org/badges/grand-total/EpiForsk)
<!-- badges: end -->

This package is a framework for sharing guides, examples, and functions here at 
EpiForsk. It is primarily managed by ADLS and KIJA, but is intended to be a 
collaborative effort and we encourage sharing your hard earned code snippets.  

## Availability
The package is available on [CRAN](https://CRAN.R-project.org/package=EpiForsk).

## Installation & Use
### CRAN version
To install the package write
```{r}
install.packages("EpiForsk")
```
And to load the package write
```{r}
library("EpiForsk")
```
### Latest version
The latest version of the package can be installed directly from github:
```
# install.packages("devtools")
devtools::install_github("Laksafoss/EpiForsk")
```

## What is already in the package
The package is (hopefully) constantly under development. To see all content
currently available in the package use 

```{r}
help(package = "EpiForsk")
```

## What is suited for the package
There is a strict ban on any and all individual level information! Your code and
examples should strive to be as general as possible to avoid project and person 
specific information. 

Highly specialized code is allowed in the package, but we strongly encourage
you to make it useful to your colleagues by striving to strip it of project
specific details, allowing for generally transferable ideas to shine through.

A high priority is to have the package hosted on CRAN, which imposes certain
limitations on it. One such is its size, which is currently limited by CRAN at
100 MB. As the popularity of this initiative (hopefully) grows, we may 
reconsider what will be allowed in the package. 

There are two main formats for contributing: 

### Vignettes
Vignettes are a loose format guide with both description text and examples. 
In vignettes we share examples of typical data management, analysis methods,
and other blog/article style walkthroughs.  

### Functions
Functions automate common tasks the frequently occur in our daily work. These 
will work as any other functions made available by other packages in R. However,
the goal is not to make the sleekest, fastest and most efficient versions of
these functionalists, but rather implement functionalists tailored to our 
specific needs. 

## How to contribute
We encourage you to write your contribution yourself. To get started, read the
"contributing" vignette. If this is out of scope, you are welcome to contact 
ADLS or KIJA and we talk about possible solutions.

## Requirements for contributions
The package MUST be self-sufficient. This means that any data you wish to use 
in your examples should either be simulated in the example (preferred) or made 
available as a dummy data set within the package (only if small).

In general we follow [Hadley's guide](https://r-pkgs.org/) for package writing,
and this book contains a plethora of good advice. 

### Functions
As the package must comply with the CRAN check rules all functions must have a 
documentation. Moreover, we require that this documentation is made via 
[Roxygen2](https://roxygen2.r-lib.org/index.html) and contains one or more 
examples. 

### Vignettes
So far we have no formal requirements for vignettes. 
