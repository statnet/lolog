# Latent Order LOGistic (LOLOG) Graph Models

LOLOG is a general framework for generative statistical modeling of graph datasets.  


## Installation

Make sure you have the dependancies installed

```
install.packages(c("network","parallel","ggplot2","reshape2","Rcpp"))
```

For good measure, the suggested packages too

```
install.packages(c("testthat","inline","knitr","rmarkdown","ergm"))
```

To install the latest development version from github run the following (you'll need a sh shell and git):

```
git clone https://github.com/fellstat/lolog.git
sh lolog/mkdist
```

## Using the package

```
library(lolog)
data(ukFaculty)

# A dyad independent model
fitind <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC"))
summary(fitind)
```

For a more detailed desciption of what can be done with lolog, [see the package vignette](inst/doc/lolog-introduction.pdf)

## Development using Eclipse

This package is set up as an Eclipse project, and the C++ code can be compiled and run without reinstalling the package. To set up in your eclipse IDE, select import project -> General -> Existing Projects into Workspace and select the lolog directory.

This project was set up following the methods outlined in:

<http://blog.fellstat.com/?p=170>




