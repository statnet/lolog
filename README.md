# Latent Order LOGistic (LOLOG) Graph Models

LOLOG is a general framework for generative statistical modeling of graph datasets.  


## Installation

Make sure you have the dependacies installed

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

## Run a model

```
data(ukFaculty)

# A dyad independent model
fitind <- lolog(ukFaculty ~ edges() + nodeMatch("GroupC") + nodeCov("GroupC"))
summary(fitind)
```