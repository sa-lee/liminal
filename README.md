
<!-- README.md is generated from README.Rmd. Please edit that file -->

# liminal

<!-- badges: start -->

[![R build
status](https://github.com/sa-lee/liminal/workflows/R-CMD-check/badge.svg)](https://github.com/sa-lee/liminal/actions)
<!-- badges: end -->

**liminal** is an R package for constructing interactive visualisations
designed for exploratory high-dimensional data analysis. It’s main
purpose is to combine tours with (non-linear) dimension reduction
algorithms to provide a more holistic view of the geometry and topology
of a dataset. These are designed for data analysts first, so they render
either inside the RStudio Viewer pane or from a web-browser using
**shiny**.

There are two main functions for generating tour interfaces:

-   The basic tour animation via `limn_tour()`
-   Linking tours to another view `limn_tour_link()`

The goal of **liminal** is to provide complementary visualisations for
use with understanding embedding algorithms such as tSNE. It has been
[shown](https://distill.pub/2016/misread-tsne/) that in order to produce
an ‘effective’ embedding one may have to play with hyperparamters and
various settings for these algorithms. **liminal** allows you to see how
different parameterisations warps the underlying high-dimensional space.

See the [liminal
vignette](https://sa-lee.github.io/liminal/articles/liminal.html) for
details of package usage.

## Quick Start

The development version of **liminal** can be installed as follows:

``` r
# install.packages("remotes")
remotes::install_github("sa-lee/liminal")
```

You can generate a tour view that will load in the Rstudio Viewer pane:

``` r
library(liminal)
limn_tour(fake_trees, dim1:dim10)
```

The interface provides instructions on how to use it, click on the help
button to get started!
