
<!-- README.md is generated from README.Rmd. Please edit that file -->

# liminal

<!-- badges: start -->

[![R build
status](https://github.com/sa-lee/liminal/workflows/R-CMD-check/badge.svg)](https://github.com/sa-lee/liminal/actions)
<!-- badges: end -->

The goal of **liminal** is to provide complementary visualisations for
use with understanding embedding algorithms such as tSNE. It has been
[shown](https://distill.pub/2016/misread-tsne/) that in order to produce
an ‘effective’ embedding one may have to play with hyperparamters and
various settings for these algorithms.

See the [liminal
vignette](https://sa-lee.github.io/liminal/articles/liminal.html) for
details of package usage.

## Quick Start

The development version of **liminal** can be installed as follows:

``` r
# install.packages("remotes")
remotes::install_github("sa-lee/liminal")
```

## About

**liminal** aims to assist with the use of embedding algorithms by
combining them with interactive and dynamic graphics (via the
**vegawidget** and **shiny** packages) and with a technique from
multivariate statistics called the tour (via the **tourr** package).
Briefly, a tour is a sequence of interpolated projections of
multivariate data onto lower dimensional space. The sequence is
displayed as a dynamic visualisation, and enables us to see the shadows
the high dimensional data makes in a lower dimensional view. By
combining the tour with embedding algorithms, we can see the following:

1.  whether distances in the embedding view are meaningful
2.  the local and global structure of the data
3.  identify ‘interesting’ shapes or points in the data

The **liminal** package easily incorporates into a exploratory data
analysis workflow, and tracks user events such as brushes for downstream
analysis.
