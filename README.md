
<!-- README.md is generated from README.Rmd. Please edit that file -->

# liminal

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `liminal` is to provide diganostics and visual analytics for
understanding embedding algorithms such as tSNE. It has been
[shown](https://distill.pub/2016/misread-tsne/) that in order to produce
an effective embedding may one have to play with hyperparamters and
various settings for these algorithms.

`liminal` aims to increase the effective use of these algorithms by
combining them with interactive and dynamic graphics (via the vegawidget
and shiny packages) and with a technique from multivariate stastics
called the tour (via the tourr package). Briefly, a tour is a sequence
of interploated projections of multivariate data onto lower dimensional
space. The sequence is displayed as a dynamic visualisation, and enables
us to see the shadows of the high dimensional data makes in a lower
dimensional view. By combining the tour with embedding algorithms, we
can see the following:

1.  whether distances in the embedding view are meaningful
2.  the local and global structure of the data
3.  identify ‘interesting’ shapes or points in the data

## Quick start

Let’s take a look at a high-dimensional tree structured data set:

``` r
library(liminal)
dim(fake_trees)
#> [1] 3000  101
```

This data set has 3000 observations and 100 numeric variables, and has
ten branches. We can generate principal components:

``` r
pcs  <- prcomp(fake_trees[, -ncol(fake_trees)])
# var explained
head(cumsum(pcs$sdev / sum(pcs$sdev)))
#> [1] 0.05569726 0.09768259 0.13586130 0.17200716 0.20405094 0.23398570
```

And visualise the results as interactive scatter plot by augmenting the
orginal data

``` r
fake_trees <- dplyr::bind_cols(fake_trees, as.data.frame(pcs$x))
limn_xy(fake_trees, x = PC1, y = PC2, color = branches)
```

![](man/figures/README-pca-xy-1.png)<!-- -->

We see some separation of the branches in first two principal
components, however, we can’t see each of the branches clearly or their
relation to each other. We can tourr them to get an idea of underlying
structure:

``` r
# this loads a shiny app
limn_tour(fake_trees, PC1:PC15, color = branches)
```

The blue branch is hidden and connects all other branches.

We can also compare this to t-SNE embedding:

``` r
set.seed(2099)
tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))

tsne_df <- data.frame(tsneX = tsne$Y[,1],
                      tsneY = tsne$Y[,2],
                      branches = fake_trees$branches)
limn_xy(tsne_df, x = tsneX, y = tsneY, color = branches)
```

![](man/figures/README-tsne-xy-1.png)<!-- -->

The topology is a little messed up. We can see where our embedding is
different via a linked scatter plot:

``` r
limn_tour_xylink(dplyr::select(fake_trees, PC1:PC10, branches),
                 tsne_df,
                 x_color = branches, 
                 y_color = branches)
```
