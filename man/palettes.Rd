% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/liminal-data.R
\name{limn_pal_tableau10}
\alias{limn_pal_tableau10}
\alias{limn_pal_tableau20}
\title{liminal color palettes}
\usage{
limn_pal_tableau10()

limn_pal_tableau20()
}
\value{
A character vector of hex color codes of length 10 or 20.
}
\description{
liminal color palettes
}
\details{
Vectors of colors based on the schemes available in Vega-Lite.
Their main purpose is so you can use these palettes in \code{ggplot2} graphics,
so that graphs align with the \code{\link[=limn_tour]{limn_tour()}} functions.
}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  ggplot(fake_trees, aes(x = dim1, y = dim2, color = branches)) +
    geom_point() +
    scale_color_manual(values = limn_pal_tableau10())

  ggplot(fake_trees, aes(x = dim1, y = dim2, color = branches)) +
    geom_point() +
    scale_color_manual(values = limn_pal_tableau20())
}
}
\seealso{
https://vega.github.io/vega/docs/schemes/
}