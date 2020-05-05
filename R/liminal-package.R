#' liminal: interactive high-dimensional data visualisation
#'
#' liminal is an R package for constructing interactive visualisations designed
#' for exploratory high-dimensional data analysis. It's main purpose is to
#' combine tours with (non-linear) dimension reduction algorithms to provide a
#' more holistic view of the geometry and topology of dataset. These
#' are designed for data analysts first, so they render either inside the
#' RStudio Viewer pane or from a web-browser.
#'
#' There are three main functions for generating visualisations:
#'
#'   * Simple interactive scatter plots with `limn_xy()` and `limn_xycol()`
#'   * Generating tours via `limn_tour()`
#'   * Linking tours to another view `limn_tour_link()`
#'
#' User interactions can be customised via brush compostions
#'   * ...
#'
#' For more details on the features of liminal, read the vignettes:
#' `browseVignettes(package = "liminal")`
#' @import vegawidget
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
