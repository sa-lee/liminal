#' liminal: interactive high-dimensional data visualisation
#'
#' liminal is an R package for constructing interactive visualisations designed
#' for exploratory high-dimensional data analysis. It's main purpose is to
#' combine tours with (non-linear) dimension reduction algorithms to provide a
#' more holistic view of the geometry and topology of a dataset. These
#' are designed for data analysts first, so they render either inside the
#' RStudio Viewer pane or from a web-browser using `shiny`.
#
#' There are two main functions for generating tour interfaces:
#'   * The basic tour animation via [limn_tour()]
#'   * Linking tours to another view [limn_tour_link()]
#'
#' For more details on the features and usage of liminal, see the vignettes:
#' `browseVignettes(package = "liminal")`
#' @import vegawidget
#' @import shiny
#' @import miniUI
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
