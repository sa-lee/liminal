#' Parton distribution function sensitivity experiments
#'
#' @description Data from Wang et al., 2018 to compare embedding approaches to a
#' tour path.
#'
#' @details Data were obtained from CT14HERA2 parton distribution function
#' fits as used in Laa et al., 2018. There are 28 directions in the parameter
#' space of parton distribution function fit, each point in the variables
#' labelled X1-X56 indicate moving +- 1 standard devation from the 'best'
#' (maximum likelihood estimate) fit of the function. Each observation has
#' all predictions of the corresponding measurement from an experiment.
#'
#' (see table 3 in that paper for more explicit details).
#'
#'  The remaining columns are:
#'
#' * InFit: A flag indicating whether an observation entered the fit of
#'   CT14HERA2 parton distribution function
#' * Type: First number of ID
#' * ID: contains the identifier of experiment, 1XX/2XX/5XX correpsonds
#' to Deep Inelastic Scattering (DIS) / Vector Boson Production (VBP) /
#'  Strong Interaction (JET). Every ID points to an experimental paper.
#' * pt: the per experiment observational id
#' * x,mu: the kinematics of a parton. x is the parton momentum fraction, and
#' mu is the factorisation scale.
#'
#' @references
#' Wang, B.-T., Hobbs, T. J., Doyle, S., Gao, J., Hou, T.-J., Nadolsky, P. M.,
#' & Olness, F. I. (2018). PDFSense: Mapping the sensitivity of
#' hadronic experiments to nucleon structure.
#' Retrieved from [https://arxiv.org/abs/1808.07470](https://arxiv.org/abs/1808.07470)
#'
#' Cook, D., Laa, U., & Valencia, G. (2018).
#' Dynamical projections for the visualization of PDFSense data.
#' The European Physical Journal C, 78(9), 742.
#' \doi{10.1140/epjc/s10052-018-6205-2}
#'
#'
#' @source [http://www.physics.smu.edu/botingw/PDFsense_web_histlogy/](http://www.physics.smu.edu/botingw/PDFsense_web_histlogy/)
"pdfsense"

#' A high-dimensional tree data structure with 10 branching points.
#'
#' @details Data are obtained from diffusion limited aggregation
#' tree simulation in the `phate` python and `phateR` packages, but
#' reconstructed as a wide data.frame rather than a list.
#'
#' There are 3000 rows and 101 columns, the first 100 columns are labelled
#' dim1 - dim100, and are numeric, while the final column is a
#' factor representing the branch id.
#'
#' @source [PHATE](https://github.com/KrishnaswamyLab/PHATE/blob/master/Python/phate/tree.py)
"fake_trees"


#' liminal color palettes
#'
#' @details Vectors of colors based on the schemes available in Vega-Lite.
#' Their main purpose is so you can use these palettes in `ggplot2` graphics,
#' so that graphs align with the [limn_tour()] functions.
#'
#' @return A character vector of hex color codes of length 10 or 20.
#'
#' @seealso https://vega.github.io/vega/docs/schemes/
#'
#' @rdname palettes
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(fake_trees, aes(x = dim1, y = dim2, color = branches)) +
#'     geom_point() +
#'     scale_color_manual(values = limn_pal_tableau10())
#'
#'   ggplot(fake_trees, aes(x = dim1, y = dim2, color = branches)) +
#'     geom_point() +
#'     scale_color_manual(values = limn_pal_tableau20())
#' }
#' @export
limn_pal_tableau10 <- function() {
  c(
    "#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#eeca3b",
    "#b279a2", "#ff9da6", "#9d755d", "#bab0ac"
  )
}

#' @rdname palettes
#' @export
limn_pal_tableau20 <- function() {
  c(
    "#4c78a8", "#9ecae9", "#f58518", "#ffbf79", "#54a24b", "#88d27a",
    "#b79a20", "#f2cf5b", "#439894", "#83bcb6", "#e45756", "#ff9d98",
    "#79706e", "#bab0ac", "#d67195", "#fcbfd2", "#b279a2", "#d6a5c9",
    "#9e765f", "#d8b5a5"
  )
}
