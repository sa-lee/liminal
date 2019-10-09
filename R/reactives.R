display_xy <-
function (center = TRUE, axes = "center", half_range = NULL,
          col = "black", pch = 20, edges = NULL, ...)
{
  labels <- NULL
  init <- function(data) {
    half_range <<- compute_half_range(half_range, data, center)
    labels <<- abbreviate(colnames(data), 3)
  }
  if (!is.null(edges)) {
    if (!is.matrix(edges) && ncol(edges) == 2) {
      stop("Edges matrix needs two columns, from and to, only.")
    }
  }
  render_frame <- function() {
    par(pty = "s", mar = rep(0.1, 4))
    blank_plot(xlim = c(-1, 1), ylim = c(-1, 1))
  }
  render_transition <- function() {
    rect(-1, -1, 1, 1, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {
    draw_tour_axes(proj, labels, limits = 1, axes)
    x <- data %*% proj
    if (center)
      x <- center(x)
    x <- x/half_range
    points(x, col = col, pch = pch)
    if (!is.null(edges)) {
      segments(x[edges[, 1], 1], x[edges[, 1], 2], x[edges[,
                                                           2], 1], x[edges[, 2], 2])
    }
  }
  list(init = init, render_frame = render_frame, render_transition = render_transition,
       render_data = render_data, render_target = nul)
}
