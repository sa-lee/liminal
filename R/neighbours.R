# Utilities for estimating  and searching kNN graph
# These functions are used internally to perform spatially resolved
# linked brushing. The default is to compute exact nearest neighbors
# using the FNN package. Otherwise, for largeish data and/or an alternate
# distance metric, the RcppAnnoy package is used to compute approximate nearest
# neighbors.

find_knn <- function(.data,
                     num_neighbors = 10,
                     metric = c("euclidean", "cosine", "manhattan"),
                     .include_self = TRUE) {

  nr <- nrow(.data)
  # smallish data use FNN
  if (nr <= 4096  && metric == "euclidean") {
    return(build_fnn(.data, num_neighbors, .include_self))
  }

  # otherwise use ANN
  build_ann(.data, num_neighbors, metric)

}


#' @importFrom FNN get.knn
build_fnn <- function(.data, num_neighbors, .include_self = TRUE) {

  if (.include_self) {
    num_neighbors <- num_neighbors - 1
  }

  fnn <- FNN::get.knn(.data, num_neighbors)
  idx <- fnn$nn.index
  dist <- fnn$nn.dist

  if (.include_self) {
    idx <- cbind(seq_len(nrow(.data)), idx)
    dist <- cbind(rep(0, nrow(.data)), dist)
  }

  list(idx = idx, dist = dist)

}

#' @importFrom methods new is
new_ann <- function(metric, nc) {
  switch(metric,
         "euclidean" = methods::new(RcppAnnoy::AnnoyEuclidean, nc),
         "cosine" = methods::new(RcppAnnoy::AnnoyAngular, nc),
         "manhattan" = methods::new(RcppAnnoy::AnnoyManhattan, nc),
         stop("Unknown metric:", metric)
  )
}

build_ann_index <- function(.data, metric = "euclidean", n_trees = 50) {
  nr <- nrow(.data)
  nc <- ncol(.data)
  ann <- new_ann(metric, nc)

  for (i in seq_len(nr)) {
    ann$addItem(i - 1, .data[i, ])
  }

  ann$build(n_trees)

  ann
}

search_ann_index <- function(.data,
                             num_neighbors,
                             ann,
                             search_num) {
  nr <- nrow(.data)
  idx <- matrix(0L, nrow = nr, ncol = num_neighbors)
  dist <- matrix(0, nrow = nr, ncol = num_neighbors)

  for (i in seq_len(nr)) {
    ans <- ann$getNNsByVectorList(.data[i,], num_neighbors, search_num, TRUE)
    stopifnot(length(ans$item) == num_neighbors)
    idx[i, ] <- ans$item
    dist[i, ] <- ans$distance

  }

  if (methods::is(ann, "Rcpp_AnnoyAngular")) {
    dist <- 0.5 * (dist*dist)
  }

  list(idx = idx + 1L, dist = dist)
}

build_ann <- function(.data,
                      num_neighbors,
                      metric,
                      n_trees = 50,
                      search_num = 2 * n_trees * num_neighbors) {

  ann <- build_ann_index(.data, metric, n_trees)

  search_ann_index(.data, num_neighbors, ann, search_num)

}



reference_data_knn <- function(ref) {

  if (is(ref, "data.frame")) {
    return(model.matrix(~ . - 1, na.fail(ref)))
  }

  if (is(ref, "dist")) {
    return(as.matrix(na.fail(ref)))
  }

  stopifnot(is(ref, "matrix"))

  na.fail(ref)
}


nest_by_neighbours <- function(tbl, idx) {
  if (is(idx, "matrix")) {
    tbl <- tbl[as.vector(idx), ]
    idx <- as.vector(row(idx))

  } else {
    stopifnot(length(idx) == nrow(tbl))
  }

  dplyr::group_nest(tbl, row_number = idx, .key = "neighbors")
}

