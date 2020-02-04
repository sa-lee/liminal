#' Generate multiple t-SNE embeddings for a single data set
#'
#' @param .data
#' @param cols
#' @param .params A named list with valid parameters to the embedding function,
#' this will expand out all combinations of inputs
#' @param ... other paramters to pass to [Rtsne::Rtsne()]
limn_embed_tsne <- function(.data, cols, .params, ...) {
  norm_args_embed(.params, "Rtsne")

  cols <- rlang::enquo(cols)

  param_df <- dplyr::as_tibble(expand.grid(.params))
  inx <- seq_len(nrow(param_df))
  X <- init_tour_matrix(.data, cols, identity)

  coords <- lapply(inx,
                   function(i) {
                     args <- as.list(param_df[i,,drop = FALSE])
                     out <- do.call(Rtsne::Rtsne,
                                    c(list(X = X),
                                      args, ...))[["Y"]]
                     data.frame(x = out[,1],
                                y = out[,2],
                                rowid = seq_len(nrow(out)))
                   })

  param_df[["coords"]] <- coords

  class(param_df) <- c("embed_df", class(param_df))

  param_df

}

norm_args_embed <- function(.params, ns) {
  stopifnot(requireNamespace(ns, quietly = TRUE))
  stopifnot(is.list(.params))
  stopifnot(length(names(.params)) == length(.params))
}
