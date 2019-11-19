schema_dir <- function() {
  system.file("inst", "extdata", "schemas", package = "liminal")
}

schema_scatter <- function() {
  json <- file.path(schema_dir(), "scatter.json")
  jsonlite::fromJSON(json)
}

schema_axes_tour <- function(name, half_range) {
  json <- file.path(schema_dir(), "biplot.json")
  ans <- jsonlite::fromJSON(json)
  ans[["data"]][["name"]] <- name
  ans[["encoding"]][["x"]][["scale"]][["domain"]] <- c(-half_range, half_range)
  ans[["encoding"]][["y"]][["scale"]][["domain"]] <- c(-half_range, half_range)
  ans
}
