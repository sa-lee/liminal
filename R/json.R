schema_dir <- function() {
  system.file("inst", "extdata", "schemas", package = "liminal")
}

schema_scatter <- function() {
  json <- file.path(schema_dir(), "scatter.json")
  jsonlite::fromJSON(json)
}
