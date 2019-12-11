
brush_selection <- function(name = "brush",
                  mapping = "color",
                  aggregate = NULL,
                  aggregate_mark = NULL ,
                  link = FALSE,
                  link_by = NULL,
                  link_transform = NULL,
                  persistent = FALSE) {
  val <- list(
    name = name,
    mapping = mapping,
    aggregate = aggregate,
    aggregate_mark = aggregate_mark,
    link = link,
    link_by = link_by,
    link_transform = link_transform,
    persistent = persistent
  )
  class(val) <- c("brush_selection", "list")
}

is_brush_selection <- function(x) inherits(x, "brush_selection")

set_brush_condition <- function(spec, brush) {
  stopifnot(is_brush_selection(brush))
  field <- brush[["mapping"]]
  spec[["encoding"]][[field]][["condition"]] <-
    list(selection = brush[["name"]])
  spec
}

set_brush_layer <- function(spec, brush) {
  stopifnot(is_brush_selection(brush))
  # is there a layer on brush?
  aggregate <- brush[["aggregate"]]
  if (is.null(aggregate)) return(spec)

  transform <- list(list(filter = list(selection = brush[["name"]])))
  aggregate <- list(aggregate = aggregate)
  encoding <- list(x = c(spec[["encoding"]][["x"]],
                         aggregate,
                         list(axis = list(title = spec[["encoding"]][["x"]][["field"]]))),
                   y = c(spec[["encoding"]][["y"]], aggregate,
                         list(axis = list(title = spec[["encoding"]][["y"]][["field"]]))))
  brush_layer <- list(transform = transform,
                      mark = brush[["aggregate"]],
                      encoding = encoding)

  layer <- list(list(layer = list(
    spec[!(names(spec) %in% c("data", "$schema"))],
    brush_layer
  )))

  list(`$schema` = spec[["$schema"]],
       data = spec[["data"]],
       layer = layer)

}
