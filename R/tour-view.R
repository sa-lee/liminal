# Internal functions for generating the tour spec

generate_tour_frame <- function(tour_data, start, half_range, color_tbl, morph) {
  # intialise frame
  source_values <- tour_data %*% start
  source_values <- morph(source_values, half_range)
  colnames(source_values) <- c("x", "y")
  source_values <- as.data.frame(source_values)
  dplyr::bind_cols(source_values, color_tbl)
}

generate_tour_spec <- function(tour_frame, color_tbl, half_range) {
  json <- file.path(schema_dir(), "tour-proto.json")
  ans <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  ans <- set_half_range(ans, half_range)
  ans <- set_encoding_color(ans, color_tbl, colnames(color_tbl))
  ans <- set_encoding_opacity(ans, alpha = opacity_value(nrow(tour_frame)))
  ans <- set_data_name(ans, "path")
  ans <- set_data_values(ans, tour_frame)
  ans
}

spec_tour <- function(tour_frame, color_tbl, half_range) {

  view_tour <- generate_tour_spec(tour_frame, color_tbl, half_range)

  vegawidget::vegawidget(
    vegawidget::as_vegaspec(view_tour),
    embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE)
  )
}

set_encoding_color_op_linked <- function(layer, color_tbl, color_name) {

  has_colclick <- "colclick" %in% names(layer[["selection"]])

  has_color_col <- length(color_name) == 0

  if (has_color_col) {
    layer[["encoding"]][["color"]][["condition"]] <-
      list(selection =
             layer[["encoding"]][["color"]][["condition"]][["selection"]],
           value = "black")
    if (has_colclick) {
      layer[["selection"]] <- layer[["selection"]][1]
    }

    layer[["encoding"]][["opacity"]][["condition"]][["selection"]] <- "left_brush"

    return(layer)
  }

  color_vec <- color_tbl[[1]]
  layer[["encoding"]][["color"]][["condition"]][["field"]] <-
    color_name
  layer[["encoding"]][["color"]][["condition"]][["type"]] <-
    color_type(color_vec)
  layer[["encoding"]][["color"]][["condition"]][["scale"]][["domain"]] <-
    color_scale(color_vec)
  layer[["encoding"]][["color"]][["condition"]][["legend"]] <- list(title = color_name)

  if (has_colclick) {
    layer[["selection"]][["colclick"]][["fields"]] <- list(color_name)
  }

  layer

}

generate_linked_tour_spec <- function(x_frame, y_frame, x_color_tbl, y_color_tbl, half_range) {
  json <- file.path(schema_dir(), "tour-linked-proto.json")
  ans <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

  embed_cols <- colnames(y_frame)
  embed_layer <- ans$hconcat[[1]]

  embed_layer[["encoding"]][["x"]][["field"]] <- embed_cols[1]
  embed_layer[["encoding"]][["x"]][["scale"]] <-
    list(domain = range(y_frame[[embed_cols[1]]]))
  embed_layer[["encoding"]][["y"]][["field"]] <- embed_cols[2]
  embed_layer[["encoding"]][["y"]][["scale"]] <-
    list(domain = range(y_frame[[embed_cols[2]]]))

  embed_layer <-
    set_encoding_color_op_linked(embed_layer,
                                 y_color_tbl,
                                 colnames(y_color_tbl))

  tour_layer <- ans$hconcat[[2]]
  tour_layer <- set_half_range(tour_layer, half_range)
  tour_layer <-
    set_encoding_color_op_linked(tour_layer,
                                 x_color_tbl,
                                 colnames(x_color_tbl))

  ans$transform[[1]]$from$fields <- c(ans$transform[[1]]$from$fields,
                                      colnames(x_color_tbl))

  ans$hconcat[[1]] <- embed_layer
  ans$hconcat[[2]] <- tour_layer

  ans$datasets$path <- x_frame
  ans$datasets$embed <- y_frame

  ans
}

spec_linked_tour <- function(x_frame, y_frame, x_color_tbl, y_color_tbl, half_range) {
  view_tour <-
    generate_linked_tour_spec(x_frame, y_frame,
                              x_color_tbl, y_color_tbl, half_range)

  vegawidget::vegawidget(
    vegawidget::as_vegaspec(view_tour),
    embed = vegawidget::vega_embed(actions = FALSE, tooltip = FALSE)
  )
}

