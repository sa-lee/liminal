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


