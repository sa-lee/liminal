# Reactives for a given tour path
rct_tour <- function(plan, aps = 1, fps = 24, session) {
  shiny::reactive({
    step <- plan(aps/fps)
    invalidateLater(1000/fps, session)
    step
  })
}

stream_axes <- function(rct_tour, cols) {
  shiny::reactive({
    generate_axes(rct_tour()$proj, cols)
  })
}

stream_proj <- function(rct_tour, tour_data, source_values, half_range, transformer) {
  shiny::reactive({
    x <- tour_data %*% rct_tour()$proj
    x <- transformer(x) / half_range
    source_values[, c("x","y")] <- as.data.frame(x)
    source_values
  })
}
