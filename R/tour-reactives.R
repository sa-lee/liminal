# Reactives for a given tour path

rct_pause <- function(rct_shift_click) {
  shiny::reactive({
    res <- rct_shift_click()
    length(res) > 0
  })
}

# reactive half range
rct_half_range <- function(rct_zoom, half_range) {
  shiny::reactive({
    res <- rct_zoom()
    res <- unlist(res)
    if (length(res)) {
      return(max(abs(res)))
    }
    return(half_range)
  })
}

rct_tour <- function(plan, aps = 1, fps = 12, rct_event, rct_refresh, session) {
  current <- plan(0)
  shiny::reactive({
    play <- rct_refresh()
    play <- current$step >= 0 && play
    play <- !rct_event() && play


    if (play) {
      current <<- plan(aps/fps)
      invalidateLater(1000/fps, session)
      Sys.sleep(1/fps)
    }

    current
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
