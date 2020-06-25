# Reactives for a given tour path

#' @importFrom shiny reactive invalidateLater
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

rct_tour <- function(plan, aps = 1, fps = 8, rct_event, rct_refresh, selections, session) {
  current <- plan(0)
  shiny::reactive({
    play <- rct_refresh()
    play <- current$step >= 0 && play
    db <- shiny::debounce(rct_event, 1000/(3*fps))
    play <- !db() && play
    if (play) {
      current <<- plan(aps/fps)
      selections[["proj"]] <- current$proj
      shiny::invalidateLater(1000*aps/fps, session)
    }
    current
  })
}
