# function (data, tour_path = grand_tour(), display = display_xy(),
#           start = NULL, aps = 1, fps = 30, max_frames = Inf, rescale = TRUE,
#           sphere = FALSE, ...)
# {
#   if (rescale)
#     data <- rescale(data)
#   if (sphere)
#     data <- sphere_data(data)
#   if (!interactive() && missing(max_frames)) {
#     max_frames <- 1
#   }
#   if (max_frames == Inf) {
#     to_stop()
#   }
#   plat <- find_platform()
#   if (rstudio_gd() && fps > 19) {
#     warning("Rstudio graphics device supports maximum fps of 19",
#             call. = FALSE)
#     fps <- 19
#   }
#   tour <- new_tour(data, tour_path, start)
#   start <- tour(0)
#   bs <- 1
#   bases <- array(NA, c(ncol(data), ncol(start$target), bs))
#   display$init(data)
#   display$render_frame()
#   display$render_data(data, start$proj)
#   b <- 0
#   i <- 0
#   tryCatch({
#     while (i < max_frames) {
#       i <- i + 1
#       step <- tour(aps/fps)
#       if (step$step == 1) {
#         b <- b + 1
#         if (b > bs) {
#           bases <- c(bases, rep(NA, bs * dim(bases)[1] *
#                                   dim(bases)[2]))
#           dim(bases) <- c(ncol(data), ncol(start$target),
#                           2 * bs)
#           bs <- 2 * bs
#         }
#         bases[, , b] <- step$target
#       }
#       dev.hold()
#       on.exit(dev.flush())
#       if (plat$os == "win" || plat$iface == "rstudio") {
#         display$render_frame()
#       }
#       else {
#         display$render_transition()
#       }
#       display$render_data(data, step$proj, step$target)
#       dev.flush()
#       if (step$step < 0)
#         break
#       Sys.sleep(1/fps)
#     }
#   }, interrupt = function(cond) {
#     dev.flush()
#     return()
#   })
#   invisible(bases[, , seq_len(b)])
# }
