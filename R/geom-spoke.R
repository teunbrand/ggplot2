#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomSpoke <- ggproto(
  "GeomSpoke", GeomSegment,
  setup_data = function(data, params) {
    data$radius <- data$radius %||% params$radius
    data$angle <- data$angle %||% params$angle

    transform(data,
              xend = x + cos(angle) * radius,
              yend = y + sin(angle) * radius
    )
  },
  required_aes = c("x", "y", "angle", "radius")
)

#' Line segments parameterised by location, direction and distance
#'
#' This is a polar parameterisation of [geom_segment()]. It is
#' useful when you have variables that describe direction and distance.
#' The angles start from east and increase counterclockwise.
#'
#' @aesthetics GeomSpoke
#' @inheritParams layer
#' @inheritParams geom_segment
#' @export
#' @examples
#' df <- expand.grid(x = 1:10, y=1:10)
#'
#' set.seed(1)
#' df$angle <- runif(100, 0, 2*pi)
#' df$speed <- runif(100, 0, sqrt(0.1 * df$x))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = angle), radius = 0.5)
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = angle, radius = speed))
geom_spoke <- make_constructor(GeomSpoke)

#' @export
#' @rdname geom_spoke
#' @usage NULL
stat_spoke <- function(...) {
  lifecycle::deprecate_stop("2.0.0", "stat_spoke()", "geom_spoke()")
}
