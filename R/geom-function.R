#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.R
GeomFunction <- ggproto("GeomFunction", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    groups <- unique0(data$group)
    if (length(groups) > 1) {
      cli::cli_warn(c(
        "Multiple drawing groups in {.fn {snake_class(self)}}",
        "i" = "Did you use the correct {.field group}, {.field colour}, or {.field fill} aesthetics?"
      ))
    }

    ggproto_parent(GeomPath, self)$draw_panel(
      data, panel_params, coord, arrow, arrow.fill, lineend, linejoin, linemitre, na.rm
    )
  }
)

#' Draw a function as a continuous curve
#'
#' Computes and draws a function as a continuous curve. This makes it easy to
#' superimpose a function on top of an existing plot. The function is called
#' with a grid of evenly spaced values along the x axis, and the results are
#' drawn (by default) with a line.
#'
#' @aesthetics GeomFunction
#' @param data Ignored by `stat_function()`, do not use.
#' @inheritParams layer
#' @inheritParams geom_path
#' @examples
#'
#' # geom_function() is useful for overlaying functions
#' set.seed(1492)
#' ggplot(data.frame(x = rnorm(100)), aes(x)) +
#'   geom_density() +
#'   geom_function(fun = dnorm, colour = "red")
#'
#' # To plot functions without data, specify range of x-axis
#' base <-
#'   ggplot() +
#'   xlim(-5, 5)
#'
#' base + geom_function(fun = dnorm)
#'
#' base + geom_function(fun = dnorm, args = list(mean = 2, sd = .5))
#'
#' # The underlying mechanics evaluate the function at discrete points
#' # and connect the points with lines
#' base + stat_function(fun = dnorm, geom = "point")
#'
#' base + stat_function(fun = dnorm, geom = "point", n = 20)
#'
#' base + stat_function(fun = dnorm, geom = "polygon", color = "blue", fill = "blue", alpha = 0.5)
#'
#' base + geom_function(fun = dnorm, n = 20)
#'
#' # Two functions on the same plot
#' base +
#'   geom_function(aes(colour = "normal"), fun = dnorm) +
#'   geom_function(aes(colour = "t, df = 1"), fun = dt, args = list(df = 1))
#'
#' # Using a custom anonymous function
#' base + geom_function(fun = \(x) 0.5 * exp(-abs(x)))
#' # or using lambda syntax:
#' # base + geom_function(fun = ~ 0.5 * exp(-abs(.x)))
#' # or using a custom named function:
#' # f <- function(x) 0.5 * exp(-abs(x))
#' # base + geom_function(fun = f)
#'
#' # Using xlim to restrict the range of function
#' ggplot(data.frame(x = rnorm(100)), aes(x)) +
#' geom_density() +
#' geom_function(fun = dnorm, colour = "red", xlim=c(-1, 1))
#'
#' # Using xlim to widen the range of function
#' ggplot(data.frame(x = rnorm(100)), aes(x)) +
#' geom_density() +
#' geom_function(fun = dnorm, colour = "red", xlim=c(-7, 7))
#'
#' @export
geom_function <- make_constructor(
  GeomFunction, stat = "function",
  checks = exprs(data <- data %||% ensure_nonempty_data)
)
