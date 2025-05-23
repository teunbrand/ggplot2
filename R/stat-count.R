#' @rdname Stat
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.R
StatCount <- ggproto(
  "StatCount", Stat,
  required_aes = "x|y",

  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1),

  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
    }

    if (is.null(params$width)) {
      x <- if (params$flipped_aes) "y" else "x"
      params$width <- resolution(data[[x]], discrete = TRUE) * 0.9
    }

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(self, data, scales, width = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))

    count <- as.vector(rowsum(weight, x, na.rm = TRUE))

    bars <- data_frame0(
      count = count,
      prop = count / sum(abs(count)),
      x = sort(unique0(x)),
      width = width,
      flipped_aes = flipped_aes,
      .size = length(count)
    )
    flip_data(bars, flipped_aes)
  },

  dropped_aes = "weight"
)

#' @eval rd_computed_vars(
#'   count = "number of points in bin.",
#'   prop  = "groupwise proportion"
#' )
#' @seealso [stat_bin()], which bins data in ranges and counts the
#'   cases in each range. It differs from `stat_count()`, which counts the
#'   number of cases at each `x` position (without binning into ranges).
#'   [stat_bin()] requires continuous `x` data, whereas
#'   `stat_count()` can be used for both discrete and continuous `x` data.
#'
#' @export
#' @rdname geom_bar
stat_count <- make_constructor(
  StatCount, geom = "bar", position = "stack",
  orientation = NA, omit = "width"
)
