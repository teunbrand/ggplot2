#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomErrorbar <- ggproto(
  "GeomErrorbar", Geom,

  default_aes = aes(
    colour = from_theme(colour %||% ink),
    linewidth = from_theme(linewidth),
    linetype = from_theme(linetype),
    width = 0.9,
    alpha = NA
  ),

  draw_key = draw_key_path,

  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

  setup_params = function(data, params) {
    params <- GeomLinerange$setup_params(data, params)
    if (
      isTRUE(params$flipped_aes) &&
      isTRUE("height" %in% names(params)) &&
      !isTRUE("width" %in% names(params))
    ) {
      params <- rename(params, c(height = "width"))
      cli::cli_inform("{.arg height} was translated to {.arg width}.")
    }
    params
  },

  extra_params = c("na.rm", "orientation", "height"),

  setup_data = function(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- compute_data_size(
      data, params$width,
      default = self$default_aes$width,
      zero = FALSE, discrete = TRUE
    )
    data <- transform(data,
                      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  # Note: `width` is vestigial
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        width = NULL, flipped_aes = FALSE) {
    data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    x <- vec_interleave(data$xmin, data$xmax, NA, data$x,    data$x,    NA, data$xmin, data$xmax)
    y <- vec_interleave(data$ymax, data$ymax, NA, data$ymax, data$ymin, NA, data$ymin, data$ymin)
    data <- data_frame0(
      x = x,
      y = y,
      colour = rep(data$colour, each = 8),
      alpha = rep(data$alpha, each = 8),
      linewidth = rep(data$linewidth, each = 8),
      linetype = rep(data$linetype, each = 8),
      group = rep(seq_len(nrow(data)), each = 8),
      .size = nrow(data) * 8
    )
    data <- flip_data(data, flipped_aes)
    GeomPath$draw_panel(data, panel_params, coord, lineend = lineend)
  },

  rename_size = TRUE
)

#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomErrorbarh <- ggproto(
  "GeomErrorbarh", GeomErrorbar,
  setup_params = function(data, params) {
    deprecate_soft0(
      "4.0.0", "geom_errobarh()", "geom_errorbar(orientation = \"y\")",
      id = "no-more-errorbarh"
    )
    GeomLinerange$setup_params(data, params)
  }
)

#' @export
#' @rdname geom_linerange
geom_errorbar <- make_constructor(GeomErrorbar, orientation = NA)

#' @export
#' @rdname geom_linerange
#' @note
#' `geom_errorbarh()` is `r lifecycle::badge("deprecated")`. Use
#' `geom_errorbar(orientation = "y")` instead.
geom_errorbarh <- function(..., orientation = "y") {
  deprecate_soft0(
    "4.0.0", "geom_errobarh()", "geom_errorbar(orientation = \"y\")",
    id = "no-more-errorbarh"
  )
  geom_errorbar(..., orientation = orientation)
}
