#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
GeomTile <- ggproto(
  "GeomTile", GeomRect,
  extra_params = c("na.rm"),

  setup_data = function(self, data, params) {

    data <- compute_data_size(
      data, params$width,
      default = self$default_aes$width,
      panels = "by", target = "width",
      zero = FALSE, discrete = TRUE
    )
    data <- compute_data_size(
      data, params$height,
      default = self$default_aes$height,
      panels = "by", target = "height",
      zero = FALSE, discrete = TRUE
    )
    transform(data,
              xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = aes(
    fill = from_theme(fill %||% col_mix(ink, paper, 0.2)),
    colour = from_theme(colour %||% NA),
    linewidth = from_theme(0.4 * borderwidth),
    linetype = from_theme(bordertype),
    alpha = NA, width = 1, height = 1
  ),

  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_key = draw_key_polygon
)

#' Rectangles
#'
#' `geom_rect()` and `geom_tile()` do the same thing, but are
#' parameterised differently: `geom_tile()` uses the center of the tile and its
#' size (`x`, `y`, `width`, `height`), while `geom_rect()` can use those or the
#' locations of the corners (`xmin`, `xmax`, `ymin` and `ymax`).
#' `geom_raster()` is a high performance special case for when all the tiles
#' are the same size, and no pattern fills are applied.
#'
#' @aesthetics GeomRect
#' `geom_tile()` understands only the `x`/`width` and `y`/`height` combinations.
#' Note that `geom_raster()` ignores `colour`.
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_segment
#' @export
#'
#' @details
#' Please note that the `width` and `height` aesthetics are not true position
#' aesthetics and therefore are not subject to scale transformation. It is
#' only after transformation that these aesthetics are applied.
#'
#' @examples
#' # The most common use for rectangles is to draw a surface. You always want
#' # to use geom_raster here because it's so much faster, and produces
#' # smaller output when saving to PDF
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density))
#'
#' # Interpolation smooths the surface & is most helpful when rendering images.
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density), interpolate = TRUE)
#'
#' # If you want to draw arbitrary rectangles, use geom_tile() or geom_rect()
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = z), colour = "grey50")
#' ggplot(df, aes(x, y, width = w)) +
#'   geom_tile(aes(fill = z), colour = "grey50")
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   geom_rect(aes(fill = z), colour = "grey50")
#'
#' \donttest{
#' # Justification controls where the cells are anchored
#' df <- expand.grid(x = 0:5, y = 0:5)
#' set.seed(1)
#' df$z <- runif(nrow(df))
#' # default is compatible with geom_tile()
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster()
#' # zero padding
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster(hjust = 0, vjust = 0)
#'
#' # Inspired by the image-density plots of Ken Knoblauch
#' cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
#' cars + geom_point()
#' cars + stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(3,1))
#' cars + stat_bin_2d(aes(fill = after_stat(density)), binwidth = c(3,1))
#'
#' cars +
#'   stat_density(
#'     aes(fill = after_stat(density)),
#'     geom = "raster",
#'     position = "identity"
#'    )
#' cars +
#'   stat_density(
#'     aes(fill = after_stat(count)),
#'     geom = "raster",
#'     position = "identity"
#'   )
#' }
geom_tile <- make_constructor(GeomTile)
