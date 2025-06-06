#' Annotation: log tick marks
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is superseded by using [`guide_axis_logticks()`].
#'
#' This annotation adds log tick marks with diminishing spacing.
#' These tick marks probably make sense only for base 10.
#'
#' @param base the base of the log (default 10)
#' @param sides a string that controls which sides of the plot the log ticks appear on.
#'   It can be set to a string containing any of `"trbl"`, for top, right,
#'   bottom, and left.
#' @param outside logical that controls whether to move the log ticks outside
#' of the plot area. Default is off (`FALSE`). You will also need to use
#' `coord_cartesian(clip = "off")`. See examples.
#' @param short a [grid::unit()] object specifying the length of the
#'   short tick marks
#' @param mid a [grid::unit()] object specifying the length of the
#'   middle tick marks. In base 10, these are the "5" ticks.
#' @param long a [grid::unit()] object specifying the length of the
#'   long tick marks. In base 10, these are the "1" (or "10") ticks.
#' @param scaled is the data already log-scaled? This should be `TRUE`
#'   (default) when the data is already transformed with `log10()` or when
#'   using `scale_y_log10()`. It should be `FALSE` when using
#'   `coord_transform(y = "log10")`.
#' @param colour Colour of the tick marks.
#' @param linewidth Thickness of tick marks, in mm.
#' @param linetype Linetype of tick marks (`solid`, `dashed`, etc.)
#' @param alpha The transparency of the tick marks.
#' @param color An alias for `colour`.
#' @param ... Other parameters passed on to the layer
#' @param size `r lifecycle::badge("deprecated")`
#'
#' @export
#' @seealso [scale_y_continuous()], [scale_y_log10()] for log scale
#'   transformations.
#' @seealso [coord_transform()] for log coordinate transformations.
#'
#' @examples
#' # Make a log-log plot (without log ticks)
#' a <- ggplot(msleep, aes(bodywt, brainwt)) +
#'  geom_point(na.rm = TRUE) +
#'  scale_x_log10(
#'    breaks = scales::trans_breaks("log10", \(x) 10^x),
#'    labels = scales::trans_format("log10", scales::math_format(10^.x))
#'  ) +
#'  scale_y_log10(
#'    breaks = scales::trans_breaks("log10", \(x) 10^x),
#'    labels = scales::trans_format("log10", scales::math_format(10^.x))
#'  ) +
#'  theme_bw()
#'
#' a + annotation_logticks()                # Default: log ticks on bottom and left
#' a + annotation_logticks(sides = "lr")    # Log ticks for y, on left and right
#' a + annotation_logticks(sides = "trbl")  # All four sides
#'
#' a + annotation_logticks(sides = "lr", outside = TRUE) +
#'  coord_cartesian(clip = "off")  # Ticks outside plot
#'
#' # Hide the minor grid lines because they don't align with the ticks
#' a + annotation_logticks(sides = "trbl") + theme(panel.grid.minor = element_blank())
#'
#' # Another way to get the same results as 'a' above: log-transform the data before
#' # plotting it. Also hide the minor grid lines.
#' b <- ggplot(msleep, aes(log10(bodywt), log10(brainwt))) +
#'  geom_point(na.rm = TRUE) +
#'  scale_x_continuous(name = "body", labels = scales::label_math(10^.x)) +
#'  scale_y_continuous(name = "brain", labels = scales::label_math(10^.x)) +
#'  theme_bw() + theme(panel.grid.minor = element_blank())
#'
#' b + annotation_logticks()
#'
#' # Using a coordinate transform requires scaled = FALSE
#' t <- ggplot(msleep, aes(bodywt, brainwt)) +
#'   geom_point() +
#'   coord_transform(x = "log10", y = "log10") +
#'   theme_bw()
#' t + annotation_logticks(scaled = FALSE)
#'
#' # Change the length of the ticks
#' a + annotation_logticks(
#'   short = unit(.5,"mm"),
#'   mid = unit(3,"mm"),
#'   long = unit(4,"mm")
#' )
annotation_logticks <- function(base = 10, sides = "bl", outside = FALSE, scaled = TRUE,
    short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
    colour = "black", linewidth = 0.5, linetype = 1, alpha = 1, color = NULL, ...,
    size = deprecated())
{
  if (!is.null(color))
    colour <- color

  lifecycle::signal_stage("superseded", "annotation_logticks()", "guide_axis_logticks()")

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.5.0", I("Using the `size` aesthetic in this geom"), I("`linewidth`"))
    linewidth <- linewidth %||% size
  }

  layer(
    data = dummy_data(),
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomLogticks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      outside = outside,
      scaled = scaled,
      short = short,
      mid = mid,
      long = long,
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  )
}

#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomLogticks <- ggproto("GeomLogticks", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coord, base = 10, sides = "bl",
                        outside = FALSE, scaled = TRUE, short = unit(0.1, "cm"),
                        mid = unit(0.2, "cm"), long = unit(0.3, "cm"))
  {
    ticks <- list()
    flipped <- inherits(coord, "CoordFlip")
    x_name <- if (flipped) "y" else "x"
    y_name <- if (flipped) "x" else "y"

    # Convert these units to numbers so that they can be put in data frames
    short <- convertUnit(short, "cm", valueOnly = TRUE)
    mid   <- convertUnit(mid,   "cm", valueOnly = TRUE)
    long  <- convertUnit(long,  "cm", valueOnly = TRUE)

    if (grepl("[b|t]", sides) && all(is.finite(panel_params$x.range))) {

      # Get positions of x tick marks
      xticks <- calc_logticks(
        base = base,
        minpow = floor(panel_params$x.range[1]),
        maxpow = ceiling(panel_params$x.range[2]),
        start = 0,
        shortend = short,
        midend = mid,
        longend = long
      )

      if (scaled)
        xticks$value <- log(xticks$value, base)

      names(xticks)[names(xticks) == "value"] <- x_name   # Rename to 'x' for coordinates$transform
      xticks <- coord$transform(xticks, panel_params)
      xticks <- xticks[xticks$x <= 1 & xticks$x >= 0,]

      if (outside)
        xticks$end = -xticks$end

      # Make the grobs
      if (grepl("b", sides) && nrow(xticks) > 0) {
        ticks$x_b <- with(data, segmentsGrob(
          x0 = unit(xticks$x, "native"), x1 = unit(xticks$x, "native"),
          y0 = unit(xticks$start, "cm"), y1 = unit(xticks$end, "cm"),
          gp = gg_par(col = alpha(colour, alpha), lty = linetype, lwd = linewidth)
        ))
      }
      if (grepl("t", sides) && nrow(xticks) > 0) {
        ticks$x_t <- with(data, segmentsGrob(
          x0 = unit(xticks$x, "native"), x1 = unit(xticks$x, "native"),
          y0 = unit(1, "npc") - unit(xticks$start, "cm"), y1 = unit(1, "npc") - unit(xticks$end, "cm"),
          gp = gg_par(col = alpha(colour, alpha), lty = linetype, lwd = linewidth)
        ))
      }
    }

    if (grepl("[l|r]", sides) && all(is.finite(panel_params$y.range))) {
      yticks <- calc_logticks(
        base = base,
        minpow = floor(panel_params$y.range[1]),
        maxpow = ceiling(panel_params$y.range[2]),
        start = 0,
        shortend = short,
        midend = mid,
        longend = long
      )

      if (scaled)
        yticks$value <- log(yticks$value, base)

      names(yticks)[names(yticks) == "value"] <- y_name   # Rename to 'y' for coordinates$transform
      yticks <- coord$transform(yticks, panel_params)
      yticks <- yticks[yticks$y <= 1 & yticks$y >= 0,]

      if (outside)
        yticks$end = -yticks$end

      # Make the grobs
      if (grepl("l", sides) && nrow(yticks) > 0) {
        ticks$y_l <- with(data, segmentsGrob(
          y0 = unit(yticks$y, "native"), y1 = unit(yticks$y, "native"),
          x0 = unit(yticks$start, "cm"), x1 = unit(yticks$end, "cm"),
          gp = gg_par(col = alpha(colour, alpha), lty = linetype, lwd = linewidth)
        ))
      }
      if (grepl("r", sides) && nrow(yticks) > 0) {
        ticks$y_r <- with(data, segmentsGrob(
          y0 = unit(yticks$y, "native"), y1 = unit(yticks$y, "native"),
          x0 = unit(1, "npc") - unit(yticks$start, "cm"), x1 = unit(1, "npc") - unit(yticks$end, "cm"),
          gp = gg_par(col = alpha(colour, alpha), lty = linetype, lwd = linewidth)
        ))
      }
    }

    gTree(children = inject(gList(!!!ticks)))
  },

  default_aes = aes(
    colour = from_theme(colour %||% ink),
    linewidth = from_theme(linewidth),
    linetype = from_theme(linetype),
    alpha = 1
  )
)


# Calculate the position of log tick marks
# Returns data frame with:
# - value: the position of the log tick on the data axis, for example 1, 2, ..., 9, 10, 20, ...
# - start: on the other axis, start position of the line (usually 0)
# - end: on the other axis, end position of the line (for example, .1, .2, or .3)
calc_logticks <- function(base = 10, ticks_per_base = base - 1,
    minpow = 0, maxpow = minpow + 1, start = 0, shortend = 0.1, midend = 0.2, longend = 0.3) {

  # Number of blocks of tick marks
  reps <- maxpow - minpow

  # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
  ticknums  <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)

  # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
  powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)

  ticks  <- ticknums * base ^ powers
  ticks  <- c(ticks, base ^ maxpow)  # Add the last tick mark

  # Set all of the ticks short
  tickend <- rep(shortend, length(ticks))

  # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
  cycleIdx <- ticknums - 1

  # Set the "major" ticks long
  tickend[cycleIdx == 0] <- longend

  # Where to place the longer tick marks that are between each base
  # For base 10, this will be at each 5
  longtick_after_base <- floor(ticks_per_base/2)
  tickend[ cycleIdx == longtick_after_base ] <- midend

  tickdf <- data_frame0(
    value = ticks,
    start = start,
    end = tickend,
    .size = length(ticks)
  )

  return(tickdf)
}
