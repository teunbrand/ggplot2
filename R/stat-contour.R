#' @rdname Stat
#' @format NULL
#' @usage NULL
#' @export
StatContour <- ggproto(
  "StatContour", Stat,

  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level)),
  # z and weight get dropped during statistical transformation
  dropped_aes = c("z", "weight"),

  setup_params = function(data, params) {
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params
  },

  setup_data = function(data, params) {
    contour_deduplicate(data)
  },

  compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL,
                           breaks = NULL, na.rm = FALSE) {
    # Undo data rotation
    rotation <- estimate_contour_angle(data$x, data$y)
    data[c("x", "y")] <- rotate_xy(data$x, data$y, -rotation)

    breaks <- contour_breaks(z.range, bins, binwidth, breaks)

    isolines <- withr::with_options(list(OutDec = "."), xyz_to_isolines(data, breaks))
    path_df <- iso_to_geom(isolines, data$group[1], geom = "path")

    path_df$level <- as.numeric(path_df$level)
    path_df$nlevel <- rescale_max(path_df$level)

    # Re-apply data rotation
    path_df[c("x", "y")] <- rotate_xy(path_df$x, path_df$y, rotation)
    path_df
  }
)

#' @rdname Stat
#' @format NULL
#' @usage NULL
#' @export
StatContourFilled <- ggproto(
  "StatContourFilled", Stat,

  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level), fill = after_stat(level)),
  # z and weight get dropped during statistical transformation
  dropped_aes = c("z", "weight"),

  setup_params = function(data, params) {
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params
  },

  setup_data = function(data, params) {
    contour_deduplicate(data)
  },

  compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE) {

    # Undo data rotation
    rotation <- estimate_contour_angle(data$x, data$y)
    data[c("x", "y")] <- rotate_xy(data$x, data$y, -rotation)

    breaks <- contour_breaks(z.range, bins, binwidth, breaks)

    isobands <- withr::with_options(list(OutDec = "."), xyz_to_isobands(data, breaks))
    names(isobands) <- pretty_isoband_levels(names(isobands))
    path_df <- iso_to_geom(isobands, data$group[1], geom = "polygon")

    path_df$level <- ordered(path_df$level, levels = names(isobands))
    path_df$level_low <- breaks[as.numeric(path_df$level)]
    path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
    path_df$level_mid <- 0.5*(path_df$level_low + path_df$level_high)
    path_df$nlevel <- rescale_max(path_df$level_high)
    # Re-apply data rotation
    path_df[c("x", "y")] <- rotate_xy(path_df$x, path_df$y, rotation)

    path_df
  }
)

#' @inheritParams stat_identity
#' @inheritParams geom_contour
#' @export
#' @aesthetics StatContour
#' @aesthetics StatContourFilled
#' @eval rd_computed_vars(
#'   .details = "The computed variables differ somewhat for contour lines
#'   (computed by `stat_contour()`) and contour bands (filled contours,
#'   computed by `stat_contour_filled()`). The variables `nlevel` and `piece`
#'   are available for both, whereas `level_low`, `level_high`, and `level_mid`
#'   are only available for bands. The variable `level` is a numeric or a factor
#'   depending on whether lines or bands are calculated.",
#'   level = "Height of contour. For contour lines, this is a numeric vector
#'   that represents bin boundaries. For contour bands, this is an ordered
#'   factor that represents bin ranges.",
#'   "level_low,level_high,level_mid" = "(contour bands only) Lower and upper
#'   bin boundaries for each band, as well as the mid point between boundaries.",
#'   nlevel = "Height of contour, scaled to a maximum of 1.",
#'   piece = "Contour piece (an integer)."
#' )
#'
#' @section Dropped variables:
#' \describe{
#'   \item{`z`}{After contouring, the z values of individual data points are no longer available.}
#' }
#'
#'
#' @rdname geom_contour
stat_contour <- make_constructor(
  StatContour, geom = "contour",
  omit = "z.range"
)

#' @rdname geom_contour
#' @export
stat_contour_filled <- make_constructor(
  StatContourFilled, geom = "contour_filled",
  omit = "z.range"
)

#' Calculate the breaks used for contouring
#'
#' @inheritParams geom_contour
#' @param z_range Range of values within which breaks should be calculated
#'
#' @return A vector of breaks
#' @noRd
#'
contour_breaks <- function(z_range, bins = NULL, binwidth = NULL, breaks = NULL) {
  breaks <- allow_lambda(breaks)

  if (is.numeric(breaks)) {
    return(breaks)
  }

  breaks_fun <- fullseq
  if (is.function(breaks)) {
    breaks_fun <- breaks
  } else if (is.null(bins) && is.null(binwidth)) {
    # If no parameters set, use pretty bins
    breaks <- pretty(z_range, 10)
    return(breaks)
  }

  # If provided, use bins to calculate binwidth
  if (!is.null(bins)) {
    # round lower limit down and upper limit up to make sure
    # we generate bins that span the data range nicely
    accuracy <- signif(diff(z_range), 1)/10
    z_range[1] <- floor(z_range[1]/accuracy)*accuracy
    z_range[2] <- ceiling(z_range[2]/accuracy)*accuracy

    if (bins == 1) {
      return(z_range)
    }

    binwidth <- diff(z_range) / (bins - 1)
    breaks <- breaks_fun(z_range, binwidth)

    # Sometimes the above sequence yields one bin too few.
    # If this happens, try again.
    if (length(breaks) < bins + 1) {
      binwidth <- diff(z_range) / bins
      breaks <- breaks_fun(z_range, binwidth)
    }

    return(breaks)
  }

  # if we haven't returned yet, compute breaks from binwidth
  breaks_fun(z_range, binwidth %||% (diff(z_range) / 10))
}

#' Compute isoband objects
#'
#' @param data A data frame with columns `x`, `y`, and `z`.
#' @param breaks A vector of breaks. These are the values for
#'   which contour lines will be computed.
#'
#' @return An S3 "iso" object, which is a `list()` of `list(x, y, id)`s.
#' @noRd
#'
xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique0(data$x)),
    y = sort(unique0(data$y)),
    z = isoband_z_matrix(data),
    levels = breaks
  )
}

xyz_to_isobands <- function(data, breaks) {
  isoband::isobands(
    x = sort(unique0(data$x)),
    y = sort(unique0(data$y)),
    z = isoband_z_matrix(data),
    levels_low = breaks[-length(breaks)],
    levels_high = breaks[-1]
  )
}

#' Compute input matrix for isoband functions
#'
#' Note that [grDevices::contourLines()] needs transposed
#' output to the matrix returned by this function.
#'
#' @param data A data frame with columns `x`, `y`, and `z`.
#'
#' @return A [matrix()]
#' @noRd
#'
isoband_z_matrix <- function(data) {
  # Convert vector of data to raster
  x_pos <- as.integer(factor(data$x, levels = sort(unique0(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique0(data$y))))

  nrow <- max(y_pos)
  ncol <- max(x_pos)

  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z

  raster
}

#' Convert the output of isoband functions
#'
#' @param iso the output of [isoband::isobands()] or [isoband::isolines()]
#' @param group the name of the group
#' @param geom The type of geometry to return. Either `"path"` or `"polygon"`
#'   for isolines and isobands respectively.
#'
#' @return A data frame that can be passed to [geom_polygon()] or [geom_path()].
#' @noRd
#'
iso_to_geom <- function(iso, group = 1, geom = "path") {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))

  if (all(lengths == 0)) {
    cli::cli_warn("{.fn stat_contour}: Zero contours were generated")
    return(data_frame0())
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so that groups can be properly sorted
  groups <- paste(group, sprintf("%03d", item_id), sep = "-")
  if (geom == "path") {
    groups <- paste(groups, sprintf("%03d", ids), sep = "-")
    ids <- NULL
  }

  groups <- factor(groups)

  data_frame0(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = as.integer(groups),
    group = groups,
    subgroup = ids,
    .size = length(xs)
  )
}

#' Pretty isoband level names
#'
#' @param isoband_levels `names()` of an [isoband::isobands()] object.
#'
#' @return A vector of labels like those used in
#'   [cut()] and [cut_inverval()].
#' @noRd
#'
pretty_isoband_levels <- function(isoband_levels, dig.lab = 3) {
  interval_low <- as.numeric(gsub(":.*$", "", isoband_levels))
  interval_high <- as.numeric(gsub("^[^:]*:", "", isoband_levels))

  breaks <- unique(c(interval_low, interval_high))

  while(anyDuplicated(format(breaks, digits = dig.lab, trim = TRUE))) {
    dig.lab <- dig.lab + 1
  }

  label_low <- format(interval_low, digits = dig.lab, trim = TRUE)
  label_high <- format(interval_high, digits = dig.lab, trim = TRUE)

  # from the isoband::isobands() docs:
  # the intervals specifying isobands are closed at their lower boundary
  # and open at their upper boundary
  sprintf("(%s, %s]", label_low, label_high)
}

#' De-duplicate data for contours
#'
#' Gives a warning if data has duplicates and throws out duplicated rows.
#'
#' @param data A `data.frame`
#' @param check Column names to check for duplicates
#'
#' @return A de-duplicated `data.frame`
#' @noRd
contour_deduplicate <- function(data, check = c("x", "y", "group", "PANEL")) {
  check <- intersect(check, names(data))
  if (length(check) == 0) {
    return(data)
  }
  if (vec_duplicate_any(data[, check, drop = FALSE])) {
    # We use fromLast here to be consistent with `isoband_z_matrix()` behaviour
    dups <- duplicated(data[, check, drop = FALSE], fromLast = TRUE)
    data <- data[!dups, , drop = FALSE]

    cli::cli_warn(c(
      "Contour data has duplicated {.field x}, {.field y} coordinates.",
      i = "{sum(dups)} duplicated row{?s} have been dropped."
    ))
  }
  data
}

estimate_contour_angle <- function(x, y) {

  # Compute most frequent angle among first 20 points
  all_angles <- atan2(diff(head(y, 20L)), diff(head(x, 20L)))
  freq <- tabulate(match(all_angles, unique(all_angles)))
  i <- which.max(freq)

  # If this angle represents less than half of the angles, we probably
  # have unordered data, in which case the approach above is invalid
  if ((freq[i] / sum(freq)) < 0.5) {
    # In such case, try approach with convex hull
    hull <- grDevices::chull(x, y)
    hull <- c(hull, hull[1])
    # Find largest edge along hull
    dx <- diff(x[hull])
    dy <- diff(y[hull])
    i <- which.max(sqrt(dx^2 + dy^2))
    # Take angle of largest edge
    angle <- atan2(dy[i], dx[i])
  } else {
    angle <- all_angles[i]
  }

  # No need to rotate contour data when angle is straight
  straight <- abs(angle - c(-1, -0.5, 0, 0.5, 1) * pi) < sqrt(.Machine$double.eps)
  if (any(straight)) {
    return(0)
  }
  angle
}

rotate_xy <- function(x, y, angle) {
  # Skip rotation if angle was straight
  if (angle == 0) {
    return(list(x = x, y = y))
  }
  cos <- cos(angle)
  sin <- sin(angle)
  # Using zapsmall to make `unique0` later recognise values that may have
  # rounding errors.
  list(
    x = zapsmall(cos * x - sin * y, digits = 13),
    y = zapsmall(sin * x + cos * y, digits = 13)
  )
}
