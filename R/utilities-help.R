# Use extra_note arg to add some notes (e.g. the document is shared with multiple
# Geoms and there's some difference among their aesthetics).
rd_aesthetics <- function(type, name, extra_note = NULL) {
  obj <- switch(type,
    geom = validate_subclass(name, "Geom", env = globalenv()),
    stat = validate_subclass(name, "Stat", env = globalenv()),
    position = validate_subclass(name, "Position", env = globalenv())
  )
  aes <- rd_aesthetics_item(obj)

  c(
    "@section Aesthetics:",
    paste0(
      "\\code{", type, "_", name, "()} ",
      "understands the following aesthetics. Required aesthetics are displayed",
      " in bold and defaults are displayed for optional aesthetics:"
    ),
    "\\tabular{rll}{", aes, "}",
    if (!is.null(extra_note)) paste0(extra_note, "\n"),
    "Learn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}."
  )
}

rd_aesthetics_item <- function(x) {
  req <- x$required_aes
  req <- gsub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
  req_aes <- unlist(strsplit(x$required_aes, "|", fixed = TRUE))
  optional_aes <- setdiff(x$aesthetics(), req_aes)
  all <- union(req, sort(optional_aes))
  docs <- rd_match_docpage(all)
  defaults <- rd_defaults(x, all)

  item <- ifelse(all %in% req,
    paste0("\\strong{\\code{", docs, "}}"),
    paste0("\\code{", docs, "}")
  )
  paste0(" \u2022 \\tab ", item, " \\tab ", defaults, " \\cr")
}

rd_defaults <- function(layer, aesthetics) {
  defaults <- layer$default_aes

  out <- rep("", length(aesthetics))

  themed <- vapply(defaults, FUN.VALUE = logical(1), function(x) {
    is_quosure(x) && quo_is_call(x, name = "from_theme")
  })
  defaults <- lapply(defaults, quo_text)
  defaults[themed] <- "via \\code{theme()}"
  defaults[!themed] <- paste0("\\code{", defaults[!themed], "}")

  i <- intersect(aesthetics, names(defaults))
  out[match(i, aesthetics)] <- defaults[i]
  empty <- !nzchar(out)
  out[!empty] <- paste0("\u2192 ", out[!empty])
  out[empty] <- " "
  out[empty & aesthetics == "group"] <- "\u2192 inferred"
  out
}

rd_match_docpage <- function(aes) {

  split <- strsplit(aes, "} \\emph{or} \\code{", fixed = TRUE)
  flat  <- unlist(split)

  index <- match(
    flat,
    c(
      "x", "y", "xmin", "xmax", "ymin", "ymax", "xend", 'yend',
      "colour", "fill", "alpha",
      "group",
      "linetype", "size", "shape", "linewidth"
    ), nomatch = 0L
  )
  docpage <- c(
    "", rep("aes_position", 8), rep("aes_colour_fill_alpha", 3),
    "aes_group_order", rep("aes_linetype_size_shape", 4)
  )[index + 1]
  no_match <- index == 0
  docpage[!no_match] <- paste0(
    "\\link[ggplot2:", docpage[!no_match],
    "]{", flat[!no_match], "}"
  )
  docpage[no_match] <- flat[no_match]
  docpage <- split(docpage, rep(seq_along(split), lengths(split, FALSE)))
  vapply(docpage, paste, character(1), collapse = "} \\emph{or} \\code{")
}

rd_orientation <- function() {
  c(
    "@section Orientation: ",
    paste(
      'This geom treats each axis differently and, thus, can thus have two orientations.',
      'Often the orientation is easy to deduce from a combination of the given',
      'mappings and the types of positional scales in use. Thus, ggplot2 will by',
      'default try to guess which orientation the layer should have. Under rare',
      'circumstances, the orientation is ambiguous and guessing may fail. In that',
      'case the orientation can be specified directly using the \\code{orientation} parameter,',
      'which can be either \\code{"x"} or \\code{"y"}. The value gives the axis that the geom',
      'should run along, \\code{"x"} being the default orientation you would expect for the geom.'
    )
  )
}

#' Format 'Computed variables' section
#'
#' This is a helper function that helps format the 'Computed variables' section
#' of stat documentation pages. Briefly, it points to the delayed evaluation
#' documentation and it wraps the variable names in
#' 'after_stat()'.
#'
#' @param ... Named variable - description pairs. Variable names get wrapped in
#'   `after_stat()`. Variable names may be of the form "x|y" or "x,y" to format
#'   as "`after_stat(x)` or `after_stat(y)`" and
#'   "`after_stat(x)`, `after_stat(y)`" respectively.
#' @param .details Additional details to include in the preamble.
#' @param .skip_intro A logical, which if `TRUE` omits the link to the delayed
#'   evaluation docs. Can be useful when documenting two stat functions on the
#'   same page with different arguments, but no need to repeat the introduction
#'   in the second stat function (see e.g. `?stat_qq`).
#'
#' @return Formatted code that can be inserted into a .rd file.
#' @keywords internal
#' @noRd
#' @examples
#' rd_computed_vars(
#'   .details = "`stat_foobar()` computes the following variables:",
#'   "foo|bar" = "foobar",
#'   "bar,qux" = "quux",
#'   corge = "grault"
#' )
rd_computed_vars <- function(..., .details = "", .skip_intro = FALSE) {
  args  <- list(...)
  items <- names(args)
  descr <- unname(args)

  # Format preamble
  header <- "@section Computed variables: "
  intro  <- paste0(
    "These are calculated by the 'stat' part of layers and can be accessed ",
    "with [delayed evaluation][aes_eval]. "
  )
  if (.skip_intro) intro <- ""
  preamble <- c(header, paste0(intro, gsub("\n", "", .details)))

  # Format items
  fmt_items <- gsub(",", ")`, `after_stat(", items, fixed = TRUE)
  fmt_items <- gsub("|", ")` *or* `after_stat(",
                    fmt_items, fixed = TRUE)
  fmt_items <- paste0("*  `after_stat(", fmt_items, ")`")

  # Compose item-list
  fmt_descr <- gsub("\n", "", descr)
  fmt_list  <- paste(fmt_items, fmt_descr, sep = "\\cr ")

  c(preamble, fmt_list)
}

link_book <- function(text = "", section = "",
                      book = "https://ggplot2-book.org/",
                      suffix = "of the online ggplot2 book.") {
  links <- paste0("[", text, "](", book, section, ")")
  if (length(links) > 1) {
    links <- oxford_comma(links, final = "and")
  }
  paste(links, suffix, sep = " ")
}

roxy_tag_parse.roxy_tag_aesthetics <- function(x) {
  x <- roxygen2::tag_two_part(x, "an argument name", "a description", required = FALSE)

  class <- get0(x$val$name, parent.frame())
  if (!inherits(class, c("Geom", "Stat", "Position"))) {
    cli::cli_abort(
      "Cannot create a {.field Aesthetics} section for {.val {x$val$name}}."
    )
  }


  fun_name <- snake_class(class)
  aes_item <- rd_aesthetics_item(class)

  x$val <- c("",
    paste0(
      "\\code{", fun_name, "()} ",
      "understands the following aesthetics. Required aesthetics are displayed",
      " in bold and defaults are displayed for optional aesthetics:"
    ),
    "\\tabular{rll}{", aes_item, "}",
    if (nzchar(x$val$description)) x$val$description
  )
  x
}

roxy_tag_rd.roxy_tag_aesthetics <- function(x, base_path, env) {
  # When we document ggplot2 itself, we don't need to prefix links with ggplot2
  if (basename(base_path) == "ggplot2") {
    x$val <- gsub("\\link[ggplot2:", "\\link[=", x$val, fixed = TRUE)
  }
  roxygen2::rd_section("aesthetics", x$val)
}

on_load({
  vctrs::s3_register(
    "roxygen2::roxy_tag_parse", "roxy_tag_aesthetics",
    roxy_tag_parse.roxy_tag_aesthetics
  )
  vctrs::s3_register(
    "roxygen2::roxy_tag_rd", "roxy_tag_aesthetics",
    roxy_tag_rd.roxy_tag_aesthetics
  )
})

#' @export
format.rd_section_aesthetics <- function(x, ...) {
  vec_c(
    "\\section{Aesthetics}{",
    !!!x$value,
    "\nLearn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}.",
    "}"
  )
}
