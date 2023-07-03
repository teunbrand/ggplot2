
#' Partial scales
#'
#' Partial scales can be used to adjust scale parameters without
#' explicitly committing to a particular type of scale.
#'
#' @param name The name of the scale that can be used as guide title.
#' @param ... Other parameters passed to scales. Many of these depend on the
#'   eventual scale type, but `limits`, `breaks`, `labels` and `guide` are
#'   commonplace.
#'
#' @details
#' Parameters set via partial scales may be overridden by adding 'full' scales
#' to a plot. If no full scales are added to a plot, the parameters from
#' the partial scales are passed to the default full scale. Therefore, some
#' care must be taken to not accidentally pass a continuous scale parameter,
#' like `trans`, to discrete scales, or pass a `palette` to a position scale.
#'
#' @return A `<ScalePartial>` object that can be added to a plot.
#' @name partial-scales
#'
#' @examples
#' # TODO
NULL

#' @export
#' @rdname partial-scales
scale_x <- function(name = waiver(), ...) {
  scale_partial("x", name, ..., call = caller_call() %||% current_call())
}

#' @export
#' @rdname partial-scales
scale_y <- function(name = waiver(), ...) {
  scale_partial("y", name, ..., call = caller_call() %||% current_call())
}

#' Partial scales constructor
#'
#' Constructor for partial scales.
#'
#' @param aesthetic A string for an aesthetic, like `"x"` or `"shape"`.
#' @param name A name for a scale.
#' @param ...,.args Named arguments to pass on to the final scale. Use either
#'   `...` as dynamic dots, or `.args` as a named list.
#' @param call A `call` object for reporting the scale in messages.
#'
#' @return A `<ScalePartial>` ggproto object that can be added to a plot.
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
scale_partial <- function(aesthetic, name = waiver(), ..., .args = NULL,
                          call = caller_call() %||% current_call()) {

  # Check arguments
  args <- list2(name = name, ..., !!!.args)
  if (!is_named2(args) || vec_duplicate_any(names(args))) {
    msg <- "All arguments must be uniquely named."
    dup_names <- names(args)[duplicated(names(args))]
    dup_names <- dup_names[dup_names != "" & !is.na(dup_names)]
    if (length(dup_names) > 0) {
      msg <- c(msg, i = "Duplicated argument names: {.and {.field {dup_names}}}")
    }
    cli::cli_abort(msg, call = call)
  }
  args <- args[!vapply(args, is.waive, logical(1))]
  if (length(args) < 1) {
    return(NULL)
  }

  # Check aesthetic
  check_string(aesthetic, allow_empty = FALSE, call = call)
  aesthetic <- standardise_aes_names(aesthetic)

  lambdas <- intersect(
    names(args),
    c("limits", "breaks", "labels", "rescaler", "oob", "minor_breaks")
  )
  args[lambdas] <- lapply(args[lambdas], allow_lambda)

  ggproto(
    NULL, ScalePartial,
    call = call,
    aesthetics = aesthetic,
    params = args
  )
}

ScalePartial <- ggproto(
  "ScalePartial", Scale,

  call = NULL,

  aesthetics = character(),

  params = list(),

  update_params = function(self, params, default = FALSE, call = NULL) {
    if (length(params) < 1) {
      return()
    }
    # ScalePartial can only be updated by other ScalePartial, at which
    # point the error call becomes ambiguous, so set arguments to NULL.
    self$call[-1] <- NULL
    self$params <- defaults(params, self$params)
    return()
  },

  clone = function(self) {
    ggproto(NULL, self)
  }
)

