
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

scale_x <- function(name = waiver(), ...) {
  scale_partial("x", name, ..., call = caller_call() %||% current_call())
}

scale_y <- function(name = waiver(), ...) {
  scale_partial("y", name, ..., call = caller_call() %||% current_call())
}

