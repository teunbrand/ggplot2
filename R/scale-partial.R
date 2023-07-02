
scale_partial <- function(aesthetic, name = waiver(), ..., .args = NULL,
                          call = caller_call() %||% current_call()) {

  args <- .args %||% list2(name = name, ...)
  args <- args[!vapply(args, is.waive, logical(1))]

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

