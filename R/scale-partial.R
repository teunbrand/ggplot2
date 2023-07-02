
scale_partial <- function(aesthetic, name = waiver(), ..., .args = NULL) {
  aesthetic <- standardise_aes_names(aesthetic)

  args <- .args %||% list2(name = name, ...)
  args <- args[!vapply(args, is.waive, logical(1))]

  lambdas <- intersect(
    names(args),
    c("limits", "breaks", "labels", "rescaler", "oob", "minor_breaks")
  )
  args[lambdas] <- lapply(args[lambdas], allow_lambda)

  ggproto(
    NULL, ScalePartial,
    aesthetics = aesthetic,
    params = args
  )
}

ScalePartial <- ggproto(
  "ScalePartial", Scale,

  aesthetics = character(),

  params = list(),

  update_params = function(self, params, default = FALSE) {
    self$params <- defaults(params, self$params)
  },

  clone = function(self) {
    ggproto(NULL, self)
  }
)

scale_x <- function(name = waiver(), ...) {
  scale_partial("x", name, ...)
}

scale_y <- function(name = waiver(), ...) {
  scale_partial("y", name, ...)
}

