#' Fit Multiple Lavaan Model Specifications
#'
#' Fits multiple lavaan EFA/CFA models and returns fitted model objects.
#'
#' @param modelos List of model syntax strings.
#' @param data Data frame with item responses.
#' @param estimator Estimation method (e.g., "WLSMV").
#' @param rotation Rotation method for EFA (default: "oblimin").
#' @param ordered Logical, whether variables are ordered (default: TRUE).
#' @param verbose Logical, whether to print model output (default: FALSE).
#'
#' @return A list of fitted lavaan model objects.
#'
#' @export
specification_models <- function(modelos, data, estimator, rotation = "oblimin",
                                  ordered = TRUE, verbose = FALSE) {
  # Check for required package
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required but not installed.")
  }

  specification <- list()

  for (i in 1:length(modelos)) {
    fit.original = lavaan::cfa(paste0(modelos[i]),
                       data = data,
                       estimator = estimator,
                       rotation = rotation,
                       mimic = "Mplus",
                       ordered = ordered,
                       verbose = verbose)
    specification[[i]] = fit.original

    if (verbose) {
      print(fit.original)
    }
  }

  return(specification)
}
