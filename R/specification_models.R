#' @name specification_models
#' @export
specification_models <- function(modelos, data, estimator, rotation = "oblimin", verbose = FALSE) {
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
                       ordered = TRUE,
                       verbose = verbose)  # Usar el argumento verbose aquí
    specification[[i]] = fit.original

    if (verbose) {  # Si verbose es TRUE, imprimir el modelo
      print(fit.original)
    }
  }

  return(specification)
}
