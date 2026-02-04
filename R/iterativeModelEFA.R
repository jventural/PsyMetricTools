#' @name iterativeModelEFA
#' @export
iterativeModelEFA <- function(data, n_factors,
                                      estimator = "WLSMV",
                                      name_items = "RAS",
                                      n_items = NULL,
                                      specific_items = NULL,
                                      rotation = "oblimin",
                                      verbose = FALSE) {
  #Internal function
  identify_multiple_loadings <- function(results) {
    multi_loadings <- apply(results[, -1], 1, function(x) sum(x != 0) > 1)
    return(results$Items[multi_loadings])
  }
  #

  if (is.null(specific_items) && !is.null(n_items)) {
    specific_items <- paste0(name_items, 1:n_items)
  } else if (is.null(specific_items)) {
    stop("Debes proporcionar 'n_items' o 'specific_items'.")
  }

  items_to_exclude <- c()
  all_specifications <- list()
  all_results <- list()
  all_fit_measures <- list()
  all_interFactors <- list()

  repeat {
    current_specific_items <- setdiff(specific_items, items_to_exclude)
    modelos <- generate_modelos(n_factors = n_factors, specific_items = current_specific_items, name_items = name_items)

    operationResult <- try({
      Specifications <- specification_models(modelos, data = data, estimator = estimator, rotation = rotation, verbose = verbose)
      results <- Standardized_solutions(Specifications[[length(Specifications)]], name_items = name_items, apply_threshold = TRUE)
      fit_measures <- extract_fit_measures(Specifications)
      interFactor <- lavaan::inspect(Specifications[[length(Specifications)]], what = "std")$psi

      TRUE  # Si se llega hasta aqui, no hubo errores.
    }, silent = TRUE)

    if (inherits(operationResult, "try-error")) {
      message("Se detecto un error durante la ejecucion. Revisando posibles causas...")
      break  # Puedes decidir que accion tomar en caso de error.
    }

    # Comprobar advertencias despues de la operacion critica.
    if (!is.null(last.warning)) {
      warnings <- sapply(last.warning, function(w) w$message)
      if (any(grepl("could not compute standard errors", warnings, ignore.case = TRUE))) {
        message("Advertencia detectada sobre errores estandar. Considerando acciones...")
        # Aqui decides que hacer en caso de esta advertencia especifica.
      }
    }

    all_specifications[[length(all_specifications) + 1]] <- Specifications
    all_results[[length(all_results) + 1]] <- results
    all_fit_measures[[length(all_fit_measures) + 1]] <- fit_measures
    all_interFactors[[length(all_interFactors) + 1]] <- interFactor

    items_multiple_loadings <- identify_multiple_loadings(results)
    if (length(items_multiple_loadings) == 0) {
      break
    } else {
      items_to_exclude <- c(items_to_exclude, items_multiple_loadings)
    }
  }

  final_specific_items <- setdiff(specific_items, items_to_exclude)

  return(list(final_model = Specifications[[length(Specifications)]],
              final_items = final_specific_items,
              excluded_items = items_to_exclude,
              all_specifications = all_specifications,
              all_results = all_results,
              all_fit_measures = all_fit_measures,
              all_interFactors = all_interFactors))
}
