iterativeModelEFA <- function(data, initialModel, MAX_ITERATIONS = 10) {
  #function internal
  specification_models_internal <- function(modelos, data, estimator) {
    # Función para instalar y cargar librerías
    install_and_load <- function(package) {
      if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
      }
      library(package, character.only = TRUE)
    }

    # Instalar y cargar la librería requerida
    install_and_load("lavaan")

    AD <- list()

    for (i in 1:length(modelos)) {
      # Intentar ajustar el modelo y capturar errores y advertencias
      fit.original <- tryCatch({
        cfa(paste0(modelos[i]),
            data = data,
            estimator = estimator,
            rotation = "oblimin",
            mimic = "Mplus",
            ordered = TRUE, verbose = FALSE)
      }, warning = function(w) {
        # Manejar advertencias aquí si es necesario, por ejemplo, imprimirlas
        message("\nAdvertencia en el modelo ", i, ": ", w$message)
        return(NULL) # Devuelve NULL o un objeto diferente si es necesario
      }, error = function(e) {
        # Manejar errores aquí si es necesario, por ejemplo, imprimiéndolos
        message("\nError en el modelo ", i, ": ", e$message)
        return(NULL) # Devuelve NULL o un objeto diferente si es necesario
      }, finally = {
        # Código que siempre se ejecuta, éxito o error
      })

      if (!is.null(fit.original)) {
        AD[[i]] <- fit.original
      }
      # No es necesario imprimir algo específico aquí a menos que quieras
    }

    return(AD)
  }
  library(dplyr) # Asegura cargar dplyr para las funciones de manipulación de datos

  identifyItemsToExclude <- function(result_df) {
    crossLoadedItems <- result_df %>%
      rowwise() %>%
      mutate(crossLoad = sum(c_across(starts_with("f")) > 0.3) > 1) %>%
      filter(crossLoad) %>%
      pull(Items)
    return(crossLoadedItems)
  }

  excludedItems <- list()
  iteration <- 1
  allResultsDf <- list()
  finalModelInfo <- NULL

  name_items <- initialModel$name_items

  pb <- txtProgressBar(min = 0.01, max = MAX_ITERATIONS, style = 3)

  while (iteration <= MAX_ITERATIONS) {
    initialModel$exclude_items <- unique(c(initialModel$exclude_items, excludedItems))

    modelos <- generate_modelos(n_factors = initialModel$n_factors,
                                n_items = initialModel$n_items,
                                name_items = name_items,
                                exclude_items = initialModel$exclude_items)

    Specifications <- specification_models_internal(modelos, data = data, estimator = "WLSMV")
    Bondades <- extract_fit_measures(Specifications)

    foundItemsToExclude <- FALSE

    for (i in seq_along(Specifications)) {
      result_df <- Standardized_solutions(Specifications[[i]], name_items = name_items, apply_threshold = TRUE)
      key <- paste("Modelo", i, "Iteración", iteration)
      allResultsDf[[key]] <- result_df

      itemsToExclude <- identifyItemsToExclude(result_df)
      if (length(itemsToExclude) > 0) {
        foundItemsToExclude <- TRUE
        excludedItems <- unique(c(excludedItems, itemsToExclude))
        break
      } else if (!foundItemsToExclude && i == length(Specifications)) {
        finalModelInfo <- key
      }
    }

    if (!foundItemsToExclude) {
      setTxtProgressBar(pb, MAX_ITERATIONS)  # Actualiza la barra al 100% antes de terminar
      break
    }

    iteration = iteration + 1
    setTxtProgressBar(pb, iteration)
  }

  close(pb)

  if (!is.null(finalModelInfo)) {
    message(paste("Análisis completado:", finalModelInfo, "tiene la estructura más simple."))
  } else if (iteration > MAX_ITERATIONS) {
    message("Se alcanzó el número máximo de iteraciones sin encontrar una estructura factorial completamente aceptable.")
  }

  return(list(Models = Specifications, Bondades = Bondades, ExcludedItems = excludedItems, FinalIteration = iteration, AllResultsDf = allResultsDf, FinalModelInfo = finalModelInfo))
}
