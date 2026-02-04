#' Exploratory Factor Analysis with Bootstrap
#'
#' Performs EFA with optional bootstrap resampling for stability analysis.
#'
#' @param n_factors Number of factors to extract.
#' @param n_items Number of items in the analysis.
#' @param name_items Vector of item names or prefix.
#' @param data Data frame with item responses.
#' @param apply_threshold Logical, whether to apply loading threshold.
#' @param estimator Estimation method (default: "WLSMV").
#' @param rotation Rotation method (default: "oblimin").
#' @param exclude_items Optional vector of items to exclude.
#' @param bootstrap Logical, whether to perform bootstrap (default: FALSE).
#' @param n_bootstrap Number of bootstrap samples (default: 1000).
#' @param bootstrap_seed Seed for reproducibility (default: 123).
#'
#' @return A list with EFA results and optional bootstrap statistics.
#' @examples
#' \dontrun{
#' # Create sample data
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE),
#'   Item7 = sample(1:5, n, replace = TRUE),
#'   Item8 = sample(1:5, n, replace = TRUE),
#'   Item9 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Basic EFA without bootstrap
#' result_basic <- efa_with_bootstrap(
#'   n_factors = 3,
#'   n_items = 9,
#'   name_items = "Item",
#'   data = data,
#'   apply_threshold = TRUE,
#'   bootstrap = FALSE
#' )
#'
#' # View fit indices
#' result_basic$Bondades_Original
#'
#' # View factor loadings
#' result_basic$result_df
#'
#' # EFA with bootstrap for stability analysis
#' result_boot <- efa_with_bootstrap(
#'   n_factors = 3,
#'   n_items = 9,
#'   name_items = "Item",
#'   data = data,
#'   apply_threshold = TRUE,
#'   bootstrap = TRUE,
#'   n_bootstrap = 100,  # Use 1000 in practice
#'   bootstrap_seed = 123
#' )
#'
#' # View bootstrap summary of fit indices
#' result_boot$Bootstrap$fit_indices_summary$summary
#'
#' # View bootstrap summary of factor loadings
#' result_boot$Bootstrap$loadings_summary
#'
#' # View global loadings summary
#' result_boot$Bootstrap$global_loadings_summary
#' }
#' @export
efa_with_bootstrap <- function(n_factors,
                               n_items,
                               name_items,
                               data,
                               apply_threshold,
                               estimator = "WLSMV",
                               rotation = "oblimin",
                               exclude_items = NULL,
                               bootstrap = FALSE,
                               n_bootstrap = 1000,
                               bootstrap_seed = 123) {

  # Funcion auxiliar interna para realizar un analisis EFA individual
  run_single_efa <- function(data_sample) {
    tryCatch({
      # Generar modelo para lavaan exploratorio
      modelos <- generate_modelos(n_factors = n_factors, n_items = n_items,
                                  name_items = name_items, exclude_items = exclude_items)
      # Especificacion para lavaan exploratorio con rotacion
      specifications <- specification_models(modelos, data = data_sample,
                                             estimator = estimator, rotation = rotation)
      # Bondades de ajuste
      bondades <- extract_fit_measures(specifications)
      # Matriz patron
      loadings <- Standardized_solutions(specifications[[n_factors]],
                                         name_items = name_items,
                                         apply_threshold = apply_threshold)

      # Extraer cargas factoriales usando lavaan::standardizedsolution
      loadings_raw <- lavaan::standardizedsolution(specifications[[n_factors]]) %>%
        dplyr::filter(op == "=~") %>%
        dplyr::select(lhs, rhs, est.std)

      # Correlacion entre factores
      interfactor <- lavaan::inspect(specifications[[n_factors]], what = "std")$psi

      return(list(
        bondades = bondades,
        loadings = loadings,
        loadings_raw = loadings_raw,
        interfactor = interfactor,
        success = TRUE
      ))
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  }

  # Funcion auxiliar interna para procesar indices de bondad de ajuste del bootstrap
  process_bootstrap_fit_indices <- function(bootstrap_bondades, n_factors) {
    # Extraer nombres de indices de ajuste
    fit_names <- names(bootstrap_bondades[[1]])

    # Filtrar columnas problematicas (como "Factores" que genera NA)
    # Excluir columnas que no son numericas o que generan todos NA
    valid_indices <- c()
    for (name in fit_names) {
      # Verificar si la columna contiene valores numericos validos
      test_values <- sapply(bootstrap_bondades[1:min(5, length(bootstrap_bondades))], function(x) x[[name]])
      if (!all(is.na(test_values)) && is.numeric(test_values)) {
        valid_indices <- c(valid_indices, name)
      }
    }

    # Crear matriz solo con indices validos
    fit_matrix <- matrix(NA, nrow = length(bootstrap_bondades),
                         ncol = length(valid_indices))
    colnames(fit_matrix) <- valid_indices

    for (i in seq_along(bootstrap_bondades)) {
      df <- bootstrap_bondades[[i]]
      target_row <- df[df$Factores == paste0("f", n_factors), ]
      if (nrow(target_row) == 0) target_row <- df[min(n_factors, nrow(df)), ]
      for (j in seq_along(valid_indices)) {
        fit_matrix[i, j] <- as.numeric(target_row[[valid_indices[j]]])
      }
    }

    # Calcular estadisticas descriptivas solo para indices validos
    fit_summary <- data.frame(
      Indice = valid_indices,
      Media = apply(fit_matrix, 2, mean, na.rm = TRUE),
      Minimo = apply(fit_matrix, 2, min, na.rm = TRUE),
      Maximo = apply(fit_matrix, 2, max, na.rm = TRUE),
      Mediana = apply(fit_matrix, 2, median, na.rm = TRUE),
      SD = apply(fit_matrix, 2, sd, na.rm = TRUE),
      Q25 = apply(fit_matrix, 2, quantile, 0.25, na.rm = TRUE),
      Q75 = apply(fit_matrix, 2, quantile, 0.75, na.rm = TRUE),
      IC_Inf_95 = apply(fit_matrix, 2, quantile, 0.025, na.rm = TRUE),
      IC_Sup_95 = apply(fit_matrix, 2, quantile, 0.975, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    return(list(
      summary = fit_summary,
      raw_matrix = fit_matrix
    ))
  }

  # Funcion auxiliar interna para procesar cargas factoriales del bootstrap
  process_bootstrap_loadings <- function(bootstrap_loadings, name_items) {
    # Identificar estructura comun de cargas
    example_loadings <- bootstrap_loadings[[1]]
    factor_names <- unique(example_loadings$Factor)

    # Crear matriz para almacenar todas las cargas
    loadings_list <- list()

    for (factor in factor_names) {
      factor_loadings <- matrix(NA, nrow = length(bootstrap_loadings), ncol = length(name_items))
      colnames(factor_loadings) <- name_items

      for (i in seq_along(bootstrap_loadings)) {
        factor_data <- bootstrap_loadings[[i]][bootstrap_loadings[[i]]$Factor == factor, ]
        for (item in name_items) {
          item_row <- factor_data[factor_data$Item == item, ]
          if (nrow(item_row) > 0) {
            factor_loadings[i, item] <- item_row$Carga[1]
          }
        }
      }

      # Calcular estadisticas para este factor
      factor_summary <- data.frame(
        Item = name_items,
        Factor = factor,
        Media = apply(factor_loadings, 2, mean, na.rm = TRUE),
        Mediana = apply(factor_loadings, 2, median, na.rm = TRUE),
        SD = apply(factor_loadings, 2, sd, na.rm = TRUE),
        IC_Inf_95 = apply(factor_loadings, 2, quantile, 0.025, na.rm = TRUE),
        IC_Sup_95 = apply(factor_loadings, 2, quantile, 0.975, na.rm = TRUE),
        Prop_Significativo = apply(abs(factor_loadings) > 0.3, 2, mean, na.rm = TRUE),
        stringsAsFactors = FALSE
      )

      loadings_list[[factor]] <- list(
        summary = factor_summary,
        raw_matrix = factor_loadings
      )
    }

    return(loadings_list)
  }

  # Funcion auxiliar interna para crear resumen global de cargas factoriales
  create_global_loadings_summary <- function(results_bootstrap) {
    # Extrae todos los remuestreos
    loadings_list <- results_bootstrap$raw_bootstrap_data$loadings

    if (is.null(loadings_list) || length(loadings_list) == 0) {
      return(NULL)
    }

    # Combinalos en un solo data.frame, agregando .id = "bootstrap"
    boot_df <- dplyr::bind_rows(loadings_list, .id = "bootstrap")

    # Obtener nombres de columnas de factores (excluyendo Items y bootstrap)
    factor_cols <- setdiff(names(boot_df), c("Items", "bootstrap"))

    # Si no hay factores, retornar NULL
    if (length(factor_cols) == 0) {
      return(NULL)
    }

    # Calcular estadisticos descriptivos por item para cada factor
    results_list <- list()

    for (factor_col in factor_cols) {
      stats_items <- boot_df %>%
        dplyr::group_by(Items) %>%
        dplyr::summarise(
          Factor = factor_col,
          n_boot   = dplyr::n(),
          media    = mean(.data[[factor_col]], na.rm = TRUE),
          sd       = sd(.data[[factor_col]], na.rm = TRUE),
          minimo   = min(.data[[factor_col]], na.rm = TRUE),
          p25      = stats::quantile(.data[[factor_col]], 0.25, na.rm = TRUE),
          mediana  = stats::median(.data[[factor_col]], na.rm = TRUE),
          p75      = stats::quantile(.data[[factor_col]], 0.75, na.rm = TRUE),
          maximo   = max(.data[[factor_col]], na.rm = TRUE),
          .groups = "drop"
        )

      results_list[[factor_col]] <- stats_items
    }

    # Combinar todos los factores en un solo data.frame
    global_summary <- dplyr::bind_rows(results_list)

    return(global_summary)
  }

  # Funcion auxiliar interna para crear resumen global de correlaciones interfactoriales
  create_global_interfactor_summary <- function(results_bootstrap) {
    # Extrae todos los remuestreos de interfactor
    inter_list <- results_bootstrap$raw_bootstrap_data$interfactor

    if (is.null(inter_list) || length(inter_list) == 0) {
      return(NULL)
    }

    # Verificar si hay matrices validas
    valid_matrices <- purrr::map_lgl(inter_list, ~ {
      is.matrix(.x) && nrow(.x) >= 1 && ncol(.x) >= 1
    })

    if (!any(valid_matrices)) {
      return(NULL)
    }

    # Filtrar solo matrices validas
    inter_list_valid <- inter_list[valid_matrices]

    # Convierte cada matriz en un data.frame largo con columnas: factor1, factor2, corr
    inter_df <- purrr::map2_dfr(inter_list_valid, seq_along(inter_list_valid), ~ {
      mat   <- .x
      boot  <- .y
      as.data.frame(mat) %>%
        tibble::rownames_to_column("factor1") %>%
        tidyr::pivot_longer(-factor1, names_to = "factor2", values_to = "corr") %>%
        dplyr::mutate(bootstrap = boot)
      # NO filtrar diagonal - incluir todas las correlaciones
    })

    if (nrow(inter_df) == 0) {
      return(NULL)
    }

    # Agrupa por par unico de factores (ordena factor1/factor2) y resume
    stats_interfactor <- inter_df %>%
      dplyr::mutate(
        pair1 = pmin(factor1, factor2),
        pair2 = pmax(factor1, factor2)
      ) %>%
      dplyr::group_by(pair1, pair2) %>%
      dplyr::summarise(
        n_boot   = dplyr::n(),
        media    = mean(corr, na.rm = TRUE),
        sd       = sd(corr, na.rm = TRUE),
        minimo   = min(corr, na.rm = TRUE),
        p25      = stats::quantile(corr, 0.25, na.rm = TRUE),
        mediana  = stats::median(corr, na.rm = TRUE),
        p75      = stats::quantile(corr, 0.75, na.rm = TRUE),
        maximo   = max(corr, na.rm = TRUE),
        .groups  = "drop"
      )

    return(stats_interfactor)
  }
  process_bootstrap_interfactor <- function(bootstrap_interfactor, n_factors) {
    if (n_factors <= 1) {
      return(NULL)
    }

    # Crear array para almacenar todas las matrices de correlacion
    n_iterations <- length(bootstrap_interfactor)
    correlation_array <- array(NA, dim = c(n_factors, n_factors, n_iterations))

    for (i in seq_along(bootstrap_interfactor)) {
      if (is.matrix(bootstrap_interfactor[[i]]) &&
          nrow(bootstrap_interfactor[[i]]) == n_factors &&
          ncol(bootstrap_interfactor[[i]]) == n_factors) {
        correlation_array[, , i] <- bootstrap_interfactor[[i]]
      }
    }

    # Calcular estadisticas descriptivas para cada correlacion
    correlation_summary <- array(NA, dim = c(n_factors, n_factors, 6))
    dimnames(correlation_summary)[[3]] <- c("Media", "Mediana", "SD", "Q25", "Q75", "IC_95_Amplitud")

    for (i in 1:n_factors) {
      for (j in 1:n_factors) {
        if (i != j) {
          correlations <- correlation_array[i, j, ]
          correlations <- correlations[!is.na(correlations)]

          if (length(correlations) > 0) {
            correlation_summary[i, j, "Media"] <- mean(correlations)
            correlation_summary[i, j, "Mediana"] <- stats::median(correlations)
            correlation_summary[i, j, "SD"] <- stats::sd(correlations)
            correlation_summary[i, j, "Q25"] <- stats::quantile(correlations, 0.25)
            correlation_summary[i, j, "Q75"] <- stats::quantile(correlations, 0.75)
            ic_range <- stats::quantile(correlations, 0.975) - stats::quantile(correlations, 0.025)
            correlation_summary[i, j, "IC_95_Amplitud"] <- ic_range
          }
        }
      }
    }

    return(list(
      summary = correlation_summary,
      raw_array = correlation_array
    ))
  }

  # Analisis original con datos completos
  cat("Realizando analisis factorial exploratorio original...\n")
  original_results <- run_single_efa(data)

  if (!original_results$success) {
    stop("Error en el analisis original: ", original_results$error)
  }

  # Generar especificaciones originales para retornar
  modelos_original <- generate_modelos(n_factors = n_factors, n_items = n_items,
                                       name_items = name_items, exclude_items = exclude_items)
  specifications_original <- specification_models(modelos_original, data = data,
                                                  estimator = estimator, rotation = rotation)

  # Inicializar resultados
  results <- list(
    Bondades_Original = original_results$bondades,
    Specifications = specifications_original,
    InterFactor = original_results$interfactor,
    result_df = original_results$loadings
  )

  # Realizar bootstrap si se solicita
  if (bootstrap) {
    cat("Iniciando analisis de bootstrap con", n_bootstrap, "muestras...\n")

    # Configurar semilla para reproducibilidad
    if (!is.null(bootstrap_seed)) {
      set.seed(bootstrap_seed)
    }

    # Inicializar contenedores para resultados de bootstrap
    bootstrap_bondades <- list()
    bootstrap_loadings <- list()
    bootstrap_loadings_raw <- list()
    bootstrap_interfactor <- list()
    successful_iterations <- 0

    # Realizar bootstrap
    pb <- utils::txtProgressBar(min = 0, max = n_bootstrap, style = 3)

    for (i in 1:n_bootstrap) {
      # Generar muestra bootstrap
      bootstrap_indices <- sample(nrow(data), nrow(data), replace = TRUE)
      bootstrap_data <- data[bootstrap_indices, ]

      # Realizar analisis en muestra bootstrap
      bootstrap_result <- run_single_efa(bootstrap_data)

      if (bootstrap_result$success) {
        successful_iterations <- successful_iterations + 1
        bootstrap_bondades[[successful_iterations]] <- bootstrap_result$bondades
        bootstrap_loadings[[successful_iterations]] <- bootstrap_result$loadings
        bootstrap_loadings_raw[[successful_iterations]] <- bootstrap_result$loadings_raw
        bootstrap_interfactor[[successful_iterations]] <- bootstrap_result$interfactor
      }

      utils::setTxtProgressBar(pb, i)
    }
    close(pb)

    cat("\nBootstrap completado:", successful_iterations, "de", n_bootstrap, "iteraciones exitosas\n")

    if (successful_iterations > 0) {
      # Procesar resultados de bondades de ajuste
      bootstrap_fit_summary <- process_bootstrap_fit_indices(bootstrap_bondades,
                                                             n_factors)

      # Procesar resultados de cargas factoriales usando los datos correctos
      bootstrap_loadings_summary <- process_bootstrap_loadings(bootstrap_loadings, name_items)

      # Procesar correlaciones entre factores
      bootstrap_interfactor_summary <- process_bootstrap_interfactor(bootstrap_interfactor, n_factors)

      # Crear resumenes globales usando las nuevas funciones eficientes
      global_loadings_summary <- create_global_loadings_summary(results)
      global_interfactor_summary <- create_global_interfactor_summary(results)

      # Agregar resultados de bootstrap a la lista de retorno
      results$Bootstrap <- list(
        n_bootstrap_successful = successful_iterations,
        fit_indices_summary = bootstrap_fit_summary,
        loadings_summary = bootstrap_loadings_summary,
        interfactor_summary = bootstrap_interfactor_summary,
        global_loadings_summary = global_loadings_summary,
        global_interfactor_summary = global_interfactor_summary,
        raw_bootstrap_data = list(
          bondades = bootstrap_bondades,
          loadings = bootstrap_loadings,
          interfactor = bootstrap_interfactor
        )
      )

      # Crear resumenes globales despues de que Bootstrap este completo
      results$Bootstrap$global_loadings_summary <- create_global_loadings_summary(results$Bootstrap)
      results$Bootstrap$global_interfactor_summary <- create_global_interfactor_summary(results$Bootstrap)
    } else {
      warning("No se pudieron completar iteraciones de bootstrap exitosas")
    }
  }

  return(results)
}
