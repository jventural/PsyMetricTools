#' Bootstrap EFA (Exploratory Factor Analysis)
#'
#' Performs bootstrap resampling for Exploratory Factor Analysis using lavaan,
#' calculating fit measures, factor loadings, and McDonald's omega reliability
#' for each replication.
#'
#' @param data A data frame containing the item responses.
#' @param n_factors Number of factors to extract.
#' @param n_items Total number of items in the scale.
#' @param name_items Prefix of item names (e.g., "PBA" for PBA1, PBA2, etc.).
#' @param exclude_items Character vector of item names to exclude (default: NULL).
#' @param rotation Rotation method (default: "oblimin").
#' @param estimator Estimator to use (default: "WLSMV").
#' @param apply_threshold Whether to apply threshold to loadings (default: FALSE).
#' @param seed Random seed for reproducibility (default: 2023).
#' @param n_replications Number of bootstrap replications (default: 1000).
#'
#' @return A list containing:
#' \itemize{
#'   \item Replicaciones: Data frame with all bootstrap results
#'   \item fit_summary: Summary statistics of fit measures
#'   \item loadings_summary: Summary statistics of factor loadings
#'   \item n_converged: Number of converged models
#'   \item n_valid: Number of valid models
#'   \item parameters: List of parameters used
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' results <- boot_efa(
#'   data = my_data,
#'   n_factors = 3,
#'   n_items = 23,
#'   name_items = "PBA",
#'   exclude_items = c("PBA13", "PBA23"),
#'   rotation = "oblimin",
#'   n_replications = 1000
#' )
#' }
boot_efa <- function(data,
                     n_factors,
                     n_items,
                     name_items,
                     exclude_items = NULL,
                     rotation = "oblimin",
                     estimator = "WLSMV",
                     apply_threshold = FALSE,
                     seed = 2023,
                     n_replications = 1000) {

  # Funcion interna para identificar el tipo de estimador

lavaan_estimator <- function(x) {
    if (lavaan::lavInspect(x, "options")$estimator == "DWLS") {
      if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
          lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
        estimator <- "WLSM"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
                 lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
        estimator <- "WLSMVS"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
                 lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
        estimator <- "WLSMV"
      } else if (lavaan::lavInspect(x, "options")$se == "standard" &
                 lavaan::lavInspect(x, "options")$test == "standard") {
        estimator <- "DWLS"
      } else {
        estimator <- "DWLS_variant"
      }
    } else if (lavaan::lavInspect(x, "options")$estimator == "ULS") {
      if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
          lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
        estimator <- "ULSM"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
                 lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
        estimator <- "ULSMVS"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
                 lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
        estimator <- "ULSMV"
      } else if (lavaan::lavInspect(x, "options")$se == "standard" &
                 lavaan::lavInspect(x, "options")$test == "standard") {
        estimator <- "ULS"
      } else {
        estimator <- "ULS_variant"
      }
    } else if (lavaan::lavInspect(x, "options")$estimator == "ML") {
      if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
          lavaan::lavInspect(x, "options")$test == "satorra.bentler") {
        estimator <- "MLM"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.huber.white" &
                 lavaan::lavInspect(x, "options")$test %in% c(
                   "yuan.bentler.mplus",
                   "yuan.bentler"
                 )) {
        estimator <- "MLR"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
                 lavaan::lavInspect(x, "options")$test == "mean.var.adjusted") {
        estimator <- "MLMVS"
      } else if (lavaan::lavInspect(x, "options")$se == "robust.sem" &
                 lavaan::lavInspect(x, "options")$test == "scaled.shifted") {
        estimator <- "MLMV"
      } else if (lavaan::lavInspect(x, "options")$se == "standard" &
                 lavaan::lavInspect(x, "options")$test == "standard" &
                 unique(lavaan::lavInspect(x, "options")$information) == "expected") {
        estimator <- "ML"
      } else if (lavaan::lavInspect(x, "options")$se == "standard" &
                 lavaan::lavInspect(x, "options")$test == "standard" &
                 unique(lavaan::lavInspect(x, "options")$information) == "first.order") {
        estimator <- "MLF"
      } else {
        estimator <- "ML_variant"
      }
    } else {
      estimator <- lavaan::lavInspect(x, "options")$estimator
    }

    return(estimator)
  }

  # Funcion interna para verificar si es estimador robusto
  is_robust_estimator_lavaan <- function(x) {
    if (lavaan::lavInspect(x, "options")$test %in% c(
      "satorra.bentler",
      "yuan.bentler",
      "yuan.bentler.mplus",
      "mean.var.adjusted",
      "scaled.shifted"
    )) {
      type <- "robust"
    } else {
      type <- "non-robust"
    }
    return(type)
  }

  # Funcion interna para extraer medidas de ajuste
  fit_lavaan <- function(x) {
    type <- is_robust_estimator_lavaan(x)

    if (lavaan::lavInspect(x, "converged")) {
      if (type == "robust") {
        fit_measure <- x %>%
          lavaan::fitmeasures(
            fit.measures =
              c(
                "npar",
                "chisq.scaled",
                "df.scaled",
                "pvalue.scaled",
                "rmsea.scaled",
                "rmsea.ci.lower.scaled",
                "rmsea.ci.upper.scaled",
                "srmr",
                "wrmr",
                "crmr",
                "tli.scaled",
                "cfi.scaled"
              )
          ) %>%
          tibble::enframe(name = "term") %>%
          tidyr::pivot_wider(
            names_from = term,
            values_from = value
          ) %>%
          dplyr::mutate(
            dplyr::across(dplyr::everything(), as.numeric)
          ) %>%
          dplyr::bind_cols(
            tibble::tibble(
              converged = lavaan::lavInspect(x, "converged"),
              estimator = lavaan_estimator(x),
              ngroups = lavaan::lavInspect(x, "ngroups"),
              nobs = sum(lavaan::lavInspect(x, "nobs"))
            )
          ) %>%
          dplyr::select(
            nobs, estimator, ngroups, converged, chisq.scaled,
            df.scaled, pvalue.scaled, npar, cfi.scaled, tli.scaled,
            rmsea.scaled, rmsea.ci.lower.scaled, rmsea.ci.upper.scaled,
            srmr, wrmr, crmr
          ) %>%
          dplyr::rename_with(
            ~ stringr::str_remove(., ".scaled")
          )
      } else {
        fit_measure <- x %>%
          lavaan::fitmeasures(
            fit.measures =
              c(
                "npar",
                "chisq",
                "df",
                "pvalue",
                "rmsea",
                "rmsea.ci.lower",
                "rmsea.ci.upper",
                "srmr",
                "wrmr",
                "crmr",
                "aic",
                "bic",
                "tli",
                "cfi"
              )
          ) %>%
          tibble::enframe(name = "term") %>%
          tidyr::pivot_wider(
            id_cols = term,
            names_from = term,
            values_from = value
          ) %>%
          dplyr::mutate(
            dplyr::across(dplyr::everything(), as.numeric)
          ) %>%
          dplyr::bind_cols(
            tibble::tibble(
              converged = lavaan::lavInspect(x, "converged"),
              estimator = lavaan::lavInspect(x, "options")$estimator,
              ngroups = lavaan::lavInspect(x, "ngroups"),
              missing_method = lavaan::lavInspect(x, "options")$missing,
              nobs = sum(lavaan::lavInspect(x, "nobs"))
            )
          ) %>%
          dplyr::relocate(
            nobs, estimator, ngroups, converged,
            chisq, df, pvalue, npar, cfi, tli, rmsea,
            rmsea.ci.lower, rmsea.ci.upper, srmr, wrmr, crmr,
            aic, bic
          ) %>%
          dplyr::rename(
            AIC = aic,
            BIC = bic
          )
      }
    } else {
      fit_measure <- tibble::tibble(
        nobs = sum(lavaan::lavInspect(x, "nobs")),
        estimator = lavaan_estimator(x),
        ngroups = lavaan::lavInspect(x, "ngroups"),
        converged = lavaan::lavInspect(x, "converged"),
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        nparameter = NA_real_,
        cfi = NA_real_,
        tli = NA_real_,
        rmsea = NA_real_,
        rmsea.ci.lower = NA_real_,
        rmsea.ci.upper = NA_real_,
        srmr = NA_real_,
        wrmr = NA_real_,
        crmr = NA_real_
      )
    }

    fit_measure <- fit_measure %>%
      dplyr::rename_with(
        .fn = ~ stringr::str_to_upper(.),
        .cols = dplyr::any_of(c("cfi", "tli", "rmsea", "rmsea.ci.lower",
                                "rmsea.ci.upper", "srmr", "wrmr", "crmr"))
      )

    return(fit_measure)
  }

  # Funcion interna para extraer cargas factoriales estandarizadas
  extract_loadings <- function(fit_model, n_factors, name_items) {
    tryCatch({
      if (!lavaan::lavInspect(fit_model, "converged")) {
        return(NULL)
      }

      std_solution <- lavaan::standardizedSolution(fit_model)
      loadings <- std_solution %>%
        dplyr::filter(op == "=~") %>%
        dplyr::select(lhs, rhs, est.std) %>%
        tidyr::pivot_wider(
          names_from = lhs,
          values_from = est.std
        ) %>%
        dplyr::rename(item = rhs)

      return(loadings)
    }, error = function(e) {
      return(NULL)
    })
  }

  # Funcion interna para extraer correlaciones interfactoriales
  extract_interfactor <- function(fit_model) {
    tryCatch({
      if (!lavaan::lavInspect(fit_model, "converged")) {
        return(NULL)
      }

      psi <- lavaan::inspect(fit_model, what = "std")$psi
      return(psi)
    }, error = function(e) {
      return(NULL)
    })
  }

  # Funcion interna para calcular omega McDonald desde cargas
  calc_omega_from_loadings <- function(loadings_df, method = "sum_loadings") {
    tryCatch({
      if (is.null(loadings_df) || nrow(loadings_df) == 0) {
        return(NULL)
      }

      # Identificar columnas de factores (excluyendo 'item')
      factor_cols <- setdiff(names(loadings_df), "item")

      # Funcion extracted_items2 interna - asigna items al factor con mayor carga
      df_numerics_only <- loadings_df[, factor_cols, drop = FALSE]
      max_indices <- apply(abs(df_numerics_only), 1, which.max)

      groups <- list()
      for (i in seq_along(max_indices)) {
        column <- factor_cols[max_indices[i]]
        item <- loadings_df$item[i]
        if (is.null(groups[[column]])) {
          groups[[column]] <- item
        } else {
          groups[[column]] <- c(groups[[column]], item)
        }
      }

      # Calcular omega para cada factor
      omega_results <- numeric(length(groups))
      names(omega_results) <- names(groups)

      for (g in names(groups)) {
        items_vec <- groups[[g]]
        sub_df <- loadings_df[loadings_df$item %in% items_vec, , drop = FALSE]

        if (nrow(sub_df) == 0) next

        mat <- as.matrix(sub_df[, factor_cols, drop = FALSE])
        gen_loading <- rowSums(mat)

        if (method == "comunalidad") {
          h2 <- gen_loading^2
        } else {
          h2 <- gen_loading
        }
        theta <- 1 - h2

        omega_total <- sum(h2) / (sum(h2) + sum(theta))
        omega_results[g] <- omega_total
      }

      return(as.list(omega_results))
    }, error = function(e) {
      return(NULL)
    })
  }

  set.seed(seed)

  # Generar el modelo base para EFA
  modelos <- generate_modelos(
    n_factors = n_factors,
    n_items = n_items,
    name_items = name_items,
    exclude_items = exclude_items
  )

  # Obtener nombres de items a usar
  all_items <- paste0(name_items, 1:n_items)
  if (!is.null(exclude_items)) {
    items_to_use <- setdiff(all_items, exclude_items)
  } else {
    items_to_use <- all_items
  }

  # Replicar el dataframe con reemplazo
  message("Generando ", n_replications, " muestras bootstrap...")

  boot_samples <- purrr::map_dfr(
    seq_len(n_replications),
    ~ data %>%
      dplyr::sample_n(size = nrow(data), replace = TRUE),
    .id = "obs"
  )

  # Mensaje antes de calcular EFA
  message("Calculando EFA con bootstrapping...")

  Replicaciones <- boot_samples %>%
    dplyr::select(dplyr::any_of(c("obs", items_to_use))) %>%
    dplyr::group_nest(obs) %>%
    dplyr::mutate(
      data = purrr::map(data, ~ dplyr::select(., tidyselect::where(~ !all(is.na(.))))),

      # Ajustar modelo EFA usando specification_models
      fit_efa = pbapply::pblapply(seq_along(data), function(i) {
        tryCatch({
          specs <- specification_models(
            modelos,
            data = data[[i]],
            estimator = estimator,
            rotation = rotation
          )
          return(specs[[n_factors]])
        }, error = function(e) {
          return(NULL)
        })
      }, cl = 1),

      # Verificar convergencia
      converged = purrr::map_lgl(fit_efa, ~ {
        if (is.null(.x)) return(FALSE)
        lavaan::lavInspect(.x, "converged")
      }),

      # Verificar validez
      validity = purrr::map_lgl(fit_efa, ~ {
        if (is.null(.x)) return(FALSE)
        tryCatch(
          suppressWarnings(lavaan::lavInspect(.x, "post.check")),
          error = function(e) FALSE
        )
      }),

      # Extraer medidas de ajuste
      fit_measures = purrr::map(fit_efa, ~ {
        if (is.null(.x)) {
          return(tibble::tibble(
            nobs = NA_integer_,
            estimator = NA_character_,
            ngroups = NA_integer_,
            converged = FALSE,
            chisq = NA_real_,
            df = NA_real_,
            pvalue = NA_real_,
            npar = NA_real_,
            CFI = NA_real_,
            TLI = NA_real_,
            RMSEA = NA_real_,
            RMSEA.CI.LOWER = NA_real_,
            RMSEA.CI.UPPER = NA_real_,
            SRMR = NA_real_,
            WRMR = NA_real_,
            CRMR = NA_real_
          ))
        }
        fit_lavaan(.x)
      }),

      # Extraer cargas factoriales
      loadings = purrr::map(fit_efa, ~ extract_loadings(.x, n_factors, name_items)),

      # Extraer correlaciones interfactoriales
      interfactor = purrr::map(fit_efa, ~ extract_interfactor(.x))
    ) %>%
    dplyr::collect()

  # Calcular omega McDonald para cada replica (despues del collect)
  message("\nCalculando fiabilidad omega...")
  Replicaciones <- Replicaciones %>%
    dplyr::mutate(
      omega = purrr::map(loadings, ~ calc_omega_from_loadings(.x, method = "sum_loadings"))
    )

  # Resumen de convergencia
  n_converged <- sum(Replicaciones$converged)
  n_valid <- sum(Replicaciones$validity)

  message("\n--- Resumen del Bootstrap EFA ---")
  message("Total de replicaciones: ", n_replications)
  message("Modelos convergidos: ", n_converged, " (", round(n_converged/n_replications*100, 1), "%)")
  message("Modelos validos: ", n_valid, " (", round(n_valid/n_replications*100, 1), "%)")

  # Crear resumen de medidas de ajuste
  fit_summary <- Replicaciones %>%
    dplyr::filter(converged == TRUE) %>%
    dplyr::select(fit_measures) %>%
    tidyr::unnest(fit_measures) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          q025 = ~ quantile(.x, 0.025, na.rm = TRUE),
          q975 = ~ quantile(.x, 0.975, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )

  # Crear resumen de cargas factoriales
  loadings_list <- Replicaciones %>%
    dplyr::filter(converged == TRUE) %>%
    dplyr::pull(loadings)

  loadings_list <- loadings_list[!sapply(loadings_list, is.null)]

  if (length(loadings_list) > 0) {
    loadings_summary <- dplyr::bind_rows(loadings_list, .id = "rep") %>%
      dplyr::group_by(item) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::where(is.numeric),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            median = ~ median(.x, na.rm = TRUE),
            q025 = ~ quantile(.x, 0.025, na.rm = TRUE),
            q975 = ~ quantile(.x, 0.975, na.rm = TRUE)
          ),
          .names = "{.col}_{.fn}"
        )
      )
  } else {
    loadings_summary <- NULL
  }

  return(list(
    Replicaciones = Replicaciones,
    fit_summary = fit_summary,
    loadings_summary = loadings_summary,
    n_converged = n_converged,
    n_valid = n_valid,
    parameters = list(
      n_factors = n_factors,
      n_items = n_items,
      name_items = name_items,
      exclude_items = exclude_items,
      rotation = rotation,
      estimator = estimator,
      n_replications = n_replications,
      seed = seed
    )
  ))
}
