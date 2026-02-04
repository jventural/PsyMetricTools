#' @title Bootstrap Confirmatory Factor Analysis
#' @description Performs bootstrap CFA with reliability estimation.
#' @param new_df Data frame with the data.
#' @param model_string Lavaan model specification string.
#' @param item_prefix Prefix for item column names.
#' @param seed Random seed (default 2023).
#' @param n_replications Number of bootstrap replications (default 1000).
#' @param ordered Logical indicating if variables are ordinal (default TRUE).
#' @param estimator Estimator to use (default "WLSMV").
#' @return Data frame with bootstrap results including fit measures and reliability.
#' @examples
#' \dontrun{
#' # Create sample data with 9 Likert-type items (3 factors, 3 items each)
#' set.seed(123)
#' n <- 300
#' data_cfa <- data.frame(
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
#' # Define the CFA model
#' model <- "
#'   F1 =~ Item1 + Item2 + Item3
#'   F2 =~ Item4 + Item5 + Item6
#'   F3 =~ Item7 + Item8 + Item9
#' "
#'
#' # Perform bootstrap CFA with 100 replications (use more in practice)
#' result <- boot_cfa(
#'   new_df = data_cfa,
#'   model_string = model,
#'   item_prefix = "Item",
#'   seed = 2023,
#'   n_replications = 100,
#'   ordered = TRUE,
#'   estimator = "WLSMV"
#' )
#'
#' # Access fit measures
#' result$fit_measures1
#'
#' # Check convergence rates
#' mean(result$converged1)
#'
#' # Access reliability estimates (omega)
#' result[, c("Rel1", "Rel2", "Rel3")]
#' }
#' @export
boot_cfa <- function(new_df, model_string, item_prefix, seed = 2023, n_replications = 1000, ordered = TRUE, estimator = "WLSMV") {
  # Funcion interna
  lavaan_estimator <- function(x) {
    opts <- lavaan::lavInspect(x, "options")
    test_val <- opts$test[1]
    se_val <- opts$se
    est_val <- opts$estimator

    if (est_val == "DWLS") {
      if (se_val == "robust.sem" && test_val == "satorra.bentler") {
        est <- "WLSM"
      } else if (se_val == "robust.sem" && test_val == "mean.var.adjusted") {
        est <- "WLSMVS"
      } else if (se_val == "robust.sem" && test_val == "scaled.shifted") {
        est <- "WLSMV"
      } else if (se_val == "standard" && test_val == "standard") {
        est <- "DWLS"
      } else {
        est <- "DWLS_variant"
      }
    } else if (est_val == "ULS") {
      if (se_val == "robust.sem" &
          test_val == "satorra.bentler") {
        est <- "ULSM"
      } else if (se_val == "robust.sem" &
                 test_val == "mean.var.adjusted") {
        est <- "ULSMVS"
      } else if (se_val == "robust.sem" &
                 test_val == "scaled.shifted") {
        est <- "ULSMV"
      } else if (se_val == "standard" &
                 test_val == "standard") {
        est <- "ULS"
      } else {
        est <- "ULS_variant"
      }
    } else if (est_val == "ML") {
      if (se_val == "robust.sem" && test_val == "satorra.bentler") {
        est <- "MLM"
      } else if (se_val == "robust.huber.white" &&
                 test_val %in% c("yuan.bentler.mplus", "yuan.bentler")) {
        est <- "MLR"
      } else if (se_val == "robust.sem" && test_val == "mean.var.adjusted") {
        est <- "MLMVS"
      } else if (se_val == "robust.sem" && test_val == "scaled.shifted") {
        est <- "MLMV"
      } else if (se_val == "standard" && test_val == "standard" &&
                 unique(opts$information)[1] == "expected") {
        est <- "ML"
      } else if (se_val == "standard" && test_val == "standard" &&
                 unique(opts$information)[1] == "first.order") {
        est <- "MLF"
      } else {
        est <- "ML_variant"
      }
    } else {
      est <- est_val
    }

    return(est)
  }

  # Funcion interna
  is_robust_estimator_lavaan <- function(x) {
    test_val <- lavaan::lavInspect(x, "options")[["test"]][1]
    if (test_val %in% c(
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

  # Funcion interna
  fit_lavaan <- function(x) {
    type <- is_robust_estimator_lavaan(x)

    if (lavaan::lavInspect(x, "converged")) {
      if (type == "robust") {
        fit_measure <- lavaan::fitmeasures(x,
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
          )
        fit_measure <- tibble::enframe(fit_measure, name = "term")
        fit_measure <- tidyr::pivot_wider(fit_measure,
            names_from = term,
            values_from = value
          )
        fit_measure <- dplyr::mutate(fit_measure,
            dplyr::across(dplyr::everything(), as.numeric)
          )
        fit_measure <- dplyr::bind_cols(fit_measure,
            tibble::tibble(
              converged = lavaan::lavInspect(x, "converged"),
              estimator = lavaan_estimator(x),
              ngroups = lavaan::lavInspect(x, "ngroups"),
              nobs = sum(lavaan::lavInspect(x, "nobs"))
            )
          )
        fit_measure <- dplyr::select(fit_measure,
            nobs, estimator, ngroups, converged, chisq.scaled,
            df.scaled, pvalue.scaled, npar, cfi.scaled, tli.scaled,
            rmsea.scaled, rmsea.ci.lower.scaled, rmsea.ci.upper.scaled,
            srmr, wrmr, crmr
          )
        fit_measure <- dplyr::rename_with(fit_measure,
            ~ stringr::str_remove(., ".scaled")
          )
      } else {
        fit_measure <- lavaan::fitmeasures(x,
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
          )
        fit_measure <- tibble::enframe(fit_measure, name = "term")
        fit_measure <- tidyr::pivot_wider(fit_measure,
            
            names_from = term,
            values_from = value
          )
        fit_measure <- dplyr::mutate(fit_measure,
            dplyr::across(dplyr::everything(), as.numeric)
          )
        fit_measure <- dplyr::bind_cols(fit_measure,
            tibble::tibble(
              converged = lavaan::lavInspect(x, "converged"),
              estimator = lavaan::lavInspect(x, "options")$estimator,
              ngroups = lavaan::lavInspect(x, "ngroups"),
              missing_method = lavaan::lavInspect(x, "options")$missing,
              nobs = sum(lavaan::lavInspect(x, "nobs"))
            )
          )
        fit_measure <- dplyr::relocate(fit_measure,
            nobs, estimator, ngroups, converged,
            chisq, df, pvalue, npar, cfi, tli, rmsea,
            rmsea.ci.lower, rmsea.ci.upper, srmr, wrmr, crmr,
            aic, bic
          )
        fit_measure <- dplyr::rename(fit_measure,
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

    fit_measure <- dplyr::rename_with(fit_measure,
        .fn = ~ stringr::str_to_upper(.),
        .cols = dplyr::any_of(c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "wrmr", "crmr"))
      )

    return(fit_measure)
  }

  set.seed(seed)

  # Replicar el dataframe con reemplazo
  new_df <- purrr::map_dfr(integer(n_replications), ~ dplyr::sample_n(new_df, size = nrow(new_df), replace = TRUE), .id = "obs")

  # Mensaje antes de calcular CFA
  message("Calculando el CFA con bootstrapping")

  # Seleccionar columnas
  item_cols <- grep(paste0("^", item_prefix), names(new_df), value = TRUE)
  selected_df <- new_df[, c(item_cols, "obs")]

  # Agrupar por obs
  Replicaciones <- tidyr::nest(selected_df, data = -obs)

  # Procesar cada replica
  Replicaciones$data <- lapply(Replicaciones$data, function(d) {
    d[, colSums(!is.na(d)) > 0]
  })

  Replicaciones$model_cfa1 <- rep(model_string, nrow(Replicaciones))

  # Ajustar modelos CFA con barra de progreso
  Replicaciones$fit_cfa1 <- pbapply::pblapply(seq_along(Replicaciones$data), function(i) {
    lavaan::cfa(model = Replicaciones$model_cfa1[[i]],
        data = Replicaciones$data[[i]],
        ordered = ordered,
        estimator = estimator)
  }, cl = 1)

  Replicaciones$converged1 <- sapply(Replicaciones$fit_cfa1, function(x) lavaan::lavInspect(x, "converged"))
  Replicaciones$validity1 <- sapply(Replicaciones$fit_cfa1, function(x) suppressWarnings(lavaan::lavInspect(x, "post.check")))
  Replicaciones$fit_measures1 <- lapply(Replicaciones$fit_cfa1, fit_lavaan)

  # Mensaje antes de calcular la fiabilidad
  message("\nCalculando la fiabilidad con bootstrapping")

  omega2 <- pbapply::pblapply(Replicaciones$fit_cfa1, function(model) {
    semTools::compRelSEM(model, tau.eq = FALSE, ord.scale = TRUE)
  })
  omega2 <- purrr::map_dfr(omega2, ~ dplyr::bind_rows(.))

  # Agregar los resultados de omega2 a Replicaciones y renombrar columnas
  Replicaciones <- dplyr::bind_cols(Replicaciones, omega2)
  omega_cols <- grep("^F\\d+$", names(Replicaciones), value = TRUE)
  new_names <- gsub("^F(\\d+)$", "Rel\\1", omega_cols)
  names(Replicaciones)[names(Replicaciones) %in% omega_cols] <- new_names

  return(Replicaciones)
}
