boot_cfa <- function(new_df, model_string, item_prefix, seed = 2023, n_replications = 1000) {
  # Función interna
  fit_lavaan_simplified <- function(x) {

    library(dplyr)
    library(tidyr)
    library(lavaan)
    library(tibble)
    library(stringr)


    type <- is_robust_estimator_lavaan(x)
    converged <- lavaan::lavInspect(x, "converged")
    estimator_info <- tibble(
      nobs = sum(lavaan::lavInspect(x, "nobs")),
      estimator = lavaan_estimator(x),
      ngroups = lavaan::lavInspect(x, "ngroups"),
      converged = converged,
      estimator_type = type
    )

    if (!converged) {
      return(estimator_info %>%
               mutate(across(chisq:crmr, ~NA_real_)) %>%
               rename_with(~stringr::str_to_upper(.), c(cfi:crmr)))
    }

    fit_measures <- if (type == "robust") {
      c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled",
        "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "wrmr", "crmr",
        "tli.scaled", "cfi.scaled")
    } else {
      c("npar", "chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
        "srmr", "wrmr", "crmr", "aic", "bic", "tli", "cfi")
    }

    fit_measure <- x %>%
      lavaan::fitmeasures(fit.measures = fit_measures) %>%
      enframe(name = "term") %>%
      pivot_wider(names_from = term, values_from = value) %>%
      mutate(across(everything(), as.numeric)) %>%
      bind_cols(estimator_info) %>%
      select(-estimator_type) %>%
      rename_with(~stringr::str_remove(., ".scaled")) %>%
      rename_with(.fn = ~stringr::str_to_upper(.), .cols = c(cfi:crmr))

    return(fit_measure)
  }

  # Función interna
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

  # Funcion interna
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

  set.seed(seed)

  # Cargar las bibliotecas necesarias
  library(purrr)
  library(dplyr)
  library(lavaan)
  library(pbapply) # Usar pbapply en lugar de progress

  # Replicar el dataframe con reemplazo
  new_df <- purrr::map_dfr(integer(n_replications), ~ new_df %>% sample_n(size = nrow(new_df), replace = TRUE), .id = "obs")

  # Mensaje antes de calcular CFA
  message("Calculando el CFA con bootstrapping")

  Replicaciones <- new_df %>%
    select(starts_with(item_prefix) | ends_with("obs")) %>%
    group_nest(obs) %>%
    mutate(
      data = map(data, ~ select(., tidyselect:::where(~ !all(is.na(.))))),
      model_cfa1 = map_chr(data, ~ paste0(model_string)),
      fit_cfa1 = pblapply(seq_along(data), function(i) {
        cfa(model = model_cfa1[[i]],
            data = data[[i]],
            ordered = TRUE,
            estimator = "WLSMV")
      }, cl = 1),  # Usar pblapply para aplicar cfa con barra de progreso
      converged1 = map_lgl(fit_cfa1, ~ lavInspect(.x, "converged")),
      validity1 = map_lgl(fit_cfa1, ~ suppressWarnings(lavInspect(.x, "post.check"))),
      fit_measures1 = map(fit_cfa1, ~ fit_lavaan_simplified(.x) %>%
                            rename_with(.fn = toupper, .cols = c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "wrmr")))
    ) %>%
    collect()

  # Mensaje antes de calcular la fiabilidad
  message("\nCalculando la fiabilidad con bootstrapping")

  omega2 <- pbapply::pblapply(Replicaciones$fit_cfa1, function(model) {
    semTools::compRelSEM(model, tau.eq = FALSE, ord.scale = TRUE)
  }) %>%
    map_dfr(~ bind_rows(.))

  # Agregar los resultados de omega2 a Replicaciones y renombrar columnas
  Replicaciones <- Replicaciones %>% bind_cols(omega2)  %>%
    rename_with(~ gsub("^F(\\d+)$", "Rel\\1", .), .cols = starts_with("F"))

  return(Replicaciones)
}
