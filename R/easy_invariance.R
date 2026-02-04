#' @title Analisis de Invarianza Factorial Simplificado
#' @description Realiza analisis de invarianza factorial para datos ordinales
#'   utilizando el enfoque de Wu y Estabrook (2016) con estimador WLSMV.
#' @param model Modelo de lavaan en formato texto.
#' @param data Data frame con los datos.
#' @param estimator Estimador a utilizar (por defecto "WLSMV").
#' @param ordered Logical indicando si las variables son ordinales.
#' @param ID.cat Metodo de identificacion para variables categoricas (e.g., "Wu.Estabrook.2016").
#' @param group Nombre de la variable de agrupacion.
#' @param levels_of_invariance Vector con los niveles de invarianza a evaluar:
#'   "configural", "threshold", "metric", "strict".
#' @param group.partial Vector opcional de parametros a liberar parcialmente.
#' @param fit_indices Vector con los indices de ajuste a incluir. Opciones disponibles:
#'   "CFI", "TLI", "RMSEA", "SRMR", "CRMR". Por defecto incluye c("CFI", "TLI", "RMSEA", "SRMR").
#'   Para excluir RMSEA (util con pocos grados de libertad), usar c("CFI", "TLI", "SRMR", "CRMR").
#' @return Lista con combined_data (tabla resumen) y los modelos ajustados.
#' @export
#' @examples
#' \dontrun{
#' # Ejemplo con todos los indices por defecto
#' res <- easy_invariance(
#'   model = "F =~ item1 + item2 + item3",
#'   data = mydata,
#'   estimator = "WLSMV",
#'   ordered = TRUE,
#'   ID.cat = "Wu.Estabrook.2016",
#'   group = "sex",
#'   levels_of_invariance = c("configural", "threshold", "metric", "strict")
#' )
#'
#' # Ejemplo excluyendo RMSEA
#' res <- easy_invariance(
#'   model = "F =~ item1 + item2 + item3",
#'   data = mydata,
#'   estimator = "WLSMV",
#'   ordered = TRUE,
#'   ID.cat = "Wu.Estabrook.2016",
#'   group = "sex",
#'   levels_of_invariance = c("configural", "threshold", "metric", "strict"),
#'   fit_indices = c("CFI", "TLI", "SRMR", "CRMR")
#' )
#' }
easy_invariance <- function(model, data, estimator, ordered, ID.cat, group,
                            levels_of_invariance,
                            group.partial = NULL,
                            fit_indices = c("CFI", "TLI", "RMSEA", "SRMR")) {

  run_invariance <- function(model, data, estimator, ordered,
                             ID.cat, group,
                             group.equal,
                             long.equal = NULL,
                             group.partial = NULL) {

    semTools::measEq.syntax(
      configural.model  = model,
      data              = data,
      estimator         = estimator,
      ordered           = ordered,
      parameterization  = "theta",
      ID.fac            = "std.lv",
      ID.cat            = ID.cat,
      group             = group,
      group.equal       = group.equal,
      long.equal        = long.equal,
      group.partial     = group.partial,
      return.fit        = TRUE
    )
  }

  results <- list()

  for (level in levels_of_invariance) {

    if (level == "configural") {
      results$configural <- run_invariance(
        model, data, estimator, ordered, ID.cat, group,
        group.equal   = "configural",
        long.equal    = NULL,
        group.partial = group.partial
      )

    } else if (level == "threshold") {
      results$threshold <- run_invariance(
        model, data, estimator, ordered, ID.cat, group,
        group.equal   = "thresholds",
        long.equal    = "thresholds",
        group.partial = group.partial
      )

    } else if (level == "metric") {
      results$metric <- run_invariance(
        model, data, estimator, ordered, ID.cat, group,
        group.equal   = c("thresholds", "loadings"),
        long.equal    = c("thresholds", "loadings"),
        group.partial = group.partial
      )

    } else if (level == "strict") {
      results$strict <- run_invariance(
        model, data, estimator, ordered, ID.cat, group,
        group.equal   = c("thresholds", "loadings", "residuals"),
        long.equal    = c("thresholds", "loadings", "residuals"),
        group.partial = group.partial
      )
    }
  }

  # --- Compare nested models in the intended order ---
  fits_in_order <- list(
    configural = results$configural,
    threshold  = results$threshold,
    metric     = results$metric,
    strict     = results$strict
  )
  fits_in_order <- fits_in_order[!vapply(fits_in_order, is.null, logical(1))]

  a <- suppressWarnings(do.call(semTools::compareFit, fits_in_order))

  # Extraer fit measures directamente de cada modelo
  model_names <- names(fits_in_order)


  # Mapeo de nombres de indices a nombres de lavaan
  index_mapping <- c(
    "CFI" = "cfi.scaled",
    "TLI" = "tli.scaled",
    "RMSEA" = "rmsea.scaled",
    "SRMR" = "srmr",
    "CRMR" = "crmr"
  )

  # Validar indices seleccionados
  valid_indices <- c("CFI", "TLI", "RMSEA", "SRMR", "CRMR")
  fit_indices <- toupper(fit_indices)
  fit_indices <- fit_indices[fit_indices %in% valid_indices]

  if (length(fit_indices) == 0) {
    stop("Debe especificar al menos un indice valido: CFI, TLI, RMSEA, SRMR, CRMR")
  }

  # Construir vector de indices a extraer
  lavaan_indices <- c("chisq.scaled", "df.scaled", index_mapping[fit_indices])

  fit_list <- lapply(fits_in_order, function(fit) {
    fm <- lavaan::fitMeasures(fit, lavaan_indices)

    # Crear data frame base
    result <- data.frame(
      Chisq = fm["chisq.scaled"],
      Df = fm["df.scaled"]
    )

    # Agregar indices seleccionados
    for (idx in fit_indices) {
      result[[idx]] <- round(fm[index_mapping[idx]], 3)
    }

    result
  })

  fit_df <- do.call(rbind, fit_list)
  fit_df$Model <- model_names
  rownames(fit_df) <- NULL

  # Calcular diferencias para cada indice seleccionado
  for (idx in fit_indices) {
    diff_col <- paste0(idx, "_diff")
    fit_df[[diff_col]] <- NA

    for (i in 2:nrow(fit_df)) {
      fit_df[[diff_col]][i] <- round(fit_df[[idx]][i] - fit_df[[idx]][i-1], 3)
    }
  }

  # Calcular chi-square difference tests
  nested_data <- as.data.frame(a@nested)
  nested_data$Model <- model_names

  # Combinar datos
  combined_data <- dplyr::left_join(
    fit_df,
    dplyr::select(nested_data, Model, `Chisq diff`, `Df diff`, `Pr(>Chisq)`),
    by = "Model"
  )
  combined_data$Chisq_df <- paste0(round(combined_data$Chisq, 2), " (", round(combined_data$Df, 0), ")")
  combined_data$`Chisq diff` <- round(combined_data$`Chisq diff`, 3)
  combined_data$`Pr(>Chisq)` <- round(combined_data$`Pr(>Chisq)`, 3)

  # Construir vector de columnas a seleccionar dinamicamente
  base_cols <- c("Model", "Chisq_df", "Chisq diff", "Df diff", "Pr(>Chisq)")
  index_cols <- c()
  for (idx in fit_indices) {
    index_cols <- c(index_cols, idx, paste0(idx, "_diff"))
  }

  combined_data <- combined_data[, c(base_cols, index_cols)]

  # Renombrar columnas
  new_names <- c("Level", "X2(df)", "DX2", "Ddf", "p")
  for (idx in fit_indices) {
    new_names <- c(new_names, idx, paste0("D", idx))
  }
  colnames(combined_data) <- new_names

  return(list(
    combined_data = combined_data,
    configural    = results$configural,
    threshold     = results$threshold,
    metric        = results$metric,
    strict        = results$strict
  ))
}
