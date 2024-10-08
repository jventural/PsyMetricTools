multi_cfa <- function(modelos, data, estimator, ordered = TRUE, orthogonal_indices = NULL) {
  ajustar_modelo <- function(modelo, index, data, estimator, ordered, orthogonal_indices) {
    resultado <- tryCatch({
      # Verificar si el modelo actual necesita el argumento orthogonal = TRUE
      if (!is.null(orthogonal_indices) && index %in% orthogonal_indices) {
        fit <- cfa(modelo, data = data, estimator = estimator, mimic = "Mplus", ordered = ordered, orthogonal = TRUE)
      } else {
        fit <- cfa(modelo, data = data, estimator = estimator, mimic = "Mplus", ordered = ordered)
      }

      # Calcular medidas de bondad de ajuste
      bondad_ajuste <- round(fitMeasures(fit, c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled", "crmr")), 3)
      indices_modificacion <- modificationIndices(fit, sort = TRUE) %>% dplyr::filter(mi > 10) %>% mutate(mi = round(mi, 2))
      correlaciones_factores <- round(as.data.frame(inspect(fit, what = "std")$psi), 2)
      fiabilidad <- round(compRelSEM(fit, tau.eq = F, ord.scale = T), 2)

      return(list(fit = fit,
                  bondad_ajuste = bondad_ajuste,
                  indices_modificacion = indices_modificacion,
                  correlaciones_factores = correlaciones_factores,
                  fiabilidad = fiabilidad))
    }, error = function(e) {
      # Si ocurre un error, retornamos una lista con NA y un mensaje de error
      bondad_ajuste <- data.frame(
        chisq.scaled = NA,
        df.scaled = NA,
        srmr = NA,
        wrmr = NA,
        cfi.scaled = NA,
        tli.scaled = NA,
        rmsea.scaled = NA,
        crmr = NA,
        mensaje = "model does not converge"
      )
      return(list(fit = NULL,
                  bondad_ajuste = bondad_ajuste,
                  indices_modificacion = NA,
                  correlaciones_factores = NA,
                  fiabilidad = NA))
    })
    return(resultado)
  }

  # Aplicar la función ajustar_modelo a cada modelo
  resultados <- mapply(ajustar_modelo, modelos, seq_along(modelos), MoreArgs = list(data = data, estimator = estimator, ordered = ordered, orthogonal_indices = orthogonal_indices), SIMPLIFY = FALSE)

  # Consolidar los resultados
  bondades_ajuste <- do.call(rbind, lapply(resultados, function(x) x$bondad_ajuste))
  rownames(bondades_ajuste) <- paste0("Modelo", 1:length(resultados))

  fits <- lapply(resultados, function(x) x$fit)
  indices_modificacion <- lapply(resultados, function(x) x$indices_modificacion)
  correlaciones_factores <- lapply(resultados, function(x) x$correlaciones_factores)
  fiabilidad_modelos <- lapply(resultados, function(x) x$fiabilidad)

  nombres_modelos <- paste0("Modelo", 1:length(correlaciones_factores))
  lista_correlaciones_df <- setNames(correlaciones_factores, nombres_modelos)
  lista_fiabilidad_df <- setNames(fiabilidad_modelos, nombres_modelos)

  return(list(fits = fits,
              bondades_ajuste = bondades_ajuste,
              indices_modificacion = indices_modificacion,
              correlaciones_factores = lista_correlaciones_df,
              fiabilidad = lista_fiabilidad_df))
}
