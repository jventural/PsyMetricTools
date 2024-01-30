get_factor_blavaan <- function(fit_model, credMass = 0.95) {
  # Asegúrate de que las dependencias están cargadas
  if (!require(data.table)) install.packages("data.table")
  library(data.table)
  # Extraer el estadístico de convergencia PSRF para las cargas factoriales
  psrf <- blavInspect(fit_model, what = "psrf")

  # Extraer las estimaciones posteriores estandarizadas
  stpost <- as.data.frame(standardizedPosterior(fit_model))

  # Lista para almacenar los resultados
  results_list <- list()

  # Bucle a través de las cargas factoriales
  for (i in grep("=~", names(stpost), value = TRUE)) {
    factor_name <- sub("=~", "", i) # Limpiar los nombres de los factores
    posterior_mean <- mean(stpost[[i]])
    hdi_values <- HDInterval::hdi(stpost[[i]], credMass = credMass)
    hdi_formatted <- sprintf("[%0.3f, %0.3f]", hdi_values[1], hdi_values[2])
    psrf_value <- psrf[i]

    # Determinar el valor de PSRF para mostrar
    psrf_display <- ifelse(!is.na(psrf_value), sprintf("%.3f", psrf_value), "–")

    # Añadir a la lista de resultados
    results_list[[length(results_list) + 1]] <- list(
      Factor = factor_name,
      PosteriorMean = posterior_mean,
      CredibleInterval = hdi_formatted,
      PSRF = psrf_display
    )
  }

  # Convertir la lista en un data frame usando data.table
  results_df <- data.table::rbindlist(results_list, fill = TRUE)

  # Añadir un guión en la primera fila si no hay información de PSRF
  if (nrow(results_df) > 0 && is.na(psrf[1])) {
    results_df$PSRF[1] <- "–"
  }

  return(results_df)
}
