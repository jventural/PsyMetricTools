get_factor_blavaan <- function(fit_model, credMass = 0.95) {
  # Extraer el estadístico de convergencia PSRF para las cargas factoriales
  psrf <- blavInspect(fit_model, what = "psrf")

  # Extraer las estimaciones posteriores estandarizadas
  stpost <- as.data.frame(standardizedPosterior(fit_model))

  # Inicializar el data frame de resultados
  results_df <- data.frame(
    Factor = character(0),
    PosteriorMean = numeric(0),
    CredibleInterval = character(0),
    PSRF = numeric(0),
    stringsAsFactors = FALSE # Evitar la conversión a factores
  )

  # Bucle a través de las cargas factoriales
  for (i in grep("^F1=~", names(stpost), value = TRUE)) {
    factor_name <- sub("F1=~", "", i) # Limpiar los nombres de los factores
    posterior_mean <- mean(stpost[[i]])
    hdi_values <- HDInterval::hdi(stpost[[i]], credMass = credMass)
    hdi_formatted <- sprintf("[%0.3f, %0.3f]", hdi_values[1], hdi_values[2])
    psrf_value <- psrf[i]

    # Añadir al data frame de resultados
    results_df <- rbind(results_df, data.frame(
      Factor = factor_name,
      PosteriorMean = posterior_mean,
      CredibleInterval = hdi_formatted,
      PSRF = ifelse(!is.na(psrf_value) && psrf_value > 1, sprintf("%.3f", psrf_value), "–") # PSRF como carácter, guión si es 1
    ), make.row.names = FALSE) # Evitar la creación de nombres de fila
  }

  return(results_df)
}
