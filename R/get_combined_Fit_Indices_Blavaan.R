get_combined_Fit_Indices_Blavaan <- function(models_list, credMass = 0.95) {
  model_summaries <- lapply(models_list, function(model) {
    # Calcular medias
    chisq_mean <- mean(model@details$chisq)
    pD_mean <- mean(model@details$pD)
    df_mean <- mean(model@details$df)
    brmsea_mean <- mean(model@indices$BRMSEA)
    bcfi_mean <- mean(model@indices$BCFI)

    # Calcular HDI para BRMSEA y BCFI
    hdi_brmsea <- HDInterval::hdi(model@indices$BRMSEA, credMass = credMass)
    brmsea_hdi <- sprintf("[%0.3f, %0.3f]", hdi_brmsea[1], hdi_brmsea[2])
    hdi_bcfi <- HDInterval::hdi(model@indices$BCFI, credMass = credMass)
    bcfi_hdi <- sprintf("[%0.3f, %0.3f]", hdi_bcfi[1], hdi_bcfi[2])

    # Extraer ppp de las medidas de ajuste del modelo
    ppp <- (fitMeasures(fit1) %>% as_tibble() %>% slice(3) %>% pull())

    # Crear un data frame con una sola fila y una columna para cada estadística
    summary_df <- data.frame(
      chisq_mean,
      pD_mean,
      df_mean,
      ppp,
      brmsea_mean,
      BRMSEA_CrI = brmsea_hdi,
      bcfi_mean,
      BCFI_CrI = bcfi_hdi
    )

    return(summary_df)
  })

  # Combinar todos los data frames de la lista en uno solo
  combined_df <- do.call(rbind, model_summaries)

  # Establecer los nombres de las columnas
  col_names <- c("chisq", "pD", "df", "ppp", "BRMSEA", sprintf("BRMSEA %d%% CrI", credMass * 100), "BCFI", sprintf("BCFI %d%% CrI", credMass * 100))
  colnames(combined_df) <- col_names

  return(combined_df)
}
