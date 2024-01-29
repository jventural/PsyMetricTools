Get_Fit_Indices_Blavaan <- function(model, credMass = 0.95) {
  library(tidyverse)
  if (!require(blavaan)) {
    stop("El paquete 'blavaan' es necesario para esta función.")
  }

  # Calcular medias
  chisq_mean <- mean(model@details$chisq)
  pD_mean <- mean(model@details$pD)
  df_mean <- mean(model@details$df)
  brmsea_mean <- mean(model@indices$BRMSEA)
  bcfi_mean <- mean(model@indices$BCFI)

  # Extraer ppp como el tercer elemento de fitMeasures
  ppp <- (fitMeasures(fit1) %>% as_tibble() %>% slice(3) %>% pull())

  # Calcular HDI para BRMSEA
  hdi_brmsea <- HDInterval::hdi(model@indices$BRMSEA, credMass = credMass)
  brmsea_hdi <- sprintf("[%0.3f, %0.3f]", hdi_brmsea[1], hdi_brmsea[2])

  # Calcular HDI para BCFI
  hdi_bcfi <- HDInterval::hdi(model@indices$BCFI, credMass = credMass)
  bcfi_hdi <- sprintf("[%0.3f, %0.3f]", hdi_bcfi[1], hdi_bcfi[2])

  # Determinar el nombre de las columnas de los intervalos HDI
  ci_col_name <- sprintf("%.0f%% CrI", credMass * 100)

  # Crear un data frame con una sola fila y una columna para cada estadística
  summary_df <- setNames(data.frame(
    chisq_mean,
    pD_mean,
    df_mean,
    ppp, # Añadir ppp aquí
    brmsea_mean,
    brmsea_hdi,
    bcfi_mean,
    bcfi_hdi
  ), c("chisq", "pD", "df", "ppp", "BRMSEA", ci_col_name, "BCFI", ci_col_name))

  return(summary_df)
}
