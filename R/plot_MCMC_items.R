plot_MCMC_items  <-  function(fit2, custom_palette = NULL) {
  library(tidyr)
  library(ggplot2)
  # Suponiendo que 'standardizedPosterior' devuelve un objeto que se pueda convertir en data.frame
  MCMCbinded2 <- standardizedPosterior(fit2)
  MCMCbinded2 <- as.data.frame(MCMCbinded2)

  # Seleccionamos todas las columnas que contienen '~'
  cols_with_tilde <- grep("=~", names(MCMCbinded2), value = TRUE)
  MCMCbinded2 <- MCMCbinded2[, cols_with_tilde]

  # Renombramos las columnas para eliminar el prefijo 'F1=~'
  names(MCMCbinded2) <- gsub("F1=~", "", names(MCMCbinded2))

  # Transformamos los datos a formato largo
  MCMC_long <- pivot_longer(MCMCbinded2, cols = colnames(MCMCbinded2), names_to = "Item", values_to = "Value")

  # Si no se proporciona una paleta personalizada, usa una por defecto
  if(is.null(custom_palette)) {
    custom_palette <- RColorBrewer::brewer.pal(3, "Pastel1")
  }

  # Asegúrate de que hay suficientes colores para las columnas
  if(length(custom_palette) < length(colnames(MCMCbinded2))) {
    stop("No hay suficientes colores en la paleta para todas las columnas.")
  }

  # Usar la paleta personalizada en scale_fill_manual
  p <- ggplot(data = MCMC_long, aes(x = Value)) +
    geom_density(aes(fill = Item), alpha = 0.8) +
    facet_wrap(~ Item, scales = "free_x", nrow = 2) +
    labs(y = "Density", x = "Value") +
    theme_minimal() +
    scale_fill_manual(values = custom_palette[1:length(colnames(MCMCbinded2))])

  print(p)
}
