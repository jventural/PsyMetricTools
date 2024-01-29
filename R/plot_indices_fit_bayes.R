plot_indices_fit_bayes  <-  function(bfit1, custom_palette = NULL) {
  # Extraer los valores de BRMSEA y BCFI
  brmseavalues <- bfit1@indices[["BRMSEA"]]
  bcfivalues <- bfit1@indices[["BCFI"]]

  # Crear un data frame con estos valores
  afivalues <- data.frame(brmseavalues, bcfivalues)

  # Convertir los datos a formato largo
  afivalues_long <- tidyr::pivot_longer(afivalues, cols = colnames(afivalues), names_to = "Item", values_to = "Value")

  # Si no se proporciona una paleta personalizada, usa una por defecto
  if(is.null(custom_palette)) {
    custom_palette <- RColorBrewer::brewer.pal(3, "Set1")
  }

  # Asegurarse de que hay suficientes colores
  if(length(custom_palette) < 2) {
    stop("No hay suficientes colores en la paleta para todas las columnas.")
  }

  # Crear el gráfico
  p <- ggplot(data = afivalues_long, aes(x = Value, fill = Item)) +
    geom_density(alpha = 0.8) +
    facet_wrap(~ Item, scales = "free", nrow = 2) +
    scale_fill_manual(values = custom_palette[1:2]) +
    labs(y = "Density", x = "Value", fill = "Item") +
    theme_minimal()

  print(p)
}
