traceplot_bayesrel <- function(res, paleta_colores = NULL) {
  library(ggplot2)
  library(reshape2)
  library(ggridges)
  library(MCMCpack)
  library(coda)
  # Extracción de las muestras de 'res'
  samp <- res[["Bayes"]][["samp"]][["Bayes_omega"]]

  # Conversión a lista de objetos MCMC
  samp_list <- coda::as.mcmc.list(lapply(as.data.frame(t(samp)), mcmc))

  # Convertir cada cadena MCMC en un data frame y combinarlos
  df_samp <- do.call(rbind, lapply(seq_along(samp_list), function(i) {
    df_chain <- as.data.frame(samp_list[[i]])
    df_chain$Cadena <- paste("Cadena", i, sep = "")
    df_chain$Time <- seq_len(nrow(df_chain))
    return(df_chain)
  }))

  # Reestructurar en formato largo para ggplot2
  df_long <- melt(df_samp, id.vars = c("Time", "Cadena"))

  # Crear el traceplot usando ggplot2
  p4 <- ggplot(df_long, aes(x = Time, y = value, color = Cadena)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Iteración", y = "McDonald's omega") +
    theme(legend.position = "none")

  # Aplicar la paleta de colores si se proporciona
  if (!is.null(paleta_colores)) {
    p4 <- p4 + scale_color_manual(values = paleta_colores)
  }

  return(p4)
}
