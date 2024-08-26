plot_cfa_stability2 <- function(resultados,
                                y_min_cfi = 0.9, y_max_cfi = 1,
                                y_min_tli  = 0.9, y_max_tli = 1,
                                y_min_rmsea = 0, y_max_rmsea = 0.15,
                                y_min_srmr = 0, y_max_srmr = 0.15,
                                y_min_crmr = 0, y_max_crmr = 0.15,
                                y_min_reliability = 0.50, y_max_reliability = 1,
                                y_breaks = 0.01,
                                y_breaks_reliability = 0.05,
                                hline_color = "red",
                                xlab_size = 12,
                                ylab_size = 12,
                                label_size = 10) {

  library(tidyverse)
  library(ggplot2)
  library(ggpubr)

  # Verificar y convertir a data.frame si es necesario
  if (!is.data.frame(resultados)) {
    resultados <- as.data.frame(resultados)
  }

  # Crear el gráfico de boxplot CFI
  b1 <- ggplot(resultados, aes(x = factor(Porcentaje), y = cfi.scaled)) +
    geom_boxplot() +
    labs(x = "Percentages", y = "CFI", title = "") +
    theme_bw() +
    scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, size = 0.5) +
    scale_y_continuous(limits = c(y_min_cfi, y_max_cfi), breaks = seq(y_min_cfi, y_max_cfi, y_breaks)) +
    theme(axis.text.x = element_text(size = xlab_size),
          axis.text.y = element_text(size = ylab_size))

  # Crear el gráfico de boxplot TLI
  b2 <- ggplot(resultados, aes(x = factor(Porcentaje), y = tli.scaled)) +
    geom_boxplot() +
    labs(x = "Percentages", y = "TLI", title = "") +
    theme_bw() +
    scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, size = 0.5) +
    scale_y_continuous(limits = c(y_min_tli, y_max_tli), breaks = seq(y_min_tli, y_max_tli, y_breaks)) +
    theme(axis.text.x = element_text(size = xlab_size),
          axis.text.y = element_text(size = ylab_size))

  # Crear el gráfico de boxplot RMSEA
  b3 <- ggplot(resultados, aes(x = factor(Porcentaje), y = rmsea.scaled)) +
    geom_boxplot() +
    labs(x = "Percentages", y = "RMSEA", title = "") +
    theme_bw() +
    scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    geom_hline(yintercept = 0.08, linetype = "solid", color = hline_color, size = 0.5) +
    scale_y_continuous(limits = c(y_min_rmsea, y_max_rmsea), breaks = seq(y_min_rmsea, y_max_rmsea, y_breaks)) +
    theme(axis.text.x = element_text(size = xlab_size),
          axis.text.y = element_text(size = ylab_size))

  # Crear el gráfico de boxplot SRMR
  b4 <- ggplot(resultados, aes(x = factor(Porcentaje), y = srmr)) +
    geom_boxplot() +
    labs(x = "Percentages", y = "SRMR", title = "") +
    theme_bw() +
    scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    geom_hline(yintercept = 0.08, linetype = "solid", color = hline_color, size = 0.5) +
    scale_y_continuous(limits = c(y_min_srmr, y_max_srmr), breaks = seq(y_min_srmr, y_max_srmr, y_breaks)) +
    theme(axis.text.x = element_text(size = xlab_size),
          axis.text.y = element_text(size = ylab_size))

  # Crear el gráfico de boxplot CRMR
  b5 <- ggplot(resultados, aes(x = factor(Porcentaje), y = crmr)) +
    geom_boxplot() +
    labs(x = "Percentages", y = "CRMR", title = "") +
    theme_bw() +
    scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    geom_hline(yintercept = 0.05, linetype = "solid", color = hline_color, size = 0.5) +
    scale_y_continuous(limits = c(y_min_crmr, y_max_crmr), breaks = seq(y_min_crmr, y_max_crmr, y_breaks)) +
    theme(axis.text.x = element_text(size = xlab_size),
          axis.text.y = element_text(size = ylab_size))

  # Crear el gráfico de boxplot fiabilidad
  b6 <- ggplot(resultados, aes(x = factor(Porcentaje), y = F1)) +
    geom_boxplot() +
    labs(x = "Percentages", y = "Reliability", title = "") +
    theme_bw() +
    scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    geom_hline(yintercept = 0.70, linetype = "solid", color = hline_color, size = 0.5) +
    scale_y_continuous(limits = c(y_min_reliability, y_max_reliability), breaks = seq(y_min_reliability, y_max_reliability, y_breaks_reliability)) +
    theme(axis.text.x = element_text(size = xlab_size),
          axis.text.y = element_text(size = ylab_size))

  # Combinar todos los gráficos en una figura
  figure3 <- suppressMessages(suppressWarnings(ggarrange(b1, b2, b3, b4, b5, b6,
                                                         labels = c("A", "B", "C", "D", "E", "F"),
                                                         ncol = 3, nrow = 2,
                                                         label_size = label_size)))

  return(figure3)
}
