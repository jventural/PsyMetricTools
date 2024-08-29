plot_cfa_stability <- function(resultados, num_factors = 3, y_min_cfi = 0.9, y_max_cfi = 1,
                                y_min_tli = 0.9, y_max_tli = 1, y_min_rmsea = 0, y_max_rmsea = 0.15,
                                y_min_srmr = 0, y_max_srmr = 0.15, y_min_crmr = 0, y_max_crmr = 0.15,
                                y_min_reliability = 0.5, y_max_reliability = 1,
                                y_breaks = 0.01, y_breaks_reliability = 0.05,
                                hline_color = "red", xlab_size = 12, ylab_size = 12)
{
  library(tidyverse)
  library(ggplot2)
  library(ggpubr)

  # Gráfico de CFI
  CFI_stability <- resultados %>%
    group_by(Porcentaje) %>%
    summarise(CFI = mean(cfi.scaled)) %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  p4 <- ggplot(CFI_stability, aes(x = Porcentaje, y = CFI, group = 1)) +
    geom_line() +
    geom_point() +
    labs(x = "Percentage", y = "CFI") +
    scale_y_continuous(limits = c(y_min_cfi, y_max_cfi), breaks = seq(y_min_cfi, y_max_cfi, y_breaks)) +
    geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, size = 0.5) +
    scale_x_reverse(breaks = seq(90, 30, -10)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = xlab_size), axis.text.y = element_text(size = ylab_size))

  # Gráfico de TLI
  TLI_stability <- resultados %>%
    group_by(Porcentaje) %>%
    summarise(TLI = mean(tli.scaled)) %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  p5 <- ggplot(TLI_stability, aes(x = Porcentaje, y = TLI, group = 1)) +
    geom_line() +
    geom_point() +
    labs(x = "Percentage", y = "TLI") +
    scale_y_continuous(limits = c(y_min_tli, y_max_tli), breaks = seq(y_min_tli, y_max_tli, y_breaks)) +
    geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, size = 0.5) +
    scale_x_reverse(breaks = seq(90, 30, -10)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = xlab_size), axis.text.y = element_text(size = ylab_size))

  # Gráfico de RMSEA
  RMSEA_stability <- resultados %>%
    group_by(Porcentaje) %>%
    summarise(RMSEA = mean(rmsea.scaled)) %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  p6 <- ggplot(RMSEA_stability, aes(x = Porcentaje, y = RMSEA, group = 1)) +
    geom_line() +
    geom_point() +
    labs(x = "Percentage", y = "RMSEA") +
    scale_y_continuous(limits = c(y_min_rmsea, y_max_rmsea), breaks = seq(y_min_rmsea, y_max_rmsea, y_breaks)) +
    geom_hline(yintercept = 0.08, linetype = "solid", color = hline_color, size = 0.5) +
    scale_x_reverse(breaks = seq(90, 30, -10)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = xlab_size), axis.text.y = element_text(size = ylab_size))

  # Gráfico de SRMR
  SRMR_stability <- resultados %>%
    group_by(Porcentaje) %>%
    summarise(SRMR = mean(srmr)) %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  p7 <- ggplot(SRMR_stability, aes(x = Porcentaje, y = SRMR, group = 1)) +
    geom_line() +
    geom_point() +
    labs(x = "Percentage", y = "SRMR") +
    scale_y_continuous(limits = c(y_min_srmr, y_max_srmr), breaks = seq(y_min_srmr, y_max_srmr, y_breaks)) +
    geom_hline(yintercept = 0.06, linetype = "solid", color = hline_color, size = 0.5) +
    scale_x_reverse(breaks = seq(90, 30, -10)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = xlab_size), axis.text.y = element_text(size = ylab_size))

  # Gráfico de CRMR
  CRMR_stability <- resultados %>%
    group_by(Porcentaje) %>%
    summarise(CRMR = mean(crmr)) %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  p8 <- ggplot(CRMR_stability, aes(x = Porcentaje, y = CRMR, group = 1)) +
    geom_line() +
    geom_point() +
    labs(x = "Percentage", y = "CRMR") +
    scale_y_continuous(limits = c(y_min_crmr, y_max_crmr), breaks = seq(y_min_crmr, y_max_crmr, y_breaks)) +
    geom_hline(yintercept = 0.05, linetype = "solid", color = hline_color, size = 0.5) +
    scale_x_reverse(breaks = seq(90, 30, -10)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = xlab_size), axis.text.y = element_text(size = ylab_size))

  # Gráfico de medias para Fiabilidad Multidimensional (ω)
  summary_long <- resultados %>%
    select(starts_with("F"), Porcentaje) %>%
    group_by(Porcentaje) %>%
    summarise(across(starts_with("F"),
                     list(mean = ~mean(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}")) %>%
    rename_with(~gsub("mean", "ω", .)) %>%
    mutate(Porcentaje = as.numeric(as.character(Porcentaje)))

  # Crear el gráfico para la cantidad de factores especificada
  p9 <- ggplot(summary_long, aes(x = Porcentaje, group = 1)) +
    geom_hline(yintercept = 0.70, linetype = "solid", color = "red", size = 0.5) +
    scale_x_reverse(breaks = seq(90, 30, -10)) +
    labs(x = "Percentage", y = "ω values", shape = "Variables", linetype = "Variables") +
    theme_minimal()

  for (i in 1:num_factors) {
    p9 <- p9 +
      geom_line(aes_string(y = paste0("F", i, "_ω"), linetype = paste0("'F", i, "_ω'")), size = 0.5) +
      geom_point(aes_string(y = paste0("F", i, "_ω"), shape = paste0("'F", i, "_ω'")), size = 2)
  }

  # Combinando los gráficos en una figura
  figure3 <- suppressWarnings(ggarrange(p4, p5, p6, p7, p8, p9,
                                        labels = c("A", "B", "C", "D", "E", "F"),
                                        ncol = 3, nrow = 2))

  return(figure3)
}
