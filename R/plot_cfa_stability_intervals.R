plot_cfa_stability_intervals <- function(resultados,
                                         num_factors = 1,
                                         indices_to_plot = "All",   # "All" o vector con nombres de índices
                                         indices_to_exclude = NULL, # Vector con nombres a excluir
                                         fill_color = "skyblue",
                                         alpha_ribbon = 0.2,
                                         rename_factors = NULL,     # Vector nombrado para renombrar, e.g. c("F1"="NuevoF1", "F2"="NuevoF2")
                                         ...) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # 1. Lista completa de índices disponibles
  available_indices <- c(
    "chisq.scaled", "df.scaled", "srmr", "wrmr",
    "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled",
    paste0("F", 1:num_factors)
  )

  # 2. Si se indica "All", usamos todos los índices disponibles
  if (length(indices_to_plot) == 1 && indices_to_plot == "All") {
    indices_to_plot <- available_indices
  }
  # Preservamos el orden en que el usuario los ponga
  indices_to_plot <- unique(indices_to_plot)

  # 3. Excluir índices si se indica alguno
  if (!is.null(indices_to_exclude)) {
    indices_to_exclude <- unique(indices_to_exclude)
    final_indices <- setdiff(indices_to_plot, indices_to_exclude)
  } else {
    final_indices <- indices_to_plot
  }

  # 4. Asegurarnos de que 'Porcentaje' sea numérico
  resultados <- resultados %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  # (Si tus porcentajes son 0.9, 0.8, ... y quieres 90, 80, ... descomenta la siguiente línea)
  # resultados <- resultados %>% mutate(Porcentaje = Porcentaje * 100)

  # 5. Pasar a formato largo sólo las columnas de interés
  resultados_long <- resultados %>%
    pivot_longer(
      cols = all_of(final_indices),
      names_to = "Medida",
      values_to = "Valor"
    )

  # Renombrar los índices F según lo indicado por el usuario
  if (!is.null(rename_factors)) {
    # Convertir a character para poder renombrar
    resultados_long$Medida <- as.character(resultados_long$Medida)
    resultados_long$Medida <- ifelse(resultados_long$Medida %in% names(rename_factors),
                                     rename_factors[resultados_long$Medida],
                                     resultados_long$Medida)
    # Establecer los niveles del factor de acuerdo al orden de final_indices,
    # aplicando el renombrado a las variables F
    new_levels <- sapply(final_indices, function(x) {
      if (x %in% names(rename_factors)) {
        rename_factors[x]
      } else {
        x
      }
    })
    resultados_long$Medida <- factor(resultados_long$Medida, levels = new_levels)
  } else {
    resultados_long <- resultados_long %>%
      mutate(Medida = factor(Medida, levels = final_indices))
  }

  # 6. Calcular intervalos (ej. percentiles 2.5% y 97.5%) y la media
  resultados_summary <- resultados_long %>%
    group_by(Porcentaje, Medida) %>%
    summarise(
      low  = quantile(Valor, probs = 0.025, na.rm = TRUE),
      mid  = mean(Valor, na.rm = TRUE),
      high = quantile(Valor, probs = 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  # 7. Graficar
  p <- ggplot(resultados_summary, aes(x = Porcentaje, y = mid)) +
    geom_line(color = "grey28") +
    geom_point(color = "grey28") +
    geom_ribbon(aes(ymin = low, ymax = high),
                fill = fill_color,
                alpha = alpha_ribbon) +
    facet_wrap(~ Medida, scales = "free_y") +
    theme_minimal() +
    labs(x = "", y = "") +
    # Eje x descendente de 90 a 30 (si tus datos tienen esos valores)
    scale_x_reverse(breaks = c(90, 80, 70, 60, 50, 40, 30))

  return(p)
}
