plot_performance_metrics <- function(averaged_results) {
  # Cargar librerías necesarias
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # Transformar los datos para ggplot
  data_long <- averaged_results %>%
    pivot_longer(cols = avg_sensitivity:avg_bias, names_to = "Metric", values_to = "Value")

  # Crear el gráfico
  plot <- ggplot(data_long, aes(x = Sample_Size, y = Value, color = Correlation_Method)) +
    geom_line(aes(linetype = Correlation_Method), size = 0.8) +
    labs(
      title = "Comparison of Performance Metrics by Algorithm and Correlation Method",
      x = "Sample Size",
      y = "Value"
    ) +
    facet_grid(cols = vars(Metric), rows = vars(Algorithm)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      strip.text = element_text(size = 12)
    ) +
    scale_color_brewer(palette = "Set1")

  # Retornar el objeto plot
  return(plot)
}
