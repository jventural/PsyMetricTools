plot_avg_TEFI <- function(results) {
  # Cargar las librerías necesarias
  library(dplyr)
  library(ggplot2)

  # Agrupar datos y calcular la mediana de TEFI para cada grupo
  summarised_results <- results %>%
    group_by(Algorithm, Correlation_Method, Sample_Size) %>%
    summarise(
      avg_TEFI = median(TEFI, na.rm = TRUE),
      .groups = 'drop'
    )

  # Crear el gráfico usando ggplot2
  plot <- ggplot(summarised_results, aes(x = Sample_Size, y = avg_TEFI, group = Algorithm, color = Algorithm, shape = Algorithm)) +
    geom_line() + # Líneas para conectar puntos
    geom_point() + # Puntos en cada dato
    scale_x_continuous(breaks = unique(summarised_results$Sample_Size), labels = unique(summarised_results$Sample_Size)) + # Ajustar las marcas y etiquetas del eje x
    labs(title = "Average TEFI by Sample Size",
         x = "Sample Size",
         y = "Average TEFI",
         color = "Algorithm") +
    theme_minimal() +
    facet_wrap(~Correlation_Method) # Organizar gráficos por método de correlación

  # Retornar el objeto plot
  return(plot)
}
