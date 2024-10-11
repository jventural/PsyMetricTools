generate_summary <- function(data, variables) {

  # Crear una lista para almacenar los resultados
  results <- list()

  # Calcular el conteo y porcentaje para las variables categóricas
  for (var in variables) {
    if (var != "Edad") {
      summary_table <- data %>%
        count(!!sym(var)) %>%
        mutate(Porcentaje = n / sum(n) * 100)

      # Añadir la tabla a la lista de resultados
      results[[var]] <- summary_table
    }
  }

  # Calcular la media y desviación estándar para la variable Edad
  if ("Edad" %in% variables) {
    age_summary <- data %>%
      summarise(
        mean_Edad = mean(as.numeric(Edad), na.rm = TRUE),
        sd_Edad = sd(as.numeric(Edad), na.rm = TRUE)
      )

    # Añadir la tabla a la lista de resultados
    results[["Edad"]] <- age_summary
  }

  return(results)
}
