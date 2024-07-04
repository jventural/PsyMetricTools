boot_and_evaluate <- function(data, true_network, algorithms, correlation_methods,
                              sample_sizes = c(100, 250, 500, 1000), n_simulations = 100, seed = 123) {

  library(EGAnet)
  library(dplyr)
  library(psych)
  library(pbapply)
  library(bootnet)
  library(ggplot2)
  library(tidyverse)
  # Funciones auxiliares para calcular la correlación y el sesgo

  # Función modificada para calcular la correlación entre matrices aplanadas
  cor0 <- function(matrix1, matrix2, ...) {
    # Aplanar las matrices a vectores
    vector1 <- as.vector(matrix1)
    vector2 <- as.vector(matrix2)

    # Calcular la correlación entre los vectores
    if (sum(!is.na(vector1)) < 2 || sum(!is.na(vector2)) < 2 || sd(vector1, na.rm = TRUE) == 0 || sd(vector2, na.rm = TRUE) == 0) {
      return(0)
    } else {
      return(cor(vector1, vector2, ...))
    }
  }

  # Función para calcular el sesgo
  bias <- function(x, y) mean(abs(x - y), na.rm = TRUE)

  # Función para comparar métricas de red
  comparison_metrics <- function(real, est, name = "comparison") {
    # Calcular medidas de desempeño
    TruePos <- sum(est != 0 & real != 0)
    FalsePos <- sum(est != 0 & real == 0)
    TrueNeg <- sum(est == 0 & real == 0)
    FalseNeg <- sum(est == 0 & real != 0)

    FDR <- FalsePos / (TruePos + FalsePos)

    # Compilar resultados
    out <- list(
      sensitivity = TruePos / (TruePos + FalseNeg),
      specificity = TrueNeg / (TrueNeg + FalsePos),
      precision = TruePos / (TruePos + FalsePos),
      FDR = FDR,
      correlation = cor0(est, real),  # Asegurarse de que est y real son las matrices de adyacencia
      abs_cor = cor0(abs(est), abs(real)),  # Correlación entre valores absolutos
      bias = bias(est, real),
      truePos = TruePos,
      falsePos = FalsePos,
      trueNeg = TrueNeg,
      falseNeg = FalseNeg
    )

    if (name != "") {
      names(out) <- paste0(names(out), "_", name)
    }
    out
  }

  # Función para evaluar el rendimiento de la red estimada
  performance <- function(real_network, estimated_network) {
    # Obtener los resultados mediante la función comparison_metrics
    results <- comparison_metrics(real = as.matrix(real_network$network), est = as.matrix(estimated_network$network))

    # Convertir los resultados en un dataframe y transponer
    results_df <- as.data.frame(t(unlist(results)))

    # Redondear todos los valores numéricos a dos decimales
    results_df <- round(results_df, 2)

    # Seleccionar solo las columnas de interés
    selected_columns <- c("sensitivity_comparison", "specificity_comparison", "precision_comparison",
                          "correlation_comparison", "abs_cor_comparison", "bias_comparison", "FDR_comparison")

    # Subconjunto del dataframe con solo las columnas seleccionadas
    results_df <- results_df[, selected_columns]

    # Remover el sufijo '_comparison' de los nombres de las columnas
    names(results_df) <- gsub("_comparison", "", names(results_df))

    # Retornar solo las columnas seleccionadas sin el sufijo
    return(results_df)
  }

  results_list <- list()
  set.seed(seed)

  # Usar pblapply para agregar una barra de progreso
  results_list <- pblapply(1:n_simulations, function(simulation) {
    simulation_results <- list()
    for (sample_size in sample_sizes) {
      sampled_data <- data %>% sample_frac(sample_size / nrow(data), replace = TRUE)

      for (algorithm in algorithms) {
        for (cor_method in correlation_methods) {
          tryCatch({
            if (algorithm == "leiden") {
              ega_result <- EGA(data = sampled_data, algorithm = algorithm, corr = cor_method,
                                objective_function = "CPM", resolution_parameter = 0.05,
                                plot.EGA = FALSE)
            } else {
              ega_result <- EGA(data = sampled_data, algorithm = algorithm, corr = cor_method, plot.EGA = FALSE)
            }

            if (!is.null(ega_result)) {
              performance_results <- performance(true_network, ega_result)
              performance_results$Algorithm <- algorithm
              performance_results$Correlation_Method <- cor_method
              performance_results$Simulation <- simulation
              performance_results$Sample_Size <- sample_size
              performance_results$TEFI <- ega_result$TEFI  # Asumiendo que TEFI está directamente disponible en ega_result
              simulation_results[[paste(algorithm, cor_method, sample_size, simulation)]] <- performance_results
            }
          }, error = function(e) {
            message("Error with ", algorithm, " using ", cor_method, " in simulation ", simulation, " with sample size ", sample_size, ": ", e$message)
          })
        }
      }
    }
    bind_rows(simulation_results, .id = "Model_ID")
  })

  results_df <- bind_rows(results_list)
  return(results_df)
}
