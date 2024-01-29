calculate_mcmc_standard_error <- function(fit1) {
  # Asegúrate de que las dependencias están cargadas
  if (!require(mcmcse)) install.packages("mcmcse")
  library(mcmcse)

  # Aplica la función 'standardizedPosterior' al modelo 'fit1'
  stpost1 <- standardizedPosterior(fit1)
  stpost1 <- as.data.frame(stpost1)

  # Define la función 'calculate_mcmc_standard_error' internamente
  calculate_mcmc_standard_error <- function(stpost1) {
    if (!is.matrix(stpost1) && !is.data.frame(stpost1)) {
      stop("stpost1 debe ser una matriz o un data frame.")
    }

    # Inicializar el vector de error estándar
    se1 <- numeric(ncol(stpost1))
    for (i in 1:ncol(stpost1)) {
      mcse_result <- mcmcse::mcse(stpost1[, i])
      if (is.list(mcse_result)) {
        se1[i] <- mcse_result[[2]] # Asumiendo que el segundo elemento es el SE
      } else {
        se1[i] <- mcse_result
      }
    }

    # Verificar la condición del error estándar
    checkvar1 <- se1 < 0.05 * apply(stpost1, 2, sd)

    # Formatear los valores de se1 para notación científica con dos decimales
    se1_scientific <- sapply(se1, function(x) {
      if (x == 0) {
        return("0")
      } else {
        sci_format <- sprintf("%.2e", x)
        sci_format <- gsub("e-0?", "× 10^-", sci_format, fixed = TRUE)
        sci_format <- gsub("e\\+0?", "× 10^", sci_format, fixed = TRUE)
        return(sci_format)
      }
    })

    # Convertir los valores de se1 a formato decimal completo
    se1_decimal <- sapply(se1, function(x) format(x, scientific = FALSE))

    # Devolver una lista con los resultados y los vectores de se1 en distintos formatos
    return(list(min_se_scientific = min(se1_scientific),
                max_se_scientific = max(se1_scientific),
                se1_scientific = se1_scientific,
                min_se_decimal = min(se1_decimal),
                max_se_decimal = max(se1_decimal),
                se1_decimal = se1_decimal,
                checkvar1 = checkvar1))
  }

  # Llama a la función 'calculate_mcmc_standard_error' con el resultado de 'standardizedPosterior'
  return(calculate_mcmc_standard_error(stpost1))
}
