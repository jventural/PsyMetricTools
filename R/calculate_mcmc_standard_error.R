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

  # Formatear los valores mínimos y máximos para evitar la notación científica
  min_se <- format(min(se1[se1 > 0]), scientific = FALSE)
  max_se <- format(max(se1), scientific = FALSE)

  # Formatear se1 para evitar la notación científica
  se1_formatted <- format(se1, scientific = FALSE)

  # Devolver una lista con los resultados y el vector completo de se1 formateado
  return(list(min_se = min_se, max_se = max_se, checkvar1 = checkvar1, se1 = se1_formatted))
}
