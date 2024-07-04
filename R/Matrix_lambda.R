Matrix_lambda <- function(n_items, factores, n_tamanos) {
  if (length(n_tamanos) != factores) {
    stop("La cantidad de bloques debe coincidir con el número de columnas.")
  }
  if (sum(n_tamanos) != n_items) {
    stop("La suma de elementos en bloques debe ser igual al número de filas.")
  }

  Lambda <- matrix(0, nrow = n_items, ncol = factores)
  inicio <- 1
  for (i in 1:factores) {
    fin <- inicio + n_tamanos[i] - 1
    Lambda[inicio:fin, i] <- 1
    inicio <- fin + 1
  }

  return(Lambda)
}
