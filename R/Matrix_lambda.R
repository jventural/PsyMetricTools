#' Create Lambda Matrix for CFA
#'
#' Creates a binary lambda matrix specifying item-factor relationships for CFA models.
#'
#' @param n_items Total number of items.
#' @param factores Number of factors.
#' @param n_tamanos Vector specifying the number of items per factor.
#'
#' @return A matrix with 1s indicating item-factor loadings.
#'
#' @export
Matrix_lambda <- function(n_items, factores, n_tamanos) {
  if (length(n_tamanos) != factores) {
    stop("La cantidad de bloques debe coincidir con el numero de columnas.")
  }
  if (sum(n_tamanos) != n_items) {
    stop("La suma de elementos en bloques debe ser igual al numero de filas.")
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
