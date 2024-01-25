rename_items <- function(df, prefix1 = "COPE", prefix2 = "E", inici = NULL, final = NULL, n_items1 = NULL, n_items2 = NULL) {
  # Obtener los índices de las variables a renombrar
  if (!is.null(inici)) {
    inici_idx <- which(colnames(df) == inici)
  } else {
    inici_idx <- 1
  }

  if (!is.null(final)) {
    final_idx <- which(colnames(df) == final)
  } else {
    final_idx <- ncol(df)
  }

  n_vars <- final_idx - inici_idx + 1

  # Si no se especifican n_items1 y n_items2, dividir las columnas equitativamente
  if (is.null(n_items1) && is.null(n_items2)) {
    n_items1 <- ceiling(n_vars / 2)
    n_items2 <- n_vars - n_items1
  } else if (!is.null(n_items1) && is.null(n_items2)) {
    n_items2 <- n_vars - n_items1
  } else if (is.null(n_items1) && !is.null(n_items2)) {
    n_items1 <- n_vars - n_items2
  }

  # Crear los vectores de nombres para cada prefijo
  nombres1 <- paste0(prefix1, 1:n_items1)
  nombres2 <- paste0(prefix2, 1:n_items2)

  # Concatenar los vectores de nombres
  nuevos_nombres <- c(nombres1, nombres2)

  # Verificar que el número de columnas a renombrar coincide con el número de nombres nuevos
  if (n_vars != length(nuevos_nombres)) {
    stop("El número de columnas a renombrar no coincide con el número de nombres nuevos.")
  }

  # Verificar que el número de columnas a renombrar es igual al número de columnas que existen
  if (final_idx > ncol(df)) {
    stop("El número de columnas a renombrar es mayor que el número de columnas que existen.")
  }

  # Renombrar las variables del data frame
  colnames(df)[inici_idx:final_idx] <- nuevos_nombres

  # Retornar el data frame con las variables renombradas
  return(df)
}
