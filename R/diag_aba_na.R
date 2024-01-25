diag_aba_na <- function(matriz){
  # Función para modificar los encabezados
  cambiar_encabezados <- function(matriz) {
    # Nuevos encabezados
    nuevos_encabezados <- seq_len(ncol(matriz))

    # Encabezados originales de las filas
    encabezados_filas_originales <- rownames(matriz)

    # Nuevos encabezados de las filas
    nuevos_encabezados_filas <- paste0(seq_along(encabezados_filas_originales), ". ", encabezados_filas_originales)

    # Asignar los nuevos encabezados
    colnames(matriz) <- nuevos_encabezados
    rownames(matriz) <- nuevos_encabezados_filas

    return(matriz)
  }
  matriz <- cambiar_encabezados(matriz)
  diag(matriz) <- "-"
  matriz[upper.tri(matriz)] <- NA
  return(matriz)
}
