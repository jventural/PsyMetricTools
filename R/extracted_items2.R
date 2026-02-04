#' Extract Items by Maximum Loading
#'
#' Assigns items to factors based on their highest loading value.
#'
#' @param data Data frame with Items column and factor loading columns.
#'
#' @return A list with items assigned to each factor.
#'
#' @export
extracted_items2 <- function(data){
  # Excluir columnas no numéricas
  df_numerics_only <- data[sapply(data, is.numeric)]

  # Aplicar which.max a cada fila
  max_indices <- apply(df_numerics_only, 1, which.max)

  # Extraer los items correspondientes
  results <- list()

  for(i in seq_along(max_indices)){
    column <- names(df_numerics_only)[max_indices[i]]
    item <- data$Items[i]

    # Agregar el item a la lista correspondiente
    if(is.null(results[[column]])){
      results[[column]] <- item
    }else{
      results[[column]] <- c(results[[column]], item)
    }
  }

  return(results)
}
