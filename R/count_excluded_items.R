#' @name count_excluded_items
#' @export
count_excluded_items <- function(res) {
  todos_items <- c()  # Vector para almacenar todos los elementos de excluded_items

  # Asegurarse de que estamos accediendo a 'processed_results'
  if ("processed_results" %in% names(res)) {
    # Iteramos sobre 'processed_results' para obtener todos los elementos de excluded_items
    for (i in 1:length(res$processed_results)) {
      todos_items <- c(todos_items, res$processed_results[[i]]$excluded_items)
    }
  } else {
    stop("La estructura de 'res' no contiene 'processed_results', verifica la estructura de datos.")
  }

  # Contamos los elementos repetidos
  conteo_repetidos <- table(todos_items)

  # Convertimos el resultado en un data frame y lo ordenamos
  df_resultado <- tibble(item = names(conteo_repetidos), conteo = as.numeric(conteo_repetidos))
  df_resultado <- df_resultado %>% arrange(desc(conteo))

  # Devolvemos el data frame ordenado
  return(df_resultado)
}
