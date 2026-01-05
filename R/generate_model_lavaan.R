#' @name generate_model_lavaan
#' @export
generate_model_lavaan <- function(num_items,
                          item_value,
                          t_values,
                          excluded_items = "none") {
  # Crear nombres de ítems
  item_names <- paste0("Item", 1:num_items)

  # Iniciar la ecuación de análisis factorial
  text <- paste0("F1 =~ ", item_value[1], "*", item_names[1])

  # Agregar los ítems restantes a la ecuación
  for (i in 2:num_items) {
    text <- paste0(text, " + ", item_value[i], "*", item_names[i])
  }

  text <- paste0(text, "\n\nF1 ~~ 1*F1\n\n")

  # Añadir umbrales para cada ítem
  for (i in 1:num_items) {
    for (j in 1:length(t_values)) {
      line <- paste0(item_names[i], " | ", t_values[j], "*t", j)
      item_threshold_key <- paste("Item", i, "t", j, sep = "")

      # Si el ítem está excluido, comentar la línea
      if (excluded_items != "none" && item_threshold_key %in% excluded_items) {
        line <- paste0("# ", line)
      }
      text <- paste0(text, line, "\n")
    }
    text <- paste0(text, "\n")
  }
  return(text)
}
