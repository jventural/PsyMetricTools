invertir_items <- function(df, items, num_respuestas, comienza_con_cero = TRUE) {
  df %>%
    mutate(across(all_of(items), function(x) {
      if (comienza_con_cero) {
        # Si las respuestas comienzan con 0, el máximo valor es num_respuestas - 1
        max_val = num_respuestas - 1
        return(max_val + 1 - x)
      } else {
        # Si las respuestas comienzan con 1, el máximo valor es num_respuestas
        max_val = num_respuestas
        return(max_val + 1 - x)
      }
    }))
}
