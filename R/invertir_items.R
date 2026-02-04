#' @title Reverse Score Items
#' @description Reverses the scoring of specified items in a data frame.
#' @param df Data frame containing the items.
#' @param items Character vector of item names to reverse.
#' @param num_respuestas Number of response options.
#' @param comienza_con_cero Logical, whether responses start with 0 (default TRUE).
#' @return Data frame with reversed items.
#' @export
invertir_items <- function(df, items, num_respuestas, comienza_con_cero = TRUE) {
  df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(items), function(x) {
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
