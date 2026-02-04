#' @title Reverse Score Items
#' @description Reverses the scoring of specified items in a data frame.
#' @param df Data frame containing the items.
#' @param items Character vector of item names to reverse.
#' @param num_respuestas Number of response options.
#' @param comienza_con_cero Logical, whether responses start with 0 (default TRUE).
#' @return Data frame with reversed items.
#' @examples
#' \dontrun{
#' # Create sample data with 5-point Likert scale (1-5)
#' set.seed(123)
#' data <- data.frame(
#'   Item1 = sample(1:5, 100, replace = TRUE),
#'   Item2 = sample(1:5, 100, replace = TRUE),
#'   Item3 = sample(1:5, 100, replace = TRUE),  # Item to reverse
#'   Item4 = sample(1:5, 100, replace = TRUE),
#'   Item5 = sample(1:5, 100, replace = TRUE)   # Item to reverse
#' )
#'
#' # Reverse score Items 3 and 5 (scale starts at 1)
#' data_reversed <- invertir_items(
#'   df = data,
#'   items = c("Item3", "Item5"),
#'   num_respuestas = 5,
#'   comienza_con_cero = FALSE
#' )
#'
#' # Check the reversal: 1->5, 2->4, 3->3, 4->2, 5->1
#' head(data[, c("Item3", "Item5")])
#' head(data_reversed[, c("Item3", "Item5")])
#'
#' # Example with 0-4 scale (starts at 0)
#' data2 <- data.frame(
#'   Q1 = sample(0:4, 100, replace = TRUE),
#'   Q2 = sample(0:4, 100, replace = TRUE)
#' )
#'
#' data2_reversed <- invertir_items(
#'   df = data2,
#'   items = "Q1",
#'   num_respuestas = 5,
#'   comienza_con_cero = TRUE
#' )
#' # Check: 0->5, 1->4, 2->3, 3->2, 4->1
#' }
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
