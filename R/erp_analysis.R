#' @title Extreme-Response Proportion by Item
#' @description Computes the Extreme-Response Proportion (ERP) for each item in
#'   a Likert-type scale. ERP is the proportion of respondents who selected
#'   either the lowest or highest response option, which can signal floor/ceiling
#'   effects or poor item discrimination.
#' @param data A data frame containing the item responses.
#' @param items Character vector of column names to analyse.
#' @param extremes Numeric vector of length 2 indicating the lowest and highest
#'   anchor values (default \code{c(1, 5)}).
#' @param threshold Numeric threshold above which an item is flagged
#'   (default 0.30).
#' @return A data frame with one row per item containing: \code{Item}, \code{n},
#'   \code{p_low}, \code{p_high}, \code{ERP}, \code{Median}, \code{IQR},
#'   \code{Flag_ERP}, and formatted percentage columns. Rows are sorted by
#'   descending ERP.
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   IT1 = sample(1:5, 200, replace = TRUE),
#'   IT2 = sample(1:5, 200, replace = TRUE),
#'   IT3 = sample(1:5, 200, replace = TRUE)
#' )
#' erp <- erp_by_item(df, c("IT1", "IT2", "IT3"))
#' print(erp)
#' }
#' @importFrom dplyr bind_rows mutate arrange desc
#' @importFrom stats median IQR
#' @importFrom scales percent
#' @export
erp_by_item <- function(data, items, extremes = c(1, 5), threshold = 0.30) {
  items <- items[items %in% names(data)]
  if (length(items) == 0) stop("None of the specified items were found in data.")

  res <- lapply(items, function(col) {
    x <- as.numeric(data[[col]])
    n <- sum(!is.na(x))
    p_low  <- mean(x == extremes[1], na.rm = TRUE)
    p_high <- mean(x == extremes[2], na.rm = TRUE)
    erp <- p_low + p_high
    data.frame(
      Item     = col,
      n        = n,
      p_low    = p_low,
      p_high   = p_high,
      ERP      = erp,
      Median   = stats::median(x, na.rm = TRUE),
      IQR      = stats::IQR(x, na.rm = TRUE),
      Flag_ERP = erp >= threshold,
      stringsAsFactors = FALSE
    )
  })

  res <- dplyr::bind_rows(res)
  res <- dplyr::mutate(
    res,
    p_low_pct  = scales::percent(p_low, 0.1),
    p_high_pct = scales::percent(p_high, 0.1),
    ERP_pct    = scales::percent(ERP, 0.1)
  )
  res <- dplyr::arrange(res, dplyr::desc(ERP))
  res
}


#' @title Plot Extreme-Response Proportion
#' @description Creates a horizontal bar chart of ERP values per item, with a
#'   dashed reference line at the chosen threshold. Items exceeding the threshold
#'   are visually distinguished by fill colour.
#' @param erp_table A data frame produced by \code{\link{erp_by_item}}.
#' @param threshold Numeric threshold for the reference line (default 0.30).
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   IT1 = sample(1:5, 200, replace = TRUE),
#'   IT2 = sample(1:5, 200, replace = TRUE),
#'   IT3 = sample(1:5, 200, replace = TRUE)
#' )
#' erp <- erp_by_item(df, c("IT1", "IT2", "IT3"))
#' plot_erp(erp)
#' }
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip
#'   scale_y_continuous labs theme_minimal guides
#' @importFrom scales percent_format
#' @export
plot_erp <- function(erp_table, threshold = 0.30) {
  ggplot2::ggplot(erp_table,
                  ggplot2::aes(x = stats::reorder(erp_table$Item, erp_table$ERP),
                               y = ERP, fill = Flag_ERP)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = threshold, linetype = 2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(x = "Item", y = "ERP",
                  title = "Extreme-Response Proportion per item") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none")
}
