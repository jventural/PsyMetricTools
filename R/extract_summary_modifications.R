#' Summarize Modification Indices Across Replications
#'
#' Creates summary statistics for modification indices across bootstrap samples.
#'
#' @param combined_results Combined data frame of modification indices.
#' @param min_count Minimum count threshold for inclusion (default: 10).
#'
#' @return A summary data frame with mean, SD, min, and max MI values.
#'
#' @export
extract_summary_modifications <- function(combined_results, min_count = 10) {
  summary_df <- combined_results %>%
    group_by(Modification) %>%
    summarise(
      count = n(),
      promedio_MI = mean(MI),
      desviacion_estandar_MI = sd(MI),
      minimo_MI = min(MI),
      maximo_MI = max(MI)
    ) %>%
    filter(count > min_count) %>%
    arrange(desc(promedio_MI))

  return(summary_df)
}
