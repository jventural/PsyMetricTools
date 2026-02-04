#' @name extract_summary_modifications
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
