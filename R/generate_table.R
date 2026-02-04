#' @title Generate Summary Table
#' @description Converts summary results into a formatted table.
#' @param summary_results List of summary results from generate_summary.
#' @return A data frame formatted as a summary table.
#' @export
generate_table <- function(summary_results) {
  rows <- list()
  ordered_vars <- c("Sexo", "Edad", setdiff(names(summary_results), c("Sexo", "Edad")))

  for (var in ordered_vars) {
    if (var == "Edad") {
      mean_val <- round(as.numeric(summary_results[[var]]$mean_Edad), 2)
      sd_val   <- round(as.numeric(summary_results[[var]]$sd_Edad), 2)
      rows[[length(rows) + 1]] <- c("Edad (mean, SD)", paste0(mean_val, " (", sd_val, ")"), "")
    } else {
      rows[[length(rows) + 1]] <- c(var, "", "")
      df_var <- summary_results[[var]]
      for (i in seq_len(nrow(df_var))) {
        category <- as.character(df_var[[1]][i])
        count    <- as.character(df_var[["n"]][i])
        pct      <- as.character(round(as.numeric(df_var[["Porcentaje"]][i]), 2))  # sin '%'
        rows[[length(rows) + 1]] <- c(category, count, pct)
      }
    }
  }

  table_matrix <- do.call(rbind, rows)
  colnames(table_matrix) <- c("Variables sociodemograficas", "n", "%")
  df_table <- as.data.frame(table_matrix, stringsAsFactors = FALSE)

  # IMPORTANTE: no convertir 'n' a numerico para no perder "mean (sd)"
  # Si quieres, puedes convertir SOLO la columna '%' a numerica:
  # suppressWarnings(df_table[["%"]] <- as.numeric(df_table[["%"]]))

  df_table
}
