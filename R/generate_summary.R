#' @title Generate Summary Statistics
#' @description Generates summary statistics for sociodemographic variables.
#' @param data Data frame containing the variables.
#' @param variables Character vector of variable names to summarize.
#' @return A list of summary tables for each variable.
#' @export
generate_summary <- function(data, variables) {
  round_pct_sum_100 <- function(counts) {
    counts <- as.numeric(counts)
    tot <- sum(counts, na.rm = TRUE)
    if (is.na(tot) || tot == 0) return(rep(NA_real_, length(counts)))

    p100 <- counts / tot * 10000
    base <- floor(p100 + 1e-9)
    rem  <- p100 - base
    left <- as.integer(10000 - sum(base))

    if (left > 0) {
      idx <- order(rem, decreasing = TRUE, na.last = TRUE)
      base[idx[seq_len(left)]] <- base[idx[seq_len(left)]] + 1L
    }
    base / 100
  }
  results <- list()

  for (var in variables) {
    if (var != "Edad") {
      summary_table <- data %>%
        dplyr::count(!!rlang::sym(var), name = "n")

      summary_table$Porcentaje <- round_pct_sum_100(summary_table$n)
      results[[var]] <- summary_table
    }
  }

  if ("Edad" %in% variables) {
    age_summary <- data %>%
      dplyr::summarise(
        mean_Edad = mean(as.numeric(Edad), na.rm = TRUE),
        sd_Edad   = sd(as.numeric(Edad),   na.rm = TRUE)
      )
    results[["Edad"]] <- age_summary
  }
  results
}
