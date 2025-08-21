generate_summary <- function(data, variables) {
  library(dplyr)
  library(rlang)

  round_pct_sum_100 <- function(counts) {
    counts <- as.numeric(counts)
    tot <- sum(counts, na.rm = TRUE)
    if (is.na(tot) || tot == 0) return(rep(NA_real_, length(counts)))

    # Trabajar en centésimas (100.00% = 10000)
    p100 <- counts / tot * 10000
    base <- floor(p100 + 1e-9)             # parte entera en centésimas
    rem  <- p100 - base                    # residuos a repartir
    left <- as.integer(10000 - sum(base))  # centésimas pendientes

    if (left > 0) {
      idx <- order(rem, decreasing = TRUE, na.last = TRUE)
      base[idx[seq_len(left)]] <- base[idx[seq_len(left)]] + 1L
    }
    base / 100                              # vuelve a 2 decimales
  }

  results <- list()

  for (var in variables) {
    if (var != "Edad") {
      summary_table <- data %>%
        dplyr::count(!!sym(var), name = "n")

      # Porcentajes con suma EXACTA = 100.00 (2 decimales, sin %)
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
