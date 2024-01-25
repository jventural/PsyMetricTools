normality_test_SW <- function(data, variables) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("dplyr")
  install_and_load("tidyr")
  install_and_load("broom")

  check_normality <- function(p_value) {
    if (p_value < 0.05) {
      return("No-normal")
    } else {
      return("Normal")
    }
  }

  result <- data %>%
    pivot_longer(
      cols = {{variables}},
      names_to = "Variables",
      values_to = "Ptje_vi"
    ) %>%
    mutate(Variables = factor(Variables, levels = select(data, {{variables}}) %>% names())) %>%
    group_by(Variables) %>%
    summarise(
      `Shapiro-Wilk` = broom::tidy(shapiro.test(Ptje_vi))$statistic,
      p.value = ifelse(broom::tidy(shapiro.test(Ptje_vi))$p.value < 0.001, "p < .001", format(broom::tidy(shapiro.test(Ptje_vi))$p.value, scientific = FALSE)),
      Normality = check_normality(as.numeric(broom::tidy(shapiro.test(Ptje_vi))$p.value))
    )

  return(result)
}
