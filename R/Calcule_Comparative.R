Calcule_Comparative <- function(data, cols, group_var, Robust = FALSE) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("forcats")
  install_and_load("dplyr")
  install_and_load("tidyr")
  install_and_load("broom")
  if (Robust) {
    install_and_load("WRS2")
  } else {
    install_and_load("effectsize")
  }

  # Cálculo comparativo
  group_values <- as.character(unique(data[[group_var]]))

  data %>%
    pivot_longer(
      cols = cols,
      names_to = "Variables_interes",
      values_to = "Ptje_vi",
      values_drop_na = FALSE
    ) %>%
    mutate(Variables_interes = fct_inorder(Variables_interes)) %>%
    group_by(Variables_interes) %>%
    summarise(
      sd_1 = sd(Ptje_vi[data[[group_var]] == group_values[1]]),
      sd_2 = sd(Ptje_vi[data[[group_var]] == group_values[2]]),
      const = 1
    ) %>%
    left_join(
      data %>%
        pivot_longer(
          cols = cols,
          names_to = "Variables_interes",
          values_to = "Ptje_vi",
          values_drop_na = FALSE
        ) %>%
        mutate(Variables_interes = fct_inorder(Variables_interes)) %>%
        group_by(Variables_interes) %>%
        summarise(
          broom::tidy(t.test(Ptje_vi ~ data[[group_var]], var.equal = FALSE)),
          d_cohen = if (Robust) {
            WRS2::akp.effect(Ptje_vi ~ data[[group_var]], EQVAR = FALSE)$AKPeffect
          } else {
            effectsize::cohens_d(Ptje_vi ~ data[[group_var]])$Cohens_d
          }
        ) %>%
        dplyr::select(
          Variables_interes, estimate, estimate1, estimate2, statistic, p.value, d_cohen, statistic, parameter
        ) %>%
        rename(M_1 = estimate1, M_2 = estimate2, Diff = estimate, gl = parameter, t = statistic, p = p.value)
    ) %>%
    mutate(
      M1_SD1 = paste0(round(M_1, 2), " (", round(sd_1, 2), ")"),
      M2_SD2 = paste0(round(M_2, 2), " (", round(sd_2, 2), ")")
    ) %>%
    rename(
      `M1(SD1)` = M1_SD1,
      `M2(SD2)` = M2_SD2
    ) %>%
    mutate(
      Interpretacion = case_when(
        abs(d_cohen) > 0.80 ~ "Grande",
        abs(d_cohen) > 0.50 ~ "Mediano",
        abs(d_cohen) > 0.30 ~ "Pequeño",
        TRUE ~ "Trivial"
      )
    ) %>%
    dplyr::select(
      Variables_interes, `M1(SD1)`, `M2(SD2)`, t, gl, p, d_cohen, Interpretacion
    ) %>%
    rename_with(
      ~str_replace_all(.x, "M1", group_values[1]),
      starts_with("M1")
    ) %>%
    rename_with(
      ~str_replace_all(.x, "M2", group_values[2]),
      starts_with("M2")
    )
}
