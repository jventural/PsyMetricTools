Plot_Likert <- function(Data, name_items, ranges, exclude = NULL, text_size = 3) {
  # Carga los paquetes necesarios
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    install.packages("tidyr")
  }
  library(ggplot2)
  library(dplyr)
  library(tidyr)

  # Construir nombres de columnas basados en los rangos especificados
  cols <- paste0(name_items, ranges)

  # Excluir columnas si se especifica
  if (!is.null(exclude)) {
    exclude_cols <- paste0(name_items, exclude)
    cols <- setdiff(cols, exclude_cols)
  }

  # Verificar que todas las columnas especificadas existan en el dataframe
  if (!all(cols %in% names(Data))) {
    stop("Algunas columnas especificadas no existen en el dataframe.")
  }

  # Transformar datos para el gráfico
  df_pivot <- Data %>%
    pivot_longer(cols = all_of(cols), names_to = "Variables", values_to = "Var_Ptjes") %>%
    mutate(Variables = factor(Variables, levels = select(Data, all_of(cols)) %>% names())) %>%
    select(Variables, Var_Ptjes)

  # Generar el gráfico
  ggplot(df_pivot, aes(x = as.factor(Var_Ptjes))) +
    geom_bar(fill = "#CCD1D1", colour = "black") +
    geom_text(stat = "count", aes(label = scales::percent(..count../nrow(Data), accuracy = 0.01)),
              vjust = -0.1, size = text_size) + # Usar el parámetro text_size aquí
    scale_y_continuous(limits = c(0, nrow(Data))) +
    facet_wrap(~ Variables, scale = "free_x") +
    theme_bw() +
    labs(x = "Tasas de respuesta", y = "") +
    theme(axis.text.y = element_blank())
}
