Plot_Likert <- function(Data, name_items, ranges, exclude = NULL) {
  library(tidyverse)
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("tidyverse")
  install_and_load("ggplot2")

  # Crear los nombres de las columnas
  cols <- paste0(name_items, ranges)

  # Remover los ítems a excluir
  if (!is.null(exclude)) {
    exclude_cols <- paste0(name_items, exclude)
    cols <- setdiff(cols, exclude_cols)
  }

  # Comprobar que las columnas existen en el dataframe
  if (!all(cols %in% names(Data))) {
    stop("Algunas columnas especificadas no existen en el dataframe.")
  }

  # Convertir a formato largo
  df_pivot <- Data %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "Variables",
      values_to = "Var_Ptjes"
    ) %>%
    mutate(
      Variables = factor(
        Variables,
        levels = Data %>% select(all_of(cols)) %>% names()
      )
    ) %>%
    select(Variables, Var_Ptjes)

  # Crear el gráfico
  ggplot(df_pivot, aes(x= as.factor(Var_Ptjes))) +
    geom_bar(fill = "#CCD1D1", colour="black") +
    geom_text(stat='count', aes(label=scales::percent(..count../nrow(Data),1)), vjust=-0.1,size = 3)+
    scale_y_continuous(limits=c(0,nrow(Data)))+
    facet_wrap(vars(Variables), scale = "free_x") +
    theme_bw()+
    labs(x = "Tasas de respuesta")+
    labs(y = "")+
    theme(axis.text.y = element_blank())
}
