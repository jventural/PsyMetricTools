filtrar_aberrantes <- function(data, items, plot = FALSE) {
  library(careless)
  library(dplyr)
  library(tibble)
  # Asegurarse de que la data tenga una columna de ID
  if(!"ID" %in% colnames(data)) {
    data <- data %>% rownames_to_column("ID")
  }

  # Seleccionar los ítems específicos y ejecutar mahad
  mahad_flags <- mahad(data %>% select(all_of(items)), flag = TRUE, confidence = 0.999, plot = plot)

  # Identificar los IDs con patrones aberrantes
  ids_aberrantes <- mahad_flags %>%
    rownames_to_column("ID") %>%
    filter(flagged == TRUE) %>%
    pull(ID)

  # Crear data sin los casos aberrantes
  data_filtrada <- data %>%
    filter(!ID %in% ids_aberrantes)

  # Preparar selección de columnas para la tabla de casos marcados
  columnas_seleccionadas <- c("ID", items, "d_sq", "flagged")

  # Crear tabla de casos marcados con d_sq, flagged y los ítems específicos
  tabla_aberrantes <- mahad_flags %>%
    rownames_to_column("ID") %>%
    inner_join(data, by = "ID") %>%
    select(all_of(columnas_seleccionadas))

  # Retornar la data filtrada y la tabla de casos aberrantes
  return(list(data_filtrada = data_filtrada, tabla_aberrantes = tabla_aberrantes))
}
