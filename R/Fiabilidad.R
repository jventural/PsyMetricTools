Fiabilidad <- function(vars, data) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("lavaan")
  install_and_load("semTools")

  # Extraer los valores de las variables en 'vars' del data frame 'data'
  temp_data <- data %>% select(all_of(vars))

  # Crear el modelo utilizando los nombres de las variables en 'vars'
  model_original <- paste("F1 =~", paste0(vars, collapse = " + "))

  # Estimar el modelo
  fit.original <- cfa(model_original, data = temp_data, estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)

  # Calcular la fiabilidad compuesta
  resultado <- compRelSEM(fit.original, tau.eq = F, ord.scale = T)
  return(resultado)
}

