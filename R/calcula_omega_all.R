calcula_omega_all <- function(extracted, data) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("tidyverse")
  install_and_load("lavaan")
  install_and_load("semTools")
  install_and_load("dplyr")

  # Initialize the results dataframe
  resultados <- data.frame(Variables = character(),
                           Omega = numeric())

  # Internal function to calculate reliability
  fiabilidad_interna <- function(vars, data) {
    # Extract the values of the variables in 'vars' from the data frame 'data'.
    temp_data <- data %>% select(all_of(vars))

    # Create the model using the variable names in 'vars'.
    model_original <- paste("F1 =~", paste0(vars, collapse = " + "))

    # Estimating the model
    fit.original <- cfa(model_original, data = temp_data, estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)

    # Calculate composite reliability
    resultado <- compRelSEM(fit.original, tau.eq = F, ord.scale = T)
    return(resultado)
  }

  # Iterate over the keys (variable names) in the object 'extracted'.
  for (key in names(extracted)) {
    vars <- extracted[[key]]
    omega <- fiabilidad_interna(vars = vars, data = data)
    resultados <- rbind(resultados, data.frame(Variables = key, Omega = omega))
  }
  rownames(resultados) <- NULL

  # Convert Omega column to numeric
  resultados$Omega <- as.numeric(resultados$Omega)

  return(resultados)
}

