specification_models <- function(modelos, data, estimator, rotation = "oblimin", verbose = FALSE) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar la librería requerida
  install_and_load("lavaan")

  specification <- list()

  for (i in 1:length(modelos)) {
    fit.original = cfa(paste0(modelos[i]),
                       data = data,
                       estimator = estimator,
                       rotation = rotation,
                       mimic = "Mplus",
                       ordered = TRUE,
                       verbose = verbose)  # Usar el argumento verbose aquí
    specification[[i]] = fit.original

    if (verbose) {  # Si verbose es TRUE, imprimir el modelo
      print(fit.original)
    }
  }

  return(specification)
}
