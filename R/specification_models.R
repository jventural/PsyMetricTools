specification_models <- function(modelos, data, estimator) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar la librería requerida
  install_and_load("lavaan")

  AD <- list()

  for (i in 1:length(modelos)) {
    # Suprimir mensajes y advertencias para cada modelo ajustado
    suppressMessages(suppressWarnings({
      fit.original = cfa(paste0(modelos[i]),
                         data = data,
                         estimator = estimator,
                         rotation = "oblimin",
                         mimic = "Mplus",
                         ordered = TRUE, verbose = FALSE)
    }))
    AD[[i]] = fit.original
    # Si necesitas imprimir algo específico aquí, asegúrate de que no contradiga tu objetivo de supresión
  }

  return(AD)
}

