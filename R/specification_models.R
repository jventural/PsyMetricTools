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
    fit.original = cfa(paste0(modelos[i]),
                       data = data,
                       estimator = estimator,
                       rotation = "oblimin",
                       mimic = "Mplus",
                       ordered = TRUE)
    AD[[i]] = fit.original
    print(fit.original)
  }

  return(AD)
}
