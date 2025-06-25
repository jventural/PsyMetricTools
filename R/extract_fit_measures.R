extract_fit_measures <- function(Specifications) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("lavaan")
  install_and_load("tidyverse")

  CD <- list()

  for (i in 1:length(Specifications)) {
    bondad_modelo <- fitMeasures(Specifications[[i]], c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))
    CD[[i]] <- bondad_modelo
  }

  Bondades_Original <- map_dfr(CD, bind_rows) %>% as.data.frame() %>% round(3) %>%
    mutate(Factores = rep(paste0("f", 1:length(Specifications)))) %>% relocate(Factores, .before = chisq.scaled)

  return(Bondades_Original)
}

