#' @name extract_fit_measures
#' @export
extract_fit_measures <- function(Specifications) {
  # Verificar que los paquetes requeridos estén instalados
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required but not installed. Please install it with install.packages('lavaan')")
  }

  CD <- list()

  for (i in 1:length(Specifications)) {
    bondad_modelo <- lavaan::fitMeasures(Specifications[[i]], c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))
    CD[[i]] <- bondad_modelo
  }

  Bondades_Original <- purrr::map_dfr(CD, dplyr::bind_rows) %>% as.data.frame() %>% round(3) %>%
    dplyr::mutate(Factores = rep(paste0("f", 1:length(Specifications)))) %>% dplyr::relocate(Factores, .before = chisq.scaled)

  return(Bondades_Original)
}

