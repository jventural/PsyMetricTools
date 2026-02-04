#' Create Fit Index Table
#'
#' Creates a comprehensive table combining fit indices and reliability measures.
#'
#' @param resultados A results object with bondades_ajuste and fiabilidad components.
#'
#' @return A data frame with model fit indices and omega reliability values.
#' @examples
#' \dontrun{
#' # First run multi_cfa to get results
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE)
#' )
#'
#' models <- list(
#'   "F1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6",
#'   "F1 =~ Item1 + Item2 + Item3\nF2 =~ Item4 + Item5 + Item6"
#' )
#'
#' results <- multi_cfa(modelos = models, data = data,
#'                      estimator = "WLSMV", ordered = TRUE)
#'
#' # Create fit index table combining fit measures and reliability
#' fit_table <- fit_index_Table(results)
#' print(fit_table)
#' # Output:
#' # Model   x2   df  SRMR   CFI   TLI  RMSEA  CRMR omega_F1 omega_F2
#' # 1      ...  ...  ...   ...   ...   ...    ...   0.78     NA
#' # 2      ...  ...  ...   ...   ...   ...    ...   0.75     0.80
#' }
#' @export
fit_index_Table <- function(resultados) {
  # Capturar y suprimir mensajes temporales durante la ejecucion del codigo
  suppressMessages({
    suppressWarnings({
      # Convertir los resultados de bondades de ajuste en un tibble
      bondades_ajuste <- resultados$bondades_ajuste %>%
        as_tibble() %>%
        tibble::rownames_to_column("Model") %>%
        select(-wrmr)

      # Convertir los resultados de fiabilidad en un tibble
      fiabilidad <- resultados$fiabilidad %>%
        map_dfr(~ as_tibble(as.data.frame(t(as.numeric(.)))), .id = "Model") %>%
        rename_with(~ gsub("^V", "F", .x)) %>%
        rename_with(~ paste0("omega_", .), -Model) %>%
        mutate(Model = row_number()) %>% mutate(Model = as.character(Model))

      # Unir los dos tibbles por la columna "Model"
      resultado_final <- left_join(bondades_ajuste, fiabilidad)

      # Renombrar las columnas para que coincidan con el formato deseado
      resultado_final <- resultado_final %>%
        rename(
          Model = Model,
          x2 = chisq.scaled,
          df = df.scaled,
          SRMR = srmr,
          CFI = cfi.scaled,
          TLI = tli.scaled,
          RMSEA = rmsea.scaled,
          CRMR = crmr
        )
    })
  })

  # Retornar el resultado final
  return(resultado_final)
}

