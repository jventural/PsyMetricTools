#' @name fit_index_Table
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

