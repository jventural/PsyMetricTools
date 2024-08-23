Table_fit_index <- function(resultados) {
  # Capturar y suprimir mensajes temporales durante la ejecución del código
  suppressMessages({
    suppressWarnings({
      # Convertir los resultados de bondades de ajuste en un tibble
      bondades_ajuste <- resultados$bondades_ajuste %>%
        as_tibble() %>%
        tibble::rownames_to_column("Model")

      # Convertir los resultados de fiabilidad en un tibble
      fiabilidad <- resultados$fiabilidad %>%
        map_df(~ tibble(ω = .), .id = "Model") %>% select(ω)

      # Unir los dos tibbles por la columna "Model"
      resultado_final <- bind_cols(bondades_ajuste, fiabilidad)

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
          CRMR = crmr,
          ω = ω
        )
    })
  })

  # Retornar el resultado final
  return(resultado_final)
}
