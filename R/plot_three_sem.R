plot_three_sem <- function(models,
                           titles = NULL,
                           layout      = "tree2",
                           rotation    = 2,
                           whatLabels  = "std",
                           residuals   = FALSE,
                           intercepts  = FALSE,
                           thresholds  = FALSE,
                           # Parámetros de tamaño de elementos del gráfico
                           sizeMan     = 10,
                           sizeMan2    = 5,
                           sizeLat     = 12,
                           label.cex   = 1,
                           edge.label.cex = 2,
                           edge.color  = "grey40",
                           color       = list(lat="grey80", man="grey90"),
                           mar         = c(4,4,4,4),
                           outerMar    = c(1,1,3,1),
                           # Nuevos parámetros para índices de bondad de ajuste
                           show_fit_indices = TRUE,
                           fit_indices = c("cfi", "tli", "rmsea", "srmr"),
                           use_scaled = FALSE,
                           model_descriptions = NULL,
                           custom_titles = NULL,
                           # Parámetro para controlar el tamaño del texto de los índices
                           title.cex = 0.8,
                           title.font = 2,
                           # Parámetros para guardar archivo
                           save_plot = FALSE,
                           filename = "sem_models_plot",
                           file_format = "png",
                           width = 12,
                           height = 4,
                           dpi = 300,
                           units = "in") {
  library(semPlot)

  n <- length(models)

  # Si no se proporcionan títulos personalizados, crear títulos básicos
  if (is.null(titles) && is.null(custom_titles)) {
    titles <- paste("Modelo", seq_len(n))
  }

  # Abrir dispositivo gráfico si se va a guardar
  if (save_plot) {
    if (file_format == "png") {
      png(filename = paste0(filename, ".png"),
          width = width, height = height, units = units, res = dpi)
    } else if (file_format == "pdf") {
      pdf(file = paste0(filename, ".pdf"),
          width = width, height = height)
    } else if (file_format == "tiff") {
      tiff(filename = paste0(filename, ".tiff"),
           width = width, height = height, units = units, res = dpi, compression = "lzw")
    } else if (file_format == "jpeg") {
      jpeg(filename = paste0(filename, ".jpg"),
           width = width, height = height, units = units, res = dpi, quality = 100)
    }
  }

  # 1 fila x n columnas
  par(mfrow = c(1, n), mar = outerMar)

  for (i in seq_len(n)) {
    # Crear el gráfico
    semPaths(models[[i]],
             whatLabels     = whatLabels,
             layout         = layout,
             rotation       = rotation,
             residuals      = residuals,
             intercepts     = intercepts,
             thresholds     = thresholds,
             sizeMan        = sizeMan,
             sizeMan2       = sizeMan2,
             sizeLat        = sizeLat,
             label.cex      = label.cex,
             edge.label.cex = edge.label.cex,
             edge.color     = edge.color,
             color          = color,
             mar            = mar)

    # Crear el título con o sin índices de bondad de ajuste
    if (show_fit_indices) {
      # Preparar nombres de índices (con .scaled si es necesario)
      indices_to_extract <- if (use_scaled) {
        sapply(fit_indices, function(x) {
          if (x %in% c("cfi", "tli", "rmsea")) {
            paste0(x, ".scaled")
          } else {
            x
          }
        })
      } else {
        fit_indices
      }

      # Extraer índices de bondad de ajuste del modelo actual
      fit_measures <- fitMeasures(models[[i]], indices_to_extract)

      # Crear descripción del modelo
      if (!is.null(model_descriptions)) {
        model_desc <- model_descriptions[i]
      } else {
        model_desc <- sprintf("Modelo %d", i)
      }

      # Crear título con formato de dos líneas
      if (length(fit_measures) >= 2) {
        # Primera línea: descripción del modelo
        line1 <- model_desc

        # Segunda línea: índices separados por " | "
        indices_text <- sapply(seq_along(fit_measures), function(j) {
          index_name <- if (use_scaled) {
            # Remover .scaled del nombre para mostrar
            gsub("\\.scaled$", "", names(fit_measures)[j])
          } else {
            names(fit_measures)[j]
          }
          sprintf("%s=%.3f", toupper(index_name), fit_measures[j])
        })
        line2 <- paste(indices_text, collapse = " | ")

        title_text <- paste(line1, line2, sep = "\n")
      } else {
        # Si solo hay un índice
        index_name <- if (use_scaled) {
          gsub("\\.scaled$", "", names(fit_measures)[1])
        } else {
          names(fit_measures)[1]
        }
        title_text <- sprintf("%s\n%s=%.3f",
                              model_desc,
                              toupper(index_name),
                              fit_measures[1])
      }
    } else {
      # Usar títulos proporcionados o títulos personalizados
      if (!is.null(custom_titles)) {
        title_text <- custom_titles[i]
      } else {
        title_text <- titles[i]
      }
    }

    title(title_text, line = 1, cex.main = title.cex, font.main = title.font)
  }

  # Restaurar dispositivo a 1 gráfico
  par(mfrow = c(1,1))

  # Cerrar dispositivo gráfico si se guardó archivo
  if (save_plot) {
    dev.off()
    cat("Gráfico guardado como:", paste0(filename, ".",
                                         switch(file_format, "png" = "png", "pdf" = "pdf",
                                                "tiff" = "tiff", "jpeg" = "jpg")), "\n")
  }
}
