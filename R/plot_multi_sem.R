plot_multi_sem <- function(models,
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
                           color       = list(lat = "grey80", man = "grey90"),
                           mar         = c(4,4,4,4),
                           outerMar    = c(1,1,3,1),
                           # Índices de bondad de ajuste
                           show_fit_indices   = TRUE,
                           fit_indices        = c("cfi", "tli", "rmsea", "srmr"),
                           use_scaled         = FALSE,
                           model_descriptions = NULL,
                           custom_titles      = NULL,
                           # Texto de títulos/índices
                           title.cex  = 0.8,
                           title.font = 2,
                           # Parámetros para guardar
                           save_plot   = FALSE,
                           filename    = "sem_models_plot",
                           file_format = "png",
                           width_per   = 4,    # ancho (in) por cada gráfico
                           height      = 4,    # altura fija (in)
                           dpi         = 300,
                           units       = "in") {
  library(semPlot)

  n <- length(models)

  # Generar títulos si no se proporcionan
  if (is.null(titles) && is.null(custom_titles)) {
    titles <- paste("Modelo", seq_len(n))
  } else if (is.null(titles)) {
    titles <- custom_titles
  }

  # Abrir dispositivo para guardar si se requiere
  if (save_plot) {
    total_width <- width_per * n
    if (file_format == "png") {
      png(paste0(filename, ".png"),
          width  = total_width,
          height = height,
          units  = units,
          res    = dpi)
    } else if (file_format == "pdf") {
      pdf(paste0(filename, ".pdf"),
          width  = total_width,
          height = height)
    } else if (file_format == "tiff") {
      tiff(paste0(filename, ".tiff"),
           width  = total_width,
           height = height,
           units  = units,
           res    = dpi,
           compression = "lzw")
    } else if (file_format == "jpeg") {
      jpeg(paste0(filename, ".jpg"),
           width  = total_width,
           height = height,
           units  = units,
           res    = dpi,
           quality = 100)
    } else {
      stop("Formato no soportado: elige png, pdf, tiff o jpeg.")
    }
  }

  # Configurar canvas: 1 fila x n columnas
  par(mfrow = c(1, n), mar = outerMar)

  for (i in seq_len(n)) {
    # Dibujar el SEM
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

    # Construir texto del título
    if (show_fit_indices) {
      # Determinar nombres de índices (con .scaled si aplica)
      indices_to_extract <- if (use_scaled) {
        sapply(fit_indices, function(x) {
          if (x %in% c("cfi", "tli", "rmsea")) paste0(x, ".scaled") else x
        })
      } else {
        fit_indices
      }

      fit_vals <- fitMeasures(models[[i]], indices_to_extract)

      # Descripción del modelo
      desc <- if (!is.null(model_descriptions)) {
        model_descriptions[i]
      } else {
        sprintf("Modelo %d", i)
      }

      # Formatear texto de índices
      idx_texts <- sapply(seq_along(fit_vals), function(j) {
        nm <- names(fit_vals)[j]
        nm_disp <- if (use_scaled) sub("\\.scaled$", "", nm) else nm
        sprintf("%s=%.3f", toupper(nm_disp), fit_vals[j])
      })

      title_text <- paste0(desc, "\n", paste(idx_texts, collapse = " | "))
    } else {
      title_text <- titles[i]
    }

    # Agregar título
    title(title_text,
          line     = 1,
          cex.main = title.cex,
          font.main= title.font)
  }

  # Restaurar dispositivo
  par(mfrow = c(1,1))

  if (save_plot) {
    dev.off()
    message("Gráfico guardado como: ", paste0(filename, ".",
                                              switch(file_format,
                                                     png  = "png",
                                                     pdf  = "pdf",
                                                     tiff = "tiff",
                                                     jpeg = "jpg")))
  }
}
