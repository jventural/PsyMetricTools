#' @title Plot Multiple SEM Models
#' @description Creates a panel of SEM path diagrams for multiple models.
#' @param models List of lavaan model objects.
#' @param titles Titles for each model (default NULL).
#' @param layout Layout type (default "tree2").
#' @param rotation Rotation value (default 2).
#' @param whatLabels Labels to show (default "std").
#' @param residuals Show residuals (default FALSE).
#' @param intercepts Show intercepts (default FALSE).
#' @param thresholds Show thresholds (default FALSE).
#' @param sizeMan Manifest variable size (default 10).
#' @param sizeMan2 Second manifest size (default 5).
#' @param sizeLat Latent variable size (default 12).
#' @param label.cex Label size multiplier (default 1).
#' @param edge.label.cex Edge label size (default 2).
#' @param edge.color Edge color (default "grey40").
#' @param color Color list for nodes.
#' @param mar Inner margins.
#' @param outerMar Outer margins.
#' @param show_fit_indices Show fit indices (default TRUE).
#' @param fit_indices Which fit indices to show.
#' @param use_scaled Use scaled indices (default FALSE).
#' @param model_descriptions Model descriptions.
#' @param custom_titles Custom titles.
#' @param title.cex Title size (default 0.8).
#' @param title.font Title font (default 2).
#' @param save_plot Save plot to file (default FALSE).
#' @param filename Output filename.
#' @param file_format Output format (png, pdf, tiff, jpeg).
#' @param width_per Width per plot in inches.
#' @param height Height in inches.
#' @param dpi Resolution.
#' @param units Units for dimensions.
#' @return NULL (plots are drawn to device).
#' @export
plot_multi_sem <- function(models,
                           titles = NULL,
                           layout      = "tree2",
                           rotation    = 2,
                           whatLabels  = "std",
                           residuals   = FALSE,
                           intercepts  = FALSE,
                           thresholds  = FALSE,
                           # Parametros de tamano de elementos del grafico
                           sizeMan     = 10,
                           sizeMan2    = 5,
                           sizeLat     = 12,
                           label.cex   = 1,
                           edge.label.cex = 2,
                           edge.color  = "grey40",
                           color       = list(lat = "grey80", man = "grey90"),
                           mar         = c(4,4,4,4),
                           outerMar    = c(1,1,3,1),
                           # Indices de bondad de ajuste
                           show_fit_indices   = TRUE,
                           fit_indices        = c("cfi", "tli", "rmsea", "srmr"),
                           use_scaled         = FALSE,
                           model_descriptions = NULL,
                           custom_titles      = NULL,
                           # Texto de titulos/indices
                           title.cex  = 0.8,
                           title.font = 2,
                           # Parametros para guardar
                           save_plot   = FALSE,
                           filename    = "sem_models_plot",
                           file_format = "png",
                           width_per   = 4,    # ancho (in) por cada grafico
                           height      = 4,    # altura fija (in)
                           dpi         = 300,
                           units       = "in") {

  n <- length(models)

  # Generar titulos si no se proporcionan
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
    semPlot::semPaths(models[[i]],
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

    # Construir texto del titulo
    if (show_fit_indices) {
      # Determinar nombres de indices (con .scaled si aplica)
      indices_to_extract <- if (use_scaled) {
        sapply(fit_indices, function(x) {
          if (x %in% c("cfi", "tli", "rmsea")) paste0(x, ".scaled") else x
        })
      } else {
        fit_indices
      }

      fit_vals <- lavaan::fitMeasures(models[[i]], indices_to_extract)

      # Descripcion del modelo
      desc <- if (!is.null(model_descriptions)) {
        model_descriptions[i]
      } else {
        sprintf("Modelo %d", i)
      }

      # Formatear texto de indices
      idx_texts <- sapply(seq_along(fit_vals), function(j) {
        nm <- names(fit_vals)[j]
        nm_disp <- if (use_scaled) sub("\\.scaled$", "", nm) else nm
        sprintf("%s=%.3f", toupper(nm_disp), fit_vals[j])
      })

      title_text <- paste0(desc, "\n", paste(idx_texts, collapse = " | "))
    } else {
      title_text <- titles[i]
    }

    # Agregar titulo
    title(title_text,
          line     = 1,
          cex.main = title.cex,
          font.main= title.font)
  }

  # Restaurar dispositivo
  par(mfrow = c(1,1))

  if (save_plot) {
    dev.off()
    message("Grafico guardado como: ", paste0(filename, ".",
                                              switch(file_format,
                                                     png  = "png",
                                                     pdf  = "pdf",
                                                     tiff = "tiff",
                                                     jpeg = "jpg")))
  }
}
