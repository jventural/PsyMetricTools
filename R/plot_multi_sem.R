#' @title Plot Multiple SEM Models
#' @description Creates a panel of SEM path diagrams for multiple models. The
#'   panel can be arranged in a single row (default) or in an arbitrary grid via
#'   the \code{nrow} / \code{ncol} arguments; empty cells are left blank when the
#'   number of models does not fill the grid.
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
#' @param nrow Number of rows in the panel grid. Default \code{NULL} (a single
#'   row). If only one of \code{nrow}/\code{ncol} is supplied, the other is
#'   derived from the number of models.
#' @param ncol Number of columns in the panel grid. Default \code{NULL} (one
#'   column per model when \code{nrow} is also \code{NULL}).
#' @param show_fit_indices Show fit indices (default TRUE).
#' @param fit_indices Which fit indices to show.
#' @param use_scaled Use scaled indices (default FALSE).
#' @param model_descriptions Model descriptions.
#' @param custom_titles Custom titles.
#' @param title.cex Title size (default 0.8).
#' @param title.font Title font (default 2).
#' @param save_plot Save plot to file (default FALSE).
#' @param filename Output filename without extension (default NULL; required
#'   when \code{save_plot = TRUE}, e.g. \code{file.path(tempdir(), "sem_plot")}).
#' @param file_format Output format (png, pdf, tiff, jpeg).
#' @param width_per Width per plot in inches (per column).
#' @param height Height in inches (per row).
#' @param dpi Resolution.
#' @param units Units for dimensions.
#' @return NULL (plots are drawn to device).
#' @examples
#' \donttest{
#' library(lavaan)
#'
#' # Create sample data
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
#' # Fit multiple models
#' model1 <- "F1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6"
#' model2 <- "F1 =~ Item1 + Item2 + Item3\nF2 =~ Item4 + Item5 + Item6"
#'
#' fit1 <- cfa(model1, data = data, ordered = TRUE, estimator = "WLSMV")
#' fit2 <- cfa(model2, data = data, ordered = TRUE, estimator = "WLSMV")
#'
#' # Plot multiple models side by side (single row, default)
#' plot_multi_sem(
#'   models = list(fit1, fit2),
#'   model_descriptions = c("1-Factor Model", "2-Factor Model"),
#'   show_fit_indices = TRUE,
#'   fit_indices = c("cfi", "tli", "rmsea", "srmr"),
#'   use_scaled = TRUE
#' )
#'
#' # Arrange three models in two rows (2 x 2 grid; last cell left blank)
#' plot_multi_sem(
#'   models = list(fit1, fit2, fit1),
#'   model_descriptions = c("Model 1", "Model 2", "Model 3"),
#'   nrow = 2, ncol = 2,
#'   save_plot = TRUE, filename = file.path(tempdir(), "sem_grid"),
#'   width_per = 6, height = 6
#' )
#' }
#' @importFrom graphics plot.new
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
                           # Disposicion del panel (grilla)
                           nrow        = NULL,
                           ncol        = NULL,
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
                           filename    = NULL,
                           file_format = "png",
                           width_per   = 4,    # ancho (in) por cada columna
                           height      = 4,    # altura (in) por cada fila
                           dpi         = 300,
                           units       = "in") {

  n <- length(models)

  # Determinar la grilla del panel (retrocompatible: por defecto 1 fila x n)
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- 1L
    ncol <- n
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  }
  if (nrow * ncol < n) {
    stop(sprintf("La grilla (%d x %d = %d celdas) es menor que el numero de modelos (%d).",
                 nrow, ncol, nrow * ncol, n))
  }

  # Generar titulos si no se proporcionan
  if (is.null(titles) && is.null(custom_titles)) {
    titles <- paste("Modelo", seq_len(n))
  } else if (is.null(titles)) {
    titles <- custom_titles
  }

  # Abrir dispositivo para guardar si se requiere
  if (save_plot) {
    if (is.null(filename)) {
      stop("Please provide 'filename' (without extension) when save_plot = TRUE, ",
           "e.g. filename = file.path(tempdir(), \"sem_models_plot\").")
    }
    total_width  <- width_per * ncol
    total_height <- height * nrow
    if (file_format == "png") {
      png(paste0(filename, ".png"),
          width  = total_width,
          height = total_height,
          units  = units,
          res    = dpi)
    } else if (file_format == "pdf") {
      pdf(paste0(filename, ".pdf"),
          width  = total_width,
          height = total_height)
    } else if (file_format == "tiff") {
      tiff(paste0(filename, ".tiff"),
           width  = total_width,
           height = total_height,
           units  = units,
           res    = dpi,
           compression = "lzw")
    } else if (file_format == "jpeg") {
      jpeg(paste0(filename, ".jpg"),
           width  = total_width,
           height = total_height,
           units  = units,
           res    = dpi,
           quality = 100)
    } else {
      stop("Formato no soportado: elige png, pdf, tiff o jpeg.")
    }
  }

  # Configurar canvas: nrow filas x ncol columnas
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mfrow = c(nrow, ncol), mar = outerMar)

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

  # Rellenar celdas sobrantes de la grilla en blanco
  resto <- nrow * ncol - n
  if (resto > 0) {
    for (k in seq_len(resto)) plot.new()
  }

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
