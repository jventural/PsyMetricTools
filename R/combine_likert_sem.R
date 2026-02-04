#' Combine Likert Plot with SEM Path Diagram
#'
#' Creates a combined visualization with a Likert plot and SEM path diagram.
#'
#' @param plot_likert A ggplot object with Likert plot.
#' @param fit_sem A lavaan model object for semPaths.
#' @param sem_args List of additional arguments for semPaths.
#' @param nodeLabels Custom node labels for the SEM diagram.
#' @param ncol Number of columns in layout (default: 2).
#' @param widths Relative widths of columns.
#' @param tag_levels Tag levels for plot annotation (default: "A").
#' @param tag_suffix Suffix for tags.
#' @param tag_pos Position of tags as c(x, y).
#' @param tag_offset_y Vertical offset for tags.
#' @param tag_size Font size for tags.
#' @param tag_face Font face for tags.
#'
#' @return A combined patchwork plot.
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(ggplot2)
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
#' # Create Likert plot
#' likert_plot <- Plot_Likert(
#'   Data = data,
#'   name_items = "Item",
#'   ranges = 1:6
#' )
#'
#' # Fit CFA model
#' model <- "
#'   F1 =~ Item1 + Item2 + Item3
#'   F2 =~ Item4 + Item5 + Item6
#' "
#' fit <- cfa(model, data = data, ordered = TRUE, estimator = "WLSMV")
#'
#' # Combine Likert plot with SEM path diagram
#' combined_plot <- combine_likert_sem(
#'   plot_likert = likert_plot,
#'   fit_sem = fit,
#'   ncol = 2,
#'   widths = c(1, 1),
#'   tag_levels = "A",
#'   tag_size = 14
#' )
#'
#' # Display combined plot
#' print(combined_plot)
#'
#' # Save combined plot
#' ggsave("combined_likert_sem.jpg", combined_plot,
#'        width = 14, height = 8, dpi = 300)
#' }
#' @export
combine_likert_sem <- function(
    plot_likert,       # un objeto ggplot
    fit_sem,           # tu objeto lavaan para semPaths()

    ## Parámetros de semPaths
    sem_args    = list(),
    nodeLabels  = NULL,    # ahora configurable por el usuario

    ## Disposición
    ncol        = 2,
    widths      = rep(1, ncol),

    ## Etiquetas (A, B, C…)
    tag_levels   = "A",
    tag_suffix   = "",
    tag_pos      = c(0.05, 0.95),   # posición base (x, y)
    tag_offset_y = 0.02,            # cuánto subir en y
    tag_size     = 16,
    tag_face     = "bold"
) {
  # 1) Verificar paquetes requeridos
  pkgs <- c("ggplotify", "patchwork", "semPlot")
  for(pkg in pkgs){
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(paste0("Package '", pkg, "' is required but not installed. Please install it with install.packages('", pkg, "')"))
  }

  # 2) Prepara args para semPaths()
  defaults <- list(
    object              = fit_sem,
    whatLabels          = "std",
    rotation            = 2,
    curve               = 1.5,
    optimizeLatRes      = FALSE,
    residuals           = FALSE,
    edge.label.cex      = 1.2,
    nCharEdges          = 0.9,
    edge.label.position = 0.5,
    edge.label.font     = 0.2,
    thresholds          = FALSE,
    intercepts          = FALSE,
    curvature           = 1.5,
    curvePivot          = FALSE,
    sizeMan             = 8,
    sizeMan2            = 4,
    sizeLat             = 10,
    sizeLat2            = 10,
    esize               = 3,
    asize               = 3,
    nodeLabels          = nodeLabels,   # uso del argumento
    mar                 = c(1.5, 22, 1.5, 25),
    label.prop          = 0.7,
    style               = "lisrel"
  )
  sem_plot_args <- modifyList(defaults, sem_args)

  # 3) Genera el ggplot del semPaths
  sem_gg <- ggplotify::as.ggplot(function(){
    do.call(semPlot::semPaths, sem_plot_args)
  })

  # 4) Ajusta la posición real de la etiqueta sumando el offset
  real_tag_pos <- c(tag_pos[1], min(1, tag_pos[2] + tag_offset_y))

  # 5) Combina y etiqueta
  # Primero aplicar el tema a cada plot individualmente
  plot_likert_themed <- plot_likert +
    ggplot2::theme(
      plot.tag          = ggplot2::element_text(size = tag_size, face = tag_face),
      plot.tag.position = real_tag_pos
    )

  sem_gg_themed <- sem_gg +
    ggplot2::theme(
      plot.tag          = ggplot2::element_text(size = tag_size, face = tag_face),
      plot.tag.position = real_tag_pos
    )

  # Luego combinar con patchwork
  combined <- plot_likert_themed + sem_gg_themed +
    patchwork::plot_layout(ncol = ncol, widths = widths) +
    patchwork::plot_annotation(tag_levels = tag_levels, tag_suffix = tag_suffix)

  return(combined)
}
