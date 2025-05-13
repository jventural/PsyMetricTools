combine_likert_sem <- function(
    plot_likert,       # un objeto ggplot
    fit_sem,           # tu objeto lavaan para semPaths()

    ## Parámetros de semPaths
    sem_args    = list(),

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
  # 1) Instalar/cargar paquetes
  pkgs <- c("ggplotify", "patchwork", "semPlot")
  for(pkg in pkgs){
    if (!requireNamespace(pkg, quietly = TRUE))
      install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }

  # 2) Prepara args para semPaths()
  defaults <- list(
    object             = fit_sem,
    whatLabels         = "std",
    rotation           = 2,
    curve              = 1.5,
    optimizeLatRes     = FALSE,
    residuals          = FALSE,
    edge.label.cex     = 1.2,
    nCharEdges         = 0.9,
    edge.label.position= 0.5,
    edge.label.font    = 0.2,
    thresholds         = FALSE,
    intercepts         = FALSE,
    curvature          = 1.5,
    curvePivot         = FALSE,
    sizeMan            = 8,
    sizeMan2           = 4,
    sizeLat            = 10,
    sizeLat2           = 10,
    esize              = 3,
    asize              = 3,
    nodeLabels         = c(paste0("AGPS", c(1,3,4,6,7)), "Goal"),
    mar                = c(1.5, 22, 1.5, 25),
    label.prop         = 0.7,
    style              = "lisrel"
  )
  sem_plot_args <- modifyList(defaults, sem_args)

  # 3) Genera el ggplot del semPaths
  sem_gg <- ggplotify::as.ggplot(function(){
    do.call(semPlot::semPaths, sem_plot_args)
  })

  # 4) Ajusta la posición real de la etiqueta sumando el offset
  real_tag_pos <- c(tag_pos[1], min(1, tag_pos[2] + tag_offset_y))

  # 5) Combina y etiqueta
  combined <- plot_likert + sem_gg +
    patchwork::plot_layout(ncol = ncol, widths = widths) +
    patchwork::plot_annotation(tag_levels = tag_levels, tag_suffix = tag_suffix) &
    ggplot2::theme(
      plot.tag          = ggplot2::element_text(size = tag_size, face = tag_face),
      plot.tag.position = real_tag_pos
    )

  return(combined)
}
