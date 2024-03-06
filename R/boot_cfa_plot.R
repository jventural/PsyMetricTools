boot_cfa_plot <- function(df, save = TRUE, dpi = 600, omega_ymin_annot = NULL, omega_ymax_annot = NULL, comp_ymin_annot = NULL, comp_ymax_annot = NULL, abs_ymin_annot = NULL, abs_ymax_annot = NULL) {
  suppressWarnings({
    # Establece los valores predeterminados internos
    filename <- "Plot_boot_cfa.jpg"
    height <- 16
    width <- 22
    units <- "cm"

    # Asegúrate de tener cargadas las bibliotecas necesarias
    library(ggpubr)

    # Genera los plots y tablas con las funciones previas, incluyendo los nuevos argumentos
    p1 <- plot_and_table_omega(df, omega_ymin_annot, omega_ymax_annot)
    a1 <- p1$plot
    p2 <- Plot_and_Table_comparative(df, comp_ymin_annot, comp_ymax_annot)
    a2 <- p2$plot
    p3 <- Plot_and_Table_absolute(df, abs_ymin_annot, abs_ymax_annot)
    a3 <- p3$plot

    # Combina los plots en una única figura
    figure <- ggarrange(a1, a2, a3,
                        labels = c("A", "B", "C"),
                        ncol = 3, nrow = 1)

    # Condición para guardar la figura en un archivo
    if (save) {
      ggsave(filename = filename, plot = figure, height = height, width = width, dpi = dpi, units = units)
    }

    # Retorna la figura combinada para visualización en la consola
    return(figure)
  })
}

