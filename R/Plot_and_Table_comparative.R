Plot_and_Table_comparative <- function(df_repli){

  library(tidyverse)
  library(reshape2)
  library(gridExtra)
  library(gtable)

  # Generar la tabla de estadísticas para CFI y TLI
  table <- map_dfr(df_repli$fit_measures1, ~as_tibble(.)) %>%
    select(CFI,TLI) %>%
    pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
    mutate(Value = round(Value, 3)) %>%
    group_by(Fit) %>%
    summarise(mean = round(mean(Value, na.rm = TRUE), 2),
              sd = round(sd(Value, na.rm = TRUE), 2),
              min = round(min(Value, na.rm = TRUE), 2),
              max = round(max(Value, na.rm = TRUE), 2)) %>%
    ungroup()

  # Calcular el mínimo ajustado y los límites para la escala y
  min_adjusted <- min(table$min) - 0.05
  ymin_annot = min_adjusted  #  menos que el mínimo ajustado
  ymax_annot = ymin_annot + 0.05    # 2 puntos porcentuales más que ymin_annot

  # Crear el gráfico de CFI y TLI
  plot <- df_repli$fit_measures1 %>%
    map_dfr(~as_tibble(.)) %>%
    select(CFI, TLI) %>%
    pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
    mutate(Value = round(Value, 3)) %>%
    ggplot(aes(x=Fit, y=Value, fill=Fit)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1) +
    theme_bw() +
    coord_cartesian(ylim = c(min_adjusted, 1.00)) + # Cambio aquí
    scale_fill_grey(start = 0.5, end = 0.9) +
    theme(legend.position = "none") +
    annotation_custom(gridExtra::tableGrob(table, rows=NULL, theme = gridExtra::ttheme_default(
      core=list(bg_params = list(fill = c("#F2F3F4","#F2F3F4", "#F2F3F4"), col=NA),
                fg_params=list(fontface= 1)),
      colhead=list(fg_params=list(col="black", fontface=c(1,1,3,1,1))),
      rowhead=list(fg_params=list(col="black", fontface=1)), base_size=8)) %>%
        gtable::gtable_add_grob(.,
                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                t = 2, b = nrow(.), l = 1, r = ncol(.)) %>%
        gtable::gtable_add_grob(.,
                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                t = 1, l = 1, r = ncol(.)),
      xmin=0, xmax=3, ymin=ymin_annot, ymax=ymax_annot)

  # Mostrar el gráfico
  print(plot)

  # Retornar tanto la tabla como el gráfico
  return(list(table = table, plot = plot))
}
