Plot_and_Table_comparative <- function(df_repli, comp_ymin_annot = NULL, comp_ymax_annot = NULL){

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

  # Calcular el mínimo ajustado y los límites para la escala y si no se especifican
  if(is.null(comp_ymin_annot) || is.null(comp_ymax_annot)) {
    min_adjusted <- min(table$min) - 0.05
    comp_ymin_annot <- min_adjusted  # menos que el mínimo ajustado
    comp_ymax_annot <- comp_ymin_annot + 0.05  # ajuste para ymax basado en ymin
  }

  # Calcular el límite inferior para ylim
  ymin_limit <- min(table$min)

  # Si ymin_limit es mayor a 0.95, establecerlo a 0.90
  if(ymin_limit > 0.95) {
    ymin_limit <- 0.90
  }

  # Crear el gráfico de CFI y TLI
  plot <- df_repli$fit_measures1 %>%
    map_dfr(~as_tibble(.)) %>%
    select(CFI, TLI) %>%
    pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
    mutate(Value = round(Value, 3)) %>%
    ggplot(aes(x=Fit, y=Value, fill=Fit)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1) +
    theme_bw() +
    coord_cartesian(ylim = c(ymin_limit, 1.00)) + # Usa los límites personalizados
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
      xmin=0, xmax=3, ymin=comp_ymin_annot, ymax=comp_ymax_annot)

  # Retornar tanto la tabla como el gráfico
  return(list(table = table, plot = plot))
}
