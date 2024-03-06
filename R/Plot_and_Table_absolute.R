Plot_and_Table_absolute <- function(df_repli){
  library(tidyverse)
  library(reshape2)
  library(gridExtra)
  library(gtable)
  # Generar la tabla de estadísticas para RMSEA, SRMR, CRMR
  table <- map_dfr(df_repli$fit_measures1, ~as_tibble(.)) %>%
    select(RMSEA, SRMR, CRMR) %>%
    pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
    mutate(Value = round(Value, 3)) %>%
    group_by(Fit) %>%
    summarise(mean = round(mean(Value, na.rm = TRUE), 2),
              sd = round(sd(Value, na.rm = TRUE), 2),
              min = round(min(Value, na.rm = TRUE), 2),
              max = round(max(Value, na.rm = TRUE), 2)) %>%
    ungroup()

  # Calcular el mínimo y los límites ajustados para la escala y
  #
  ymin_annot = 0  # Ajuste manual para la posición de la tabla
  ymax_annot = max(table$max) + 0.10  # Ajuste manual para la posición de la tabla

  min_adjusted <- 0.11  # Asumiendo que el rango empieza en 0 para medidas absolutas
  max_adjusted <- 0.12  # el máximo esté cubierto

  # Crear el gráfico de RMSEA, SRMR, CRMR
  plot <- df_repli$fit_measures1 %>%
    map_dfr(~as_tibble(.)) %>%
    select(RMSEA, SRMR, CRMR) %>%
    pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
    mutate(Value = round(Value, 3)) %>%
    ggplot(aes(x=Fit, y=Value, fill=Fit)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1) +
    theme_bw() +
    scale_y_continuous(limits = c(ymin_annot, ymax_annot),
                       breaks = seq(ymin_annot, ymax_annot, by = 0.01)) +
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
      xmin=0, xmax=4, ymin=min_adjusted, ymax=max_adjusted) +
    scale_fill_grey(start = 0.5, end = 0.9)+
    theme(legend.position = "none")

  # Mostrar el gráfico
  print(plot)

  # Retornar tanto la tabla como el gráfico
  return(list(table = table, plot = plot))
}
