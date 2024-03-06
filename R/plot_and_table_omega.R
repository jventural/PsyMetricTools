plot_and_table_omega <- function(df_repli) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(gridExtra)
  library(gtable)

  # Generar la tabla de estadísticas de fiabilidad
  res_omega_table <- df_repli %>%
    select(starts_with("Rel")) %>%
    rename_with(~gsub("Rel", "ω", .)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    summarise(
      mean = round(mean(Value, na.rm = TRUE), 2),
      sd = round(sd(Value, na.rm = TRUE), 2),
      min = round(min(Value, na.rm = TRUE), 2),
      max = round(max(Value, na.rm = TRUE), 2)
    ) %>%
    ungroup()

  # Calcular el mínimo ajustado y los límites para la escala y
  min_adjusted <- min(res_omega_table$min) -0.05
  ymin_annot = max(res_omega_table$mean)
  ymax_annot = 0.92    # 2 puntos porcentuales más que ymin_annot

  # Preparar datos para el gráfico
  data_long <- df_repli %>%
    pivot_longer(cols = starts_with("Rel"), names_to = "Reliability", values_to = "value") %>%
    mutate(Reliability = gsub("Rel", "ω", Reliability))

  # Crear el gráfico de fiabilidad
  plot <- ggplot(data_long, aes(x = Reliability, y = value, fill = Reliability)) +
    geom_boxplot(outlier.shape = 16) +
    theme_bw() +
    scale_y_continuous(limits = c(min_adjusted, 1), breaks = seq(min_adjusted, 1, by = 0.05)) +
    scale_fill_grey(start = 0.5, end = 0.9) +
    labs(y = "\u03C9 values") +
    theme(legend.position = "none") +
    annotation_custom(gridExtra::tableGrob(res_omega_table, rows=NULL, theme = gridExtra::ttheme_default(
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
      xmin=1, xmax=length(res_omega_table$Variable), ymin=ymin_annot, ymax=ymax_annot) +
    # scale_fill_grey(start = 0.5, end = 0.9)+
    labs(y = "\u03C9 values")+
    theme(legend.position = "none")


  # Retornar tanto la tabla como el gráfico
  return(list(table = res_omega_table, plot = plot))
}
