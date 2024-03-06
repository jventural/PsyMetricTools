Plot_and_Table_absolute <- function(df_repli, abs_ymin_annot = NULL, abs_ymax_annot = NULL){
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

  # Establecer ymin_annot y ymax_annot si no se proporcionan
  if(is.null(abs_ymin_annot)) {
    abs_ymin_annot <- 0  # Ajuste manual para la posición de la tabla
  }

  if(is.null(abs_ymax_annot)) {
    abs_ymax_annot <- max(table$max) + 0.10  # Ajuste manual para la posición de la tabla
  }

  # Crear el gráfico de RMSEA, SRMR, CRMR
  plot <- df_repli$fit_measures1 %>%
    map_dfr(~as_tibble(.)) %>%
    select(RMSEA, SRMR, CRMR) %>%
    pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
    mutate(Value = round(Value, 3)) %>%
    ggplot(aes(x=Fit, y=Value, fill=Fit)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1) +
    theme_bw() +
    scale_y_continuous(limits = c(abs_ymin_annot, abs_ymax_annot),
                       breaks = seq(abs_ymin_annot, abs_ymax_annot, by = 0.01)) +
    annotation_custom(gridExtra::tableGrob(table, rows=NULL, theme = ttheme_default(
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
      xmin=0, xmax=4, ymin=abs_ymin_annot, ymax=abs_ymax_annot) +
    scale_fill_grey(start = 0.5, end = 0.9)+
    theme(legend.position = "none")

  # Retornar tanto la tabla como el gráfico
  return(list(table = table, plot = plot))
}

