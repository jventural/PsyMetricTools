#' @title Plot Likert Scale Items (Diverging Stacked Bar)
#' @description Creates a diverging stacked bar chart visualization for Likert scale items.
#' @param data Data frame containing the survey data.
#' @param prefix Prefix for item names (default "IS").
#' @param labels Labels for the scale endpoints.
#' @param palette Color palette name from RColorBrewer (default "Blues").
#' @param text_size Text size for labels (default 3).
#' @param threshold Threshold for showing text labels (default 5).
#' @param show_text Logical, whether to show text labels (default TRUE).
#' @param item_range Range of item numbers to include.
#' @return A ggplot object.
#' @export
Plot_Likert2 <- function(data,
                         prefix = "IS",
                         labels = c("Not at all true", "Completely true"),
                         palette = "Blues",
                         text_size = 3,
                         threshold = 5,
                         show_text = TRUE,   # Nuevo argumento para mostrar o no el texto
                         item_range = 1:21) {  # Nuevo argumento para especificar el rango de ítems

  # Transforma el dataframe en formato largo
  df_long <- data %>%
    dplyr::select(dplyr::starts_with(prefix)) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(prefix),
                 names_to = "Variable",
                 values_to = "Score")

  # Recodifica los valores para que sean factores con etiquetas significativas
  df_long$Score <- factor(df_long$Score, levels = min(df_long$Score):max(df_long$Score), labels = labels)

  # Establece el orden de las variables basado en el rango de ítems
  ordered_levels <- rev(paste0(prefix, item_range))  # Utiliza el rango de ítems proporcionado
  df_long$Variable <- factor(df_long$Variable, levels = ordered_levels)

  # Calcula las proporciones y crea el eje X simétrico
  df_prop <- df_long %>%
    dplyr::group_by(Variable, Score) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(Proportion = (n / sum(n)) * 100) %>%
    dplyr::mutate(Side = ifelse(as.numeric(Score) <= length(labels)/2, -Proportion, Proportion))

  # Crea el gráfico de barras horizontales centrado
  figure1 <- ggplot2::ggplot(df_prop, ggplot2::aes(x = Side, y = Variable, fill = Score)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_brewer(palette = palette,
                      name = "Scale") +
    ggplot2::scale_x_continuous(limits = c(-100, 100),
                       breaks = seq(-100, 100, 25),
                       labels = function(x) paste0(abs(x), "%")) +
    ggplot2::labs(title = "",
         x = "Proportion",
         y = "Items",
         fill = "Scale") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(show_text & abs(Side) > threshold, sprintf("%.2f%%", abs(Side)), ""),
                  color = ifelse(as.numeric(Score) > (length(labels) / 2), "white", "black")),
              position = ggplot2::position_stack(vjust = 0.5), size = text_size) +
    ggplot2::scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
          axis.text.y = ggplot2::element_text(angle = 0, hjust = 1),
          axis.title.y = ggplot2::element_blank())

  return(figure1)
}
