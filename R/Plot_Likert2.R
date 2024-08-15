Plot_Likert2 <- function(data,
                         prefix = "IS",
                         labels = c("Not at all true", "Completely true"),
                         palette = "Blues",
                         text_size = 3,
                         threshold = 5,
                         show_text = TRUE,   # Nuevo argumento para mostrar o no el texto
                         item_range = 1:21) {  # Nuevo argumento para especificar el rango de ítems
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(RColorBrewer)
  library(scales)
  # Transforma el dataframe en formato largo
  df_long <- data %>%
    select(starts_with(prefix)) %>%
    pivot_longer(cols = starts_with(prefix),
                 names_to = "Variable",
                 values_to = "Score")

  # Recodifica los valores para que sean factores con etiquetas significativas
  df_long$Score <- factor(df_long$Score, levels = min(df_long$Score):max(df_long$Score), labels = labels)

  # Establece el orden de las variables basado en el rango de ítems
  ordered_levels <- rev(paste0(prefix, item_range))  # Utiliza el rango de ítems proporcionado
  df_long$Variable <- factor(df_long$Variable, levels = ordered_levels)

  # Calcula las proporciones y crea el eje X simétrico
  df_prop <- df_long %>%
    group_by(Variable, Score) %>%
    summarise(n = n()) %>%
    mutate(Proportion = (n / sum(n)) * 100) %>%
    mutate(Side = ifelse(as.numeric(Score) <= length(labels)/2, -Proportion, Proportion))

  # Crea el gráfico de barras horizontales centrado
  figure1 <- ggplot(df_prop, aes(x = Side, y = Variable, fill = Score)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = palette,
                      name = "Scale") +
    scale_x_continuous(limits = c(-100, 100),
                       breaks = seq(-100, 100, 25),
                       labels = function(x) paste0(abs(x), "%")) +
    labs(title = "",
         x = "Proportion",
         y = "Items",
         fill = "Scale") +
    geom_text(aes(label = ifelse(show_text & abs(Side) > threshold, sprintf("%.2f%%", abs(Side)), ""),
                  color = ifelse(as.numeric(Score) > (length(labels) / 2), "white", "black")),
              position = position_stack(vjust = 0.5), size = text_size) +
    scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(angle = 0, hjust = 1),
          axis.title.y = element_blank())

  return(figure1)
}
