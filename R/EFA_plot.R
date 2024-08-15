EFA_plot <- function(specifications, item_prefix) {
  # Cargar las librerías necesarias
  library(dplyr)
  library(ggplot2)
  library(stringr)

  # Preparar los datos con una nueva columna que indique si un ítem es complejo
  complex_items <- standardizedsolution(specifications) %>%
    dplyr::filter(op == "=~") %>%
    mutate(item = as.numeric(str_remove(rhs, item_prefix)),
           factor = as.numeric(str_remove(lhs, "f"))) %>%
    group_by(item) %>%
    mutate(complex = sum(est.std > 0.30) > 1) %>%
    ungroup()

  # Graficar con corrección de la condición para el borde rojo
  figure2 <- ggplot(complex_items, aes(x = est.std, xmin = ci.lower, xmax = ci.upper, y = item)) +
    annotate(geom = "rect",
             xmin = -1, xmax = 1,
             ymin = -Inf, ymax = Inf,
             fill = "grey90") +
    annotate(geom = "rect",
             xmin = -0.7, xmax = 0.7,
             ymin = -Inf, ymax = Inf,
             fill = "grey93") +
    annotate(geom = "rect",
             xmin = -0.4, xmax = 0.4,
             ymin = -Inf, ymax = Inf,
             fill = "grey96") +
    geom_vline(xintercept = 0, color = "white") +
    geom_pointrange(aes(alpha = abs(est.std) < 0.4,
                        color = "black"),
                    fatten = 5,
                    size = 0.8,
                    shape = 21,
                    fill = "black",
                    stroke = ifelse(complex_items$complex, 1, 0),
                    color = ifelse(complex_items$complex, "red", "black")) +
    geom_text(aes(label = item, color = abs(est.std) < 0.4), size = 2) +
    scale_color_manual(values = c("white", "black"), guide = "none") +
    scale_alpha_manual(values = c(1, 1/5)) +
    scale_x_continuous(expression(lambda[standardized]),
                       expand = c(0, 0), limits = c(-1, 1),
                       breaks = c(-1, -0.7, -0.4, 0, 0.4, 0.7, 1),
                       labels = c("-1", "-.7", "-.4", "0", ".4", ".7", "1")) +
    scale_y_continuous(breaks = 1:65, sec.axis = sec_axis(~ . * 1, breaks = 1:65)) +
    ggtitle("") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 7),   # Tamaño de los números en el eje X
      axis.text.y = element_text(size = 7),   # Tamaño de los números en el eje Y
      axis.title.x = element_text(size = 12),  # Tamaño del título del eje X
      axis.title.y = element_text(size = 12),  # Tamaño del título del eje Y
      strip.text = element_text(size = 12)     # Tamaño de los títulos de los facetas
    ) +
    facet_wrap(~ factor, labeller = label_both)

  return(figure2)
}
