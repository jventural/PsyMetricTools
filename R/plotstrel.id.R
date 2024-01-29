plotstrel.id <- function (x, estimate, ordering = FALSE, color_gray = "grayscale", colors_wes = NULL) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar la librería requerida
  install_and_load("wesanderson")
  install_and_load("ggplot2")
  install_and_load("ggridges")


  if (!"Bayes" %in% names(x)) {
    stop("La lista 'x' no contiene el elemento 'Bayes'")
  }
  if (!"ifitem" %in% names(x$Bayes)) {
    stop("El elemento 'Bayes' de la lista 'x' no contiene 'ifitem'")
  }
  posi <- grep(estimate, x$estimates, ignore.case = TRUE)
  if (length(posi) == 0) {
    stop(paste("Estimación '", estimate, "' no encontrada en 'x$estimates'", sep = ""))
  }
  item_names <- colnames(x$data)
  if (is.null(item_names) || length(item_names) != dim(x$Bayes$ifitem$samp[[posi]])[3]) {
    item_names <- paste0("x", 1:dim(x$Bayes$ifitem$samp[[posi]])[3])
  }
  n.row <- length(item_names)
  dat <- data.frame(value = as.vector(x$Bayes$samp[[posi]]))
  dat$colos <- "1"
  dat$var <- "original"
  for (i in 1:n.row) {
    item_values <- as.vector(x$Bayes$ifitem$samp[[posi]][, , i])
    tmp <- data.frame(value = item_values)
    tmp$var <- item_names[i]
    tmp$colos <- "2"
    dat <- rbind(dat, tmp)
  }
  dat$var <- factor(dat$var, levels = c("original", item_names))
  if (ordering) {
    est <- as.vector(x$Bayes$ifitem$est[[posi]])
    est <- data.frame(value = c(est, NA))
    est$var <- c(item_names, "original")
    est <- est[order(est$value, decreasing = TRUE), ]
    dat$var <- factor(dat$var, levels = est$var)
  }

  # Configuración de la paleta de colores
  if (!is.null(colors_wes)) {
    # Si se proporciona un vector de colores, úsalo directamente
    palette_colors <- colors_wes
  } else if (color_gray == "grayscale") {
    palette_colors <- scales::grey_pal()(2)
  } else if (color_gray == "wes") {
    # Utiliza una paleta de colores por defecto de wesanderson si no se especifica una
    palette_colors <- wesanderson::wes_palette("Darjeeling1", n = 2, type = "continuous")
  } else {
    stop("Esquema de color no reconocido.")
  }

  # Gráfico ggplot
  p <- ggplot(dat, aes(x = value, y = var, fill = colos)) +
    stat_density_ridges(quantile_lines = TRUE,
                        quantiles = c(0.025, 0.5, 0.975), alpha = 0.85,
                        show.legend = FALSE) +
    scale_fill_manual(values = palette_colors) +
    theme_linedraw() +
    theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black")) +
    xlab(paste0("\n", estimate)) +
    ylab("Item Dropped") +
    scale_y_discrete(expand = expand_scale(add = c(0.25, 1.5))) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 4, size = 20),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),  # Elimina las líneas verticales
          panel.grid.minor.x = element_blank())  # Elimina las líneas verticales menores

  return(p)
}
