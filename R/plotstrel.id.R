plotstrel.id <- function(x, estimate, ordering = FALSE) {
  if (!"Bayes" %in% names(x)) {
    stop("La lista 'x' no contiene el elemento 'Bayes'")
  }
  if (!"ifitem" %in% names(x$Bayes)) {
    stop("El elemento 'Bayes' de la lista 'x' no contiene 'ifitem'")
  }

  posi <- grep(estimate, x$estimates, ignore.case = T)
  if (length(posi) == 0) {
    stop(paste("Estimación '", estimate, "' no encontrada en 'x$estimates'", sep = ""))
  }

  # Extraer los nombres de los ítems de los datos
  item_names <- colnames(x$data)
  if (is.null(item_names) || length(item_names) != dim(x$Bayes$ifitem$samp[[posi]])[3]) {
    item_names <- paste0("x", 1:dim(x$Bayes$ifitem$samp[[posi]])[3])
  }

  n.row <- length(item_names)

  # Crear el marco de datos 'dat'
  dat <- data.frame(value = as.vector(x$Bayes$samp[[posi]]))
  dat$colos <- "1"
  dat$var <- "original"

  # Extraer datos para cada ítem
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
    est <- data.frame(value = c(est, NA))  # Añadir NA para el ítem "original"
    est$var <- c(item_names, "original")
    est <- est[order(est$value, decreasing = TRUE), ]
    dat$var <- factor(dat$var, levels = est$var)
  }

  # Crear el gráfico
  ggplot2::ggplot(dat, ggplot2::aes(x = value, y = var, fill = colos)) +
    ggridges::stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975),
                                  alpha = .85, show.legend = FALSE) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   strip.text = ggplot2::element_text(colour = "black")) +
    ggplot2::xlab(paste0("\n", estimate)) +  # Modificación aquí para reflejar el nombre del estimador
    ggplot2::ylab("Item Dropped") +
    ggplot2::scale_y_discrete(expand = ggplot2::expand_scale(add = c(0.25, 1.5))) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, vjust = 4, size = 20),
                   axis.title = ggplot2::element_text(size = 16),
                   axis.text = ggplot2::element_text(size = 12),
                   plot.margin = ggplot2::unit(c(1,1,1,1), "cm"))
}
