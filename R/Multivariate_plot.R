Multivariate_plot <- function(data, xmin=30, xmax=40, ymin=2, ymax=7){
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("MVN")
  install_and_load("ggplot2")
  install_and_load("gridExtra")
  install_and_load("gtable")

  Descrip_fiabilidad <- mardia_test(data) %>% as.matrix.data.frame()

  tt2 <- ttheme_default(
    core=list(bg_params = list(fill = c("#F2F3F4","#F2F3F4", "#F2F3F4"), col=NA),
              fg_params=list(fontface= 1)),
    colhead=list(fg_params=list(col="black", fontface=c(1,1,1,1,1))),
    rowhead=list(fg_params=list(col="black", fontface=1)), base_size=8)

  # Calcular la distancia de Mahalanobis
  distancias <- mahalanobis(data, colMeans(data), cov(data))

  # Calcular los cuantiles teóricos de la distribución chi-cuadrado
  teorico <- qchisq(ppoints(length(distancias)), df = ncol(data))

  # Crear un data frame para ggplot
  df1 <- data.frame(Teorico = teorico, Observado = sort(distancias))

  # Crear el gráfico QQ
  p <- ggplot(df1, aes(x = Observado, y = Teorico)) +
    geom_point(shape = 19, color = "gray20", size = 2) +
    geom_abline(slope = 1, intercept = 0, color = "red", size =1.2) +
    theme_bw() +
    labs(title = "Q-Q Plot de Distancias de Mahalanobis", x = "Squared Mahalanobis Distance", y = "Chi-Square Quantile") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) +
    annotation_custom(gridExtra::tableGrob(Descrip_fiabilidad, rows=NULL, theme = tt2) %>%
                        gtable::gtable_add_grob(.,
                                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                                t = 2, b = nrow(.), l = 1, r = ncol(.)) %>%
                        gtable::gtable_add_grob(.,
                                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                                t = 1, l = 1, r = ncol(.)),
                      xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
  return(p)
}
