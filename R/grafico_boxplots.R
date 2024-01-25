grafico_boxplots <- function(data, cols) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("reshape2")
  install_and_load("ggplot2")

  data <- data %>% mutate(ID = 1:nrow(data))

  dat.m <- melt(data, id.vars = 'ID', measure.vars = cols) %>%
    rename(Variables = "variable")

  p <- ggplot(dat.m) +
    geom_boxplot(aes(x=ID, y=value, fill=Variables)) +
    facet_wrap(~Variables, scales = "free") +
    theme_bw() +
    xlab(" ")+
    theme(legend.position = "none")

  return(p)
}

