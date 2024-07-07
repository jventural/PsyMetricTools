plot_lvm <- function(
    x, # Objeto psychonetrics
    plot = c("network", "loadings", "residcors", "residpcors", "latents"),
    ask,
    rotation = promax,
    vsize = c(5, 5), # Tamaño por defecto de los nodos, ahora como vector
    palette = "Set1", # Paleta de colores por defecto de Color Brewer
    ...
){

  library(qgraph)
  library(RColorBrewer)

  if (missing(ask)) ask <- length(plot) > 1
  parOrig <- par()
  par(ask = ask)

  # Verificar disponibilidad de la paleta y cargar colores
  available_palettes <- rownames(brewer.pal.info)
  if (!palette %in% available_palettes) {
    stop("Specified palette is not available in RColorBrewer.")
  }
  colors <- brewer.pal(brewer.pal.info[palette, "maxcolors"], palette)

  # Asignar nombres de filas a lambda si son NULL y coinciden las dimensiones
  if (is.null(rownames(x@modelmatrices$fullsample$lambda))) {
    if (nrow(x@modelmatrices$fullsample$lambda) == length(colnames(x@sample@covs$fullsample))) {
      rownames(x@modelmatrices$fullsample$lambda) <- colnames(x@sample@covs$fullsample)
    } else {
      stop("Dimensions of lambda and covs do not match.")
    }
  }

  # Corrección para acceder correctamente a las matrices de covarianzas
  obs <- which(rownames(x@modelmatrices$fullsample$lambda) %in% colnames(x@sample@covs$fullsample))

  # Verificar si obs es vacío
  if (length(obs) == 0) {
    stop("No matching observed variables found.")
  }

  # Obtener las correlaciones parciales y residuales
  pcor <- cov2cor(x@modelmatrices$fullsample$sigma_epsilon)

  # Definir 'shape' y asegurarse que 'vsize' es numérico
  shape <- rep("circle", ncol(pcor))
  shape[obs] <- "square"

  # Asegurarse de que vsize es un vector numérico del tamaño adecuado
  if (length(vsize) == 1) {
    vsize <- rep(vsize, 2) # Asegura que vsize siempre tenga dos elementos
  }
  vsize <- as.numeric(vsize)

  Res <- list()

  if ("network" %in% plot){
    Res$network <- qgraph(pcor, ..., title = "Estimated network", color = colors, shape = shape, layout = "spring", vsize = vsize[1])
  }

  if ("residpcors" %in% plot){
    Res$residpcors <- qgraph(pcor[obs, obs], ..., title = "Estimated residual partial correlations", color = colors, shape = "square", layout = "spring", vsize = vsize[2])
  }

  if ("residcors" %in% plot){
    Res$residcors <- qgraph(cov2cor(x@modelmatrices$fullsample$sigma_epsilon), ..., title = "Estimated residual correlations", color = colors, shape = "square", layout = "spring", vsize = vsize[2])
  }

  if ("loadings" %in% plot){
    load <- x@modelmatrices$fullsample$lambda
    # Rotate:
    rot <- rotation(load)
    if (is.matrix(rot)){
      load <- rot
      rotmat <- matrix(1, 1, 1)
    } else {
      load <- rot$loadings
      rotmat <- rot$rotmat
    }
    fCovs <- solve(rotmat) %*% x@modelmatrices$fullsample$sigma_zeta %*% t(solve(rotmat))
    rownames(load) <- rownames(x@modelmatrices$fullsample$lambda)[obs]
    Res$loadings <- qgraph.loadings(load, factorCors = fCovs, ..., title = "Estimated factor loadings", color = colors, labels = rownames(x@modelmatrices$fullsample$lambda)[obs], model = "reflective", vsize = vsize)
  }

  if ("latents" %in% plot){
    sigma_zeta <- x@modelmatrices[["fullsample"]][["sigma_zeta"]]
    latents_colors <- rep(colors, length.out = ncol(sigma_zeta))
    Res$latents <- qgraph(sigma_zeta, ..., title = "Latent variables network", color = latents_colors, shape = "circle", layout = "spring", vsize = vsize[1])
  }

  invisible(Res)
}
