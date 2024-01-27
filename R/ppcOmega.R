ppcOmega <- function(data, res){
  require(MASS)
  require(ggplot2)

  # Tomar el promedio de las iteraciones para lambda y psi
  lambda <- apply(res$Bayes$loadings, 2, mean)  # Promedio a lo largo de la primera dimensión
  psi <- diag(apply(res$Bayes$resid_var, 2, mean))

  cimpl <- lambda %*% t(lambda) + psi
  sim_eigenvalues <- matrix(NA, nrow = 2000, ncol = ncol(data))

  for (i in 1:2000) {
    dtmp <- MASS::mvrnorm(nrow(data), rep(0, ncol(data)), cimpl)
    sim_eigenvalues[i,] <- sort(eigen(cov(dtmp))$values, decreasing = TRUE)
  }

  # Create a data frame for plotting
  plot_data <- data.frame(
    EigenvalueNo = 1:ncol(data),
    Eigenvalue = sort(eigen(cov(data))$values, decreasing = TRUE),
    SimulationMedian = apply(sim_eigenvalues, 2, median),
    ymin = apply(sim_eigenvalues, 2, quantile, probs = 0.025),
    ymax = apply(sim_eigenvalues, 2, quantile, probs = 0.975)
  )

  # Plot using ggplot2
  p <- ggplot(plot_data, aes(x = EigenvalueNo)) +
    geom_point(aes(y = Eigenvalue), color = "black", size = 3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) +
    theme_minimal() +
    labs(x = "Eigenvalue No.", y = "Eigenvalue Size") +
    ggtitle("Posterior Predictive Check for Omega 1-Factor-Model")

  print(p)
}

