Density_report <- function(edge.matrix){
  n <- nrow(edge.matrix)
  Total_Density <- n*(n-1)/2
  Conexiones_Diff_cero <- sum(getWmat(edge.matrix) != 0)/2
  Densidad <- Conexiones_Diff_cero/Total_Density*100
  resultado <- paste(Conexiones_Diff_cero, " of ", Total_Density, " edges were distinct from zero (", format(Densidad, digits = 2, nsmall = 2), "% of density).", sep = "")
  return(resultado)
}
