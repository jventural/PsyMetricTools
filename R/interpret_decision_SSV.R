#' @title Interpret Modification Index Decisions
#' @description Interprets modification indices and power decisions from SEM analysis.
#' @param MI_Saris Data frame with modification indices and decision.pow column.
#' @return A data frame with interpretations of each decision.
#' @export
interpret_decision_SSV <- function(MI_Saris) {
  interpretation <- sapply(MI_Saris$decision.pow, function(dec) {
    if(dec == "EPC:M") {
      "MI significativo, alta potencia y EPC suficientemente grande; se recomienda liberar el parametro para ajustar el modelo."
    } else if(dec == "EPC:NM") {
      "MI significativo y alta potencia, pero EPC pequeno; la modificacion no es sustantivamente relevante, se mantiene la especificacion."
    } else if(dec == "M") {
      "MI significativo, pero la potencia es baja; se concluye que hay mala especificacion, aunque la informacion no es suficiente para evaluar su magnitud con precision."
    } else if(dec == "NM") {
      "No hay mala especificacion: o el MI no es significativo o, pese a serlo, la alta potencia indica que el EPC es pequeno; se mantiene la restriccion."
    } else if(dec == "I") {
      "Informacion insuficiente (baja potencia) para tomar una decision clara sobre la mala especificacion; el resultado es inconcluso."
    } else {
      "Decision no reconocida."
    }
  })

  result <- data.frame(
    lhs = MI_Saris$lhs,
    op = MI_Saris$op,
    rhs = MI_Saris$rhs,
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )

  return(result)
}
