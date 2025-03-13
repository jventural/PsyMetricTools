interpret_decision_SSV <- function(MI_Saris) {
  interpretation <- sapply(MI_Saris$decision.pow, function(dec) {
    if(dec == "EPC:M") {
      "MI significativo, alta potencia y EPC suficientemente grande; se recomienda liberar el parámetro para ajustar el modelo."
    } else if(dec == "EPC:NM") {
      "MI significativo y alta potencia, pero EPC pequeño; la modificación no es sustantivamente relevante, se mantiene la especificación."
    } else if(dec == "M") {
      "MI significativo, pero la potencia es baja; se concluye que hay mala especificación, aunque la información no es suficiente para evaluar su magnitud con precisión."
    } else if(dec == "NM") {
      "No hay mala especificación: o el MI no es significativo o, pese a serlo, la alta potencia indica que el EPC es pequeño; se mantiene la restricción."
    } else if(dec == "I") {
      "Información insuficiente (baja potencia) para tomar una decisión clara sobre la mala especificación; el resultado es inconcluso."
    } else {
      "Decisión no reconocida."
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
