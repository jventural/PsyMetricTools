#' @name iterativeModelCFA
#' @export
iterativeModelCFA <- function(data, initialModel, MAX_ITERATIONS = 10, MI_THRESHOLD = 5) {
  # Inicializa la barra de progreso
  pb <- utils::txtProgressBar(min = 0, max = MAX_ITERATIONS, style = 3)

  currentModel <- initialModel
  iteration <- 1
  allModifications <- list()
  allFitMeasures <- list()
  allStandardizedSolutions <- list()

  while (iteration <= MAX_ITERATIONS) {
    fit <- lavaan::cfa(model = currentModel, data = data, estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)

    fitMeasures <- lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))
    allFitMeasures[[iteration]] <- fitMeasures

    standardizedSolutions <- lavaan::standardizedSolution(fit) %>%
      dplyr::filter(op == "=~")
    allStandardizedSolutions[[iteration]] <- standardizedSolutions

    modIndices <- lavaan::modificationIndices(fit, sort = TRUE) %>%
      dplyr::filter(mi > MI_THRESHOLD & op == "~~")

    if (nrow(modIndices) > 0) {
      modSuggestion <- modIndices[1, ]
      modification <- paste(modSuggestion$lhs, "~~", modSuggestion$rhs)
      currentModel <- paste(currentModel, modification, sep = "\n")
      allModifications[[iteration]] <- list(Modification = modification, MI = modSuggestion$mi)
    } else {
      # Actualiza la barra de progreso al 100% si no hay más modificaciones antes de terminar el ciclo
      utils::setTxtProgressBar(pb, MAX_ITERATIONS)
      break
    }

    # Actualiza la barra de progreso en cada iteración
    utils::setTxtProgressBar(pb, iteration)
    iteration <- iteration + 1
  }

  # Asegúrate de actualizar la barra de progreso al 100% al finalizar todas las iteraciones
  utils::setTxtProgressBar(pb, MAX_ITERATIONS)
  close(pb)

  fitMeasuresDf <- do.call(rbind, lapply(allFitMeasures, function(x) t(as.data.frame(x))))
  rownames(fitMeasuresDf) <- paste("Iteration", seq_along(allFitMeasures))

  modificationsDf <- do.call(rbind, lapply(seq_along(allModifications), function(i) {
    data.frame(
      Iteration = paste("Iteration", i),
      Modification = allModifications[[i]]$Modification,
      MI = allModifications[[i]]$MI,
      stringsAsFactors = FALSE
    )
  }))

  list(
    FinalModel = currentModel,
    FitMeasuresDf = fitMeasuresDf,
    StandardizedSolutions = allStandardizedSolutions,
    ModificationsDf = modificationsDf
  )
}
