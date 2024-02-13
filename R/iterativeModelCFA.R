iterativeModelCFA <- function(data, initialModel, MAX_ITERATIONS = 10, MI_THRESHOLD = 5) {
  library(lavaan)
  library(dplyr)
  currentModel <- initialModel
  iteration <- 1
  allModifications <- list()
  allFitMeasures <- list()
  allStandardizedSolutions <- list()

  while (iteration <= MAX_ITERATIONS) {
    fit <- cfa(model = currentModel, data = data, estimator = "WLSMV", mimic = "Mplus", ordered = TRUE)

    fitMeasures <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))
    allFitMeasures[[iteration]] <- fitMeasures

    standardizedSolutions <- standardizedSolution(fit) %>%
      dplyr::filter(op == "=~")
    allStandardizedSolutions[[iteration]] <- standardizedSolutions

    modIndices <- modificationIndices(fit, sort = TRUE) %>%
      dplyr::filter(mi > MI_THRESHOLD & op == "~~")

    if (nrow(modIndices) > 0) {
      modSuggestion <- modIndices[1, ]
      modification <- paste(modSuggestion$lhs, "~~", modSuggestion$rhs)
      currentModel <- paste(currentModel, modification, sep = "\n")
      allModifications[[iteration]] <- list(Modification = modification, MI = modSuggestion$mi)
    } else {
      break
    }

    iteration <- iteration + 1
  }

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
