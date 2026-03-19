#' @title Safe CFA Wrapper
#' @description Fits a CFA model using \code{\link[lavaan]{cfa}} with error
#'   handling. Returns \code{NULL} instead of raising an error when the model
#'   fails to converge or encounters estimation problems.
#' @param syntax A character string with the lavaan model syntax.
#' @param data A data frame containing the observed variables.
#' @param items Character vector of item names to treat as ordered (categorical).
#' @param estimator Estimator to use (default \code{"WLSMV"}).
#' @param stdlv Logical; standardize latent variables (default \code{TRUE}).
#' @return A fitted lavaan object, or \code{NULL} if the model failed.
#' @examples
#' \dontrun{
#' model <- 'F1 =~ x1 + x2 + x3'
#' fit <- safe_cfa(model, data = mydata, items = c("x1", "x2", "x3"))
#' if (!is.null(fit)) summary(fit)
#' }
#' @importFrom lavaan cfa inspect
#' @export
safe_cfa <- function(syntax, data, items, estimator = "WLSMV", stdlv = TRUE) {
  fit <- try(lavaan::cfa(syntax, data = data, estimator = estimator,
                         ordered = items, std.lv = stdlv), silent = TRUE)
  if (inherits(fit, "try-error") || !isTRUE(lavaan::inspect(fit, "converged"))) {
    return(NULL)
  }
  fit
}


#' @title Safe Fit Measures Extraction
#' @description Extracts selected fit measures from a lavaan object. Returns a
#'   one-row data frame with \code{NA} values for any measures that could not be
#'   retrieved (e.g., when the model is \code{NULL}).
#' @param fit A fitted lavaan object or \code{NULL}.
#' @param wanted Character vector of fit measure names to extract (default
#'   includes chi-square, df, CFI, TLI, RMSEA, SRMR, and WRMR, all scaled).
#' @return A one-row data frame with the requested fit measures.
#' @examples
#' \dontrun{
#' fit <- safe_cfa(model, data = mydata, items = items)
#' safe_measures(fit)
#' }
#' @importFrom lavaan fitMeasures
#' @importFrom stats setNames
#' @export
safe_measures <- function(fit, wanted = c("chisq.scaled", "df.scaled",
                                          "cfi.scaled", "tli.scaled",
                                          "rmsea.scaled", "srmr", "wrmr")) {
  vals <- stats::setNames(rep(NA_real_, length(wanted)), wanted)
  if (!is.null(fit)) {
    fm <- suppressWarnings(lavaan::fitMeasures(fit))
    cm <- intersect(names(fm), wanted)
    vals[cm] <- round(fm[cm], 3)
  }
  as.data.frame(as.list(vals), check.names = FALSE)
}


#' @title Simple Admissibility Check for CFA
#' @description Checks whether a fitted CFA model has admissible solutions by
#'   detecting negative residual variances (Heywood cases) and standardized
#'   factor loadings with absolute value greater than 1.
#' @param fit A fitted lavaan object or \code{NULL}.
#' @param latents Character vector of latent variable names to check.
#' @return A character string: \code{"OK"} if admissible,
#'   \code{"NONCONV/FAIL"} if the model is \code{NULL}, or a description of
#'   the problems found.
#' @examples
#' \dontrun{
#' fit <- safe_cfa(model, data = mydata, items = items)
#' latents <- lavaan::lavNames(fit, type = "lv")
#' admis_simple(fit, latents)
#' }
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @export
admis_simple <- function(fit, latents) {
  if (is.null(fit)) return("NONCONV/FAIL")
  pe  <- lavaan::parameterEstimates(fit, standardized = TRUE)
  neg <- subset(pe, pe$op == "~~" & pe$lhs == pe$rhs &
                  pe$lhs %in% latents & pe$est < 0)$lhs
  std <- try(lavaan::standardizedSolution(fit), silent = TRUE)
  big <- if (inherits(std, "try-error")) {
    character(0)
  } else {
    subset(std, std$op == "=~" & std$lhs %in% latents &
             abs(std$est.std) > 1)$rhs
  }
  if (length(neg) == 0 && length(big) == 0) {
    "OK"
  } else {
    paste(c(if (length(neg)) paste0("neg_resid:", paste(neg, collapse = ",")),
            if (length(big)) paste0("stdload>1:", paste(big, collapse = ","))),
          collapse = " | ")
  }
}
