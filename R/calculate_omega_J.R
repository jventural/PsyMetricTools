#' @title Omega Coefficient from a Fitted lavaan Model
#' @description Computes McDonald's omega using the R-squared values
#'   (communalities) extracted directly from a fitted lavaan object. Accepts
#'   either a single character vector of item names or a named list of vectors
#'   to compute omega for multiple factors at once.
#' @param items A character vector of item names \emph{or} a named list of
#'   character vectors (one element per factor).
#' @param fit A fitted lavaan object (e.g., from \code{\link[lavaan]{cfa}} or
#'   an EFA specification produced by \code{EFA_modern}).
#' @param digits Integer; number of decimal places for rounding (default 3).
#' @return When \code{items} is a single vector, a numeric scalar (omega).
#'   When \code{items} is a named list, a named numeric vector with one omega
#'   per factor.
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- 'F1 =~ x1 + x2 + x3
#'            F2 =~ x4 + x5 + x6'
#' fit <- cfa(model, data = HolzingerSwineford1939)
#'
#' # Single factor
#' calculate_omega_J(c("x1", "x2", "x3"), fit)
#'
#' # Multiple factors at once
#' calculate_omega_J(
#'   list(F1 = c("x1", "x2", "x3"),
#'        F2 = c("x4", "x5", "x6")),
#'   fit
#' )
#' }
#' @importFrom lavaan lavInspect
#' @importFrom dplyr mutate filter
#' @importFrom tibble rownames_to_column
#' @export
calculate_omega_J <- function(items, fit, digits = 3) {
  omega_one <- function(item_names, fit_obj) {
    rsq <- lavaan::lavInspect(fit_obj, "rsquare")
    df <- data.frame(com = rsq)
    df$res <- 1 - df$com
    df$Items <- rownames(df)
    df_sel <- df[df$Items %in% item_names, , drop = FALSE]
    if (nrow(df_sel) == 0) {
      stop("None of the specified items found in the model: ",
           paste(item_names, collapse = ", "))
    }
    round((sum(df_sel$com)^2) / (sum(df_sel$com)^2 + sum(df_sel$res)), digits)
  }

  if (is.list(items) && !is.data.frame(items)) {
    sapply(items, omega_one, fit_obj = fit)
  } else {
    omega_one(items, fit)
  }
}
