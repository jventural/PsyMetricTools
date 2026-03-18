#' @title Average Variance Extracted and Composite Reliability
#' @description Computes the Average Variance Extracted (AVE) and Composite
#'   Reliability (CR) for each latent factor in a fitted lavaan CFA model,
#'   using standardized factor loadings and error variances.
#' @param fit A fitted lavaan object (e.g., from \code{\link[lavaan]{cfa}}).
#' @return A data frame with one row per latent factor containing: \code{latent},
#'   \code{CR}, and \code{AVE}, both rounded to three decimal places.
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- 'F1 =~ x1 + x2 + x3
#'            F2 =~ x4 + x5 + x6'
#' fit <- cfa(model, data = HolzingerSwineford1939)
#' get_ave_cr(fit)
#' }
#' @importFrom lavaan standardizedSolution
#' @export
get_ave_cr <- function(fit) {
  std <- lavaan::standardizedSolution(fit)
  L  <- subset(std, std$op == "=~", select = c("lhs", "rhs", "est.std"))
  Th <- subset(std, std$op == "~~" & std$lhs == std$rhs &
                 !(std$lhs %in% unique(L$lhs)),
               select = c("lhs", "est.std"))
  facs <- unique(L$lhs)
  out <- lapply(facs, function(f) {
    lam   <- L$est.std[L$lhs == f]
    inds  <- L$rhs[L$lhs == f]
    theta <- Th$est.std[match(inds, Th$lhs)]
    CR  <- (sum(lam))^2 / ((sum(lam))^2 + sum(theta))
    AVE <- sum(lam^2) / (sum(lam^2) + sum(theta))
    data.frame(latent = f, CR = round(CR, 3), AVE = round(AVE, 3))
  })
  do.call(rbind, out)
}
