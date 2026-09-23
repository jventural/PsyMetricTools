#' @title Launch the SemPowerLab Shiny Application
#' @description Opens an interactive application to plan the sample size of a
#'   predictive design with latent variables: two correlated latent predictors
#'   and a latent criterion, each measured by its own indicators. It wraps
#'   `semPower::semPower.powerRegression()` and answers the two questions that
#'   matter when designing the study: how many participants are needed to
#'   detect the smallest structural coefficient of interest, and what power is
#'   left with the sample the field actually allows. The application draws the
#'   model being computed, plots the power curve, crosses the effect with the
#'   average factor loading, and writes both the reproducible script and a
#'   draft of the Participants section.
#' @param launch.browser Logical. Open the application in the default browser
#'   (default TRUE).
#' @param port Port to run the application on. `NULL` (default) lets Shiny pick
#'   a free one.
#' @param ... Further arguments passed to [shiny::runApp()].
#' @return Invisibly `NULL`. Called for its side effect: the running app.
#' @details The application needs `shiny`, `bslib` and `semPower`, which are
#'   suggested rather than required by PsyMetricTools: install them with
#'   `install.packages(c("shiny", "bslib", "semPower"))` the first time.
#'
#'   The effect that drives the sample size is the *smallest* one: sizing the
#'   study for the larger coefficient leaves the other without power, and it
#'   will come out non-significant even when it exists. That is what the
#'   `nullWhich` argument declares, and the app defaults to it.
#' @references
#'   Moshagen, M., & Bader, M. (2024). semPower: General power analysis for
#'   structural equation models. *Behavior Research Methods, 56*(4),
#'   2901-2922. \doi{10.3758/s13428-023-02254-7}
#' @examples
#' # The application only runs in an interactive session
#' if (interactive()) {
#'   # opens the app in the browser
#'   run_sempowerlab()
#'
#'   # on a fixed port, without opening a browser window
#'   run_sempowerlab(launch.browser = FALSE, port = 7788)
#' }
#' @export
run_sempowerlab <- function(launch.browser = TRUE, port = NULL, ...) {

  faltan <- character(0)
  for (p in c("shiny", "bslib", "semPower", "ggplot2")) {
    if (!requireNamespace(p, quietly = TRUE)) faltan <- c(faltan, p)
  }
  if (length(faltan)) {
    stop("SemPowerLab needs ", paste(sprintf("'%s'", faltan), collapse = ", "),
         ". Install with: install.packages(c(",
         paste(sprintf('"%s"', faltan), collapse = ", "), "))", call. = FALSE)
  }

  ruta <- system.file("shiny", "sempowerlab", package = "PsyMetricTools")
  if (!nzchar(ruta) || !file.exists(file.path(ruta, "app.R"))) {
    stop("The SemPowerLab application was not found inside the installed ",
         "package. Reinstall PsyMetricTools.", call. = FALSE)
  }

  shiny::runApp(appDir = ruta, launch.browser = launch.browser,
                port = port, ...)
  invisible(NULL)
}
