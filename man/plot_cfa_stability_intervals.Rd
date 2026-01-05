\name{plot_cfa_stability_intervals}
\alias{plot_cfa_stability_intervals}
\title{Comprehensive Visualization of Bootstrap CFA Stability Results with Confidence Intervals}
\description{
This function generates a detailed graphical display of bootstrap Confirmatory Factor Analysis (CFA) stability results. It computes the mean and the 95\% confidence intervals (using the 2.5th and 97.5th percentiles) for a range of fit indices across varying sample percentages. The function displays these metrics using line, point, and ribbon plots arranged in separate facets for each index. Users can customize which indices are displayed, exclude specific indices, and rename factor-based indices (e.g., "F1", "F2", etc.) for improved interpretability.
}
\usage{
plot_cfa_stability_intervals(resultados, num_factors = 1,
                             indices_to_plot = "All",
                             indices_to_exclude = NULL,
                             fill_color = "skyblue",
                             alpha_ribbon = 0.2,
                             rename_factors = NULL, ...)
}
\arguments{
  \item{resultados}{Data frame with bootstrap CFA results.}
  \item{num_factors}{Number of factors (default 1).}
  \item{indices_to_plot}{Indices to plot (default "All").}
  \item{indices_to_exclude}{Indices to exclude (default NULL).}
  \item{fill_color}{Fill color (default "skyblue").}
  \item{alpha_ribbon}{Ribbon transparency (default 0.2).}
  \item{rename_factors}{Factor renaming list (default NULL).}
  \item{...}{Additional arguments passed to plotting functions.}
}
\details{
The function processes the input data by:
\enumerate{
  \item Converting the selected fit indices into a long format.
  \item Ensuring the \code{Porcentaje} column is numeric and, if necessary, converting its scale.
  \item Computing the mean, and the lower and upper bounds of the 95\% confidence interval for each fit index at each sample percentage.
  \item Optionally renaming factor-based indices according to user specifications.
  \item Generating a faceted ggplot that displays a line and points for the mean values along with a ribbon for the confidence intervals, with the x-axis reversed (from 90 to 30) to reflect descending sample percentages.
}
This approach enables a comprehensive visual analysis of the stability of CFA fit indices over different sample sizes.
}
\value{
A \code{ggplot} object that visually encapsulates the stability of the CFA fit indices across varying sample percentages, displaying the mean and associated 95\% confidence intervals in separate facets for each index.
}
\examples{
# Assuming 'resultados' is your data frame with bootstrap CFA results
figure <- plot_cfa_stability_intervals(
  resultados,
  num_factors = 2,
  indices_to_plot = c("cfi.scaled", "rmsea.scaled", "F1", "F2"),
  fill_color = "#f8c471",
  rename_factors = c("F1" = "Factor Calidad", "F2" = "Factor Consistencia")
)
print(figure)
}
\author{
Dr. José Ventura-León
}
