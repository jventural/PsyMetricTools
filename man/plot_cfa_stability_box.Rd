\name{plot_cfa_stability_box}
\alias{plot_cfa_stability_box}
\title{Create Stability Plots for Model Fit Indices and Reliability}

\description{
The `plot_cfa_stability_box` function generates a set of stability plots for key model fit indices and reliability measures, based on different percentages of sampled data. This function produces boxplots for indices such as CFI, RMSEA, SRMR, CRMR, and reliability, allowing for a visual assessment of their stability across different subsamples.
}

\usage{
plot_cfa_stability_box(resultados,
                    y_min_cfi = 0.9, y_max_cfi = 1,
                    y_min_rmsea = 0, y_max_rmsea = 0.15,
                    y_min_srmr = 0, y_max_srmr = 0.15,
                    y_min_crmr = 0, y_max_crmr = 0.15,
                    y_min_reliability = 0.50, y_max_reliability = 1,
                    y_breaks = 0.01,
                    y_breaks_reliability = 0.05,
                    hline_color = "red",
                    xlab_size = 12,
                    ylab_size = 12,
                    label_size = 10)
}

\arguments{
  \item{resultados}{A \code{data.frame} containing the results of a Confirmatory Factor Analysis (CFA) or similar analysis. This object should include model fit indices and reliability measures for different subsamples.}
  \item{y_min_cfi}{Minimum value for the CFI axis.}
  \item{y_max_cfi}{Maximum value for the CFI axis.}
  \item{y_min_rmsea}{Minimum value for the RMSEA axis.}
  \item{y_max_rmsea}{Maximum value for the RMSEA axis.}
  \item{y_min_srmr}{Minimum value for the SRMR axis.}
  \item{y_max_srmr}{Maximum value for the SRMR axis.}
  \item{y_min_crmr}{Minimum value for the CRMR axis.}
  \item{y_max_crmr}{Maximum value for the CRMR axis.}
  \item{y_min_reliability}{Minimum value for the reliability axis.}
  \item{y_max_reliability}{Maximum value for the reliability axis.}
  \item{y_breaks}{Incremental breaks for the CFI, RMSEA, SRMR, and CRMR axes.}
  \item{y_breaks_reliability}{Incremental breaks for the reliability axis.}
  \item{hline_color}{Color of the horizontal reference lines in the plots.}
  \item{xlab_size}{Text size for the x-axis labels.}
  \item{ylab_size}{Text size for the y-axis labels.}
  \item{label_size}{Text size for the plot labels (A, B, C, D, E).}
}

\details{
The `plot_cfa_stability_box` function creates boxplots for CFI, RMSEA, SRMR, CRMR, and reliability, displaying their distribution across different percentages of sampled data. The function allows customization of axis limits, break points, and label sizes, providing flexibility in how the stability of these indices is visualized.

To ensure a smooth execution, the function suppresses any messages and warnings that might be generated during the plotting process. The resulting figure consists of five subplots, arranged in a 3x2 grid, each representing a different model fit index or reliability measure.

This function is particularly useful for evaluating the robustness and stability of model fit indices and reliability across various data subsamples in confirmatory factor analysis or similar contexts.
}

\value{
A ggplot object containing the following boxplots:
\itemize{
  \item \code{CFI} - Stability of the Comparative Fit Index.
  \item \code{RMSEA} - Stability of the Root Mean Square Error of Approximation.
  \item \code{SRMR} - Stability of the Standardized Root Mean Square Residual.
  \item \code{CRMR} - Stability of the Coefficient of Residual Mean Square.
  \item \code{Reliability} - Stability of the omega reliability coefficient.
}
}

\examples{
# Example usage of the plot_cfa_stability_box function

# Assuming `resultados` is a data frame with CFA results
stability_plots <- plot_cfa_stability_box(resultados,
                                       y_min_cfi = 0.9, y_max_cfi = 1,
                                       y_min_rmsea = 0, y_max_rmsea = 0.15,
                                       y_min_srmr = 0, y_max_srmr = 0.15,
                                       y_min_crmr = 0, y_max_crmr = 0.15,
                                       y_min_reliability = 0.50, y_max_reliability = 1,
                                       y_breaks = 0.01,
                                       y_breaks_reliability = 0.05,
                                       hline_color = "red",
                                       xlab_size = 12,
                                       ylab_size = 12,
                                       label_size = 10)

# Display the stability plots
print(stability_plots)
}

\author{
Dr. José Ventura-León
}
