\name{plot_performance_metrics}
\alias{plot_performance_metrics}
\title{Performance Metrics Visualization Tool}

\description{
This function visualizes key performance metrics for network estimation algorithms, displaying the results aggregated by algorithm, correlation method, and sample size. It generates line plots of the median values of sensitivity, specificity, precision, correlation, absolute correlation, and bias, facilitating a detailed comparison of network estimation performance across different configurations. This tool is essential for visually assessing and comparing the effectiveness and reliability of various network analysis methodologies.
}

\usage{
plot_performance_metrics(averaged_results)
}

\arguments{
  \item{averaged_results}{A data frame containing median performance metrics obtained from network estimations. This data frame should include columns for `avg_sensitivity`, `avg_specificity`, `avg_precision`, `avg_correlation`, `avg_abs_cor`, and `avg_bias`, categorized by `Algorithm`, `Correlation_Method`, and `Sample_Size`.}
}

\details{
The function leverages the `ggplot2` package to create detailed line plots. It first transforms the data into a long format suitable for ggplot using the `pivot_longer` function from the `tidyr` package. Each performance metric is plotted as a separate line within a facet grid, organized by metric and algorithm, enabling a clear visual comparison across different sample sizes and correlation methods. The inclusion of different line types and colors enhances the distinction between correlation methods, aiding in the effective visualization of trends and differences.
}

\value{
A ggplot object representing the line plots of median performance metrics, arranged in a facet grid. This plot facilitates an intuitive and immediate visual comparison of how different algorithms and correlation methods perform across various sample sizes.
}

\examples{
# Assuming `averaged_results` is a data frame containing the necessary median metrics
plot <- plot_performance_metrics(averaged_results)
print(plot)
}

\author{
Joel Moreno
}
