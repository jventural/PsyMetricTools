\name{summary_metrics}
\alias{summary_metrics}
\title{Network Performance Metrics Summary Tool}

\description{
This function computes and summarizes key performance metrics for network estimation algorithms, aggregated by algorithm, correlation method, and sample size. It calculates the median values of sensitivity, specificity, precision, correlation, absolute correlation, bias, TEFI, and FDR for each configuration. This tool facilitates a comprehensive comparison of network estimation performance across different settings, making it invaluable for evaluating and optimizing network analysis methodologies.
}

\usage{
summary_metrics(results)
}

\arguments{
  \item{results}{A data frame containing performance metrics obtained from network estimations. This data frame should include columns for `sensitivity`, `specificity`, `precision`, `correlation`, `abs_cor`, `bias`, `TEFI`, and `FDR`, along with categorical columns for `Algorithm`, `Correlation_Method`, and `Sample_Size`.}
}

\details{
The function leverages the `dplyr` package to perform data manipulation tasks. Initially, it groups the input data frame by `Algorithm`, `Correlation_Method`, and `Sample_Size`. For each group, it calculates the median of each metric, handling missing values by excluding them from the median calculation. This approach provides a robust summary of the performance metrics, reducing the influence of outliers and focusing on the central tendency of the results. The output is a tidy data frame with each row representing a unique combination of algorithm, correlation method, and sample size, along with their corresponding median performance metrics.
}

\value{
A data frame with aggregated median values of the specified performance metrics for each combination of algorithm, correlation method, and sample size. This summary facilitates easy comparison and assessment of the performance across different settings and conditions in network analysis.
}

\examples{
# Assuming `results` is a data frame containing the necessary metrics from network estimations
aggregated_metrics <- summary_metrics(results)
print(aggregated_metrics)
}

\author{
José Ventura-León
}
