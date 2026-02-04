\name{boot_cfa_stability}
\alias{boot_cfa_stability}
\title{Bootstrap CFA Stability Analysis with Parallel Processing}

\description{
The `boot_cfa_stability` function performs a bootstrap-based stability analysis of a confirmatory factor analysis (CFA) model. The function evaluates the stability of the model's fit across different sample sizes, utilizing parallel processing to accelerate the computation of multiple bootstrap replications.
}

\usage{
boot_cfa_stability(modelo, data, num_replicas, estimator,
                   seed = 2023, n_cores = 4)
}

\arguments{
  \item{modelo}{A string representing the CFA model specification in `lavaan` syntax.}
  \item{data}{A dataframe containing the data to be used in the CFA model. The columns should correspond to the variables defined in the model.}
  \item{num_replicas}{An integer specifying the number of bootstrap replications to perform. Each replication involves resampling the data and refitting the CFA model.}
  \item{estimator}{A string specifying the estimator to be used in the CFA model. Common options include "MLR", "WLSMV", and "ULSMV".}
  \item{seed}{An optional integer to set the random seed for reproducibility. Default is 2023.}
  \item{n_cores}{An integer specifying the number of CPU cores to be used for parallel processing. Default is 4.}
}

\details{
The function first sets up a parallel processing environment using the `future` and `future.apply` packages, allowing the computation to be distributed across multiple CPU cores.

The analysis proceeds by resampling the original dataset at varying percentages of the total sample size (from 90\% to 30\%), and refitting the specified CFA model to each resampled dataset. For each replication, the function calculates the model fit indices and reliability measures.

Two progress bars are displayed during the execution of the function: one for tracking the bootstrap replications and another for the computation of fit measures for each model.

The results are consolidated into a single dataframe, which includes the fit indices, reliability measures, and corresponding replication and percentage values.
}

\value{
A dataframe containing the following columns:
\itemize{
  \item \code{chisq.scaled}: The scaled chi-square statistic of the model.
  \item \code{df.scaled}: The degrees of freedom for the chi-square statistic.
  \item \code{srmr}: The standardized root mean square residual.
  \item \code{wrmr}: The weighted root mean square residual.
  \item \code{cfi.scaled}: The comparative fit index, scaled for non-normality.
  \item \code{crmr}: The correlation root mean square residual.
  \item \code{tli.scaled}: The Tucker-Lewis Index, scaled for non-normality.
  \item \code{rmsea.scaled}: The root mean square error of approximation, scaled for non-normality.
  \item \code{F1}: The reliability measure (e.g., composite reliability) for the first factor.
  \item \code{Replica}: The replication number for each bootstrap sample.
  \item \code{Porcentaje}: The percentage of the original sample size used in each replication.
}
}

\examples{
# Example usage of the boot_cfa_stability function

# Define a CFA model
modelo <- 'F1 =~ item1 + item2 + item3'

# Load necessary libraries
library(lavaan)
library(semTools)
library(future.apply)

# Example dataset
data <- data.frame(item1 = rnorm(100), item2 = rnorm(100), item3 = rnorm(100))

# Perform bootstrap stability analysis
resultados <- boot_cfa_stability(
  modelo, data,
  num_replicas = 100,
  estimator = "MLR",
  seed = 2023,
  n_cores = 4
)

# View the results
head(resultados)
}

\author{
Dr. José Ventura-León
}
