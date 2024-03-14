\name{BootstrapEFA_Algorithm}
\alias{BootstrapEFA_Algorithm}
\title{Bootstrap Algorithm for Exploratory Factor Analysis (EFA)}

\description{
  A refined version of the bootstrap algorithm for exploratory factor analysis (EFA) designed to assess the stability and robustness of EFA models. This modified function evaluates multiple bootstrap samples from the original data, facilitating a deeper understanding of model reliability and item performance.
}

\usage{
BootstrapEFA_Algorithm(original_data, initialModel, n_factors, estimator = "WLSMV", name_items = "RAS", n_items = NULL, specific_items = NULL, rotation = "oblimin", verbose = FALSE, n_samples = 100, seed = NULL)
}

\arguments{
  \item{original_data}{DataFrame containing the observed variables for EFA analysis.}
  \item{initialModel}{A detailed configuration of the initial EFA model, specifying the number of factors, estimation method, item names, optional specific items, rotation method, verbosity of the output, and other settings required for the analysis.}
  \item{n_factors}{The number of factors to extract in the EFA model.}
  \item{estimator}{The estimation method used in the analysis, with "WLSMV" as the default.}
  \item{name_items}{Prefix for item names in the dataset, defaulting to "RAS".}
  \item{n_items}{Optional; specifies the number of items to consider from the dataset. If NULL, all items are included.}
  \item{specific_items}{Optional; allows for specifying particular items to be included in the analysis. NULL indicates no specific items are targeted.}
  \item{rotation}{The rotation method applied to the factor analysis, with "oblimin" as the default.}
  \item{verbose}{Boolean flag to enable detailed output during execution. Defaults to FALSE.}
  \item{n_samples}{Specifies the number of bootstrap samples to generate for analysis. The default is 100.}
  \item{seed}{Seed value for the random number generator to ensure reproducibility of results. Default is NULL, which does not set a seed.}
}

\details{
  This function enhances the traditional EFA bootstrap approach by integrating customizable options for factor analysis and bootstrap sampling. It generates bootstrap samples, applies EFA to each sample according to the initial model configuration, and aggregates results, including errors and warnings for comprehensive analysis.
}

\value{
  The function returns a list containing two major components: 'processed_results'—a collection of EFA results for each bootstrap sample, including model configurations and fit measures; and 'summary'—a summary including the count of warnings and errors encountered, providing insight into model stability and robustness.
}

\examples{
# Given 'data' as your dataset and 'initialModel' specifying your EFA model configuration
res <- BootstrapEFA_Algorithm(original_data = df_filtered,
                       n_factors = 3,
                       estimator = "ULSMV",
                       rotation = "oblimin",
                       name_items = "MA",
                       n_items = 11,
                       n_samples = 1000, seed = 2024)
# Examining errors and results
print(results$summary$count_errors)
print(results$processed_results)
}


\author{
  Professor José Ventura León
}
