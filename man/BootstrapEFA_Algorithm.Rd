\name{BootstrapEFA_Algorithm}
\alias{BootstrapEFA_Algorithm}
\title{Bootstrap Algorithm for Exploratory Factor Analysis (EFA)}

\description{
  This function performs a bootstrap algorithm for exploratory factor analysis (EFA) on a given dataset, based on an initial model configuration. It generates a specified number of bootstrap samples from the original data, runs iterative EFA on each sample, and tracks the convergence of each analysis. The primary goal is to assess the stability and robustness of the EFA model across multiple bootstrap iterations, providing insights into model reliability and item performance.
}

\usage{
BootstrapEFA_Algorithm(original_data, initialModel, n_samples = 100, seed = NULL)
}

\arguments{
  \item{original_data}{A dataframe containing the observed variables to be analyzed in the EFA model.}
  \item{initialModel}{A list detailing the initial configuration for the EFA model. This should include the necessary parameters to initiate the EFA analysis.}
  \item{n_samples}{An optional integer specifying the number of bootstrap samples to generate and analyze. Default is 100.}
  \item{seed}{An optional seed value for reproducibility of the bootstrap sampling. Default is NULL.}
}

\details{
  The function initiates by generating bootstrap samples from the original dataset. For each sample, it attempts to run an iterative EFA based on the provided initial model configuration. During this process, errors such as convergence issues are caught and reported, allowing the analysis to continue across all samples. The function records the outcome of each EFA analysis, including model fit measures, excluded items, and final model configurations, and returns a summary of results along with the total count of any errors encountered.
}

\value{
  A list containing two elements: 'Results', which is a list of EFA results for each successful bootstrap sample, including goodness-of-fit measures, excluded items, and final model configurations; and 'ErrorCount', an integer representing the number of samples that failed to converge. This output provides a comprehensive overview of the EFA model's performance and stability across bootstrap samples.
}

\examples{
# Assuming 'data' is your dataset and 'initialModel' is your EFA model configuration
results <- BootstrapEFA_Algorithm(original_data = data, initialModel = initialModel)

# To view the summary of successful analyses and the error count
print(results$ErrorCount)
}

\author{
  Professor José Ventura León
}
