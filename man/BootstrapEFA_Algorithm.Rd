\name{BootstrapEFA_Algorithm}
\alias{BootstrapEFA_Algorithm}
\title{Bootstrap Algorithm for Exploratory Factor Analysis (EFA)}

\description{
  Implements a bootstrap algorithm for exploratory factor analysis (EFA) to evaluate the stability and robustness of an EFA model across multiple bootstrap samples derived from the original dataset. This process aims to provide insights into the model's reliability and the performance of individual items within the model.
}

\usage{
BootstrapEFA_Algorithm(original_data, initialModel, n_samples = 100, seed = NULL)
}

\arguments{
  \item{original_data}{Dataframe with observed variables for the EFA analysis.}
  \item{initialModel}{List detailing the EFA model's initial configuration, including parameters like the number of factors, and any specific settings required for the `lavaan` analysis.}
  \item{n_samples}{Number of bootstrap samples to generate and analyze. Default is 100.}
  \item{seed}{Seed value for reproducibility of bootstrap sampling. Defaults to NULL, allowing for random sampling.}
}

\details{
  The function generates bootstrap samples from the provided dataset, each of which is analyzed using an iterative EFA process based on the initial model configuration. The function captures and reports errors such as convergence issues, allowing for uninterrupted analysis across samples. Results from each analysis, including model fit measures and configurations, are compiled for review.
}

\value{
  Returns a list with two primary elements: 'Results'—a list of detailed EFA results for each bootstrap sample, including fit measures and model configurations; and 'ErrorCount'—the number of samples that failed to converge, highlighting potential stability issues with the model. This comprehensive output aids in assessing the model's performance across different samples.
}

\examples{
# Assuming 'data' is your dataset and 'initialModel' specifies your EFA model setup
results <- BootstrapEFA_Algorithm(original_data = data, initialModel = initialModel, n_samples = 100, seed = 123)

# To review the number of errors and explore the results
print(results$ErrorCount)
print(results$Results)
}

\author{
  Professor José Ventura León
}
