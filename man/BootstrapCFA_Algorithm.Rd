\name{BootstrapCFA_Algorithm}
\alias{BootstrapCFA_Algorithm}
\title{Bootstrap Algorithm for Confirmatory Factor Analysis (CFA)}

\description{
This function implements a bootstrap algorithm to perform confirmatory factor analysis (CFA) using the lavaan package, integrating an iterative process of model adjustment. The algorithm generates a specified number of bootstrap samples from the original dataset and applies the `iterativeModelCFA` function to each sample. This approach allows for assessing the stability and robustness of the CFA model across multiple samples, providing insights into the model's generalizability and potential areas for improvement.
}

\usage{
BootstrapCFA_Algorithm(originalData, initialModel, n_samples = 10, seed = NULL, MAX_ITERATIONS = 10, MI_THRESHOLD = 5)
}

\arguments{
  \item{originalData}{The original dataset for the analysis, where rows represent observations and columns represent variables involved in the CFA model.}
  \item{initialModel}{A character string specifying the initial CFA model, following the model specification syntax used by the lavaan package.}
  \item{n_samples}{The number of bootstrap samples to generate and analyze. Default is 10.}
  \item{seed}{An optional integer value to set the seed for random number generation, ensuring reproducibility of the bootstrap samples. Default is NULL.}
  \item{MAX_ITERATIONS}{The maximum number of iterations for model adjustments within each bootstrap sample. Default is 10.}
  \item{MI_THRESHOLD}{The minimum modification index (MI) value that a model modification must exceed to be considered for inclusion in the iterative process. Default is 5.}
}

\details{
The function first generates bootstrap samples from the original dataset, with or without setting a seed for reproducibility. It then applies an iterative CFA model adjustment process to each bootstrap sample, enhancing the model based on modification indices (MI) that exceed a specified threshold. This iterative adjustment is performed using the `iterativeModelCFA` function, which is designed to refine the CFA model for better data fit.

The bootstrap approach provides a robust framework for evaluating the model's stability and identifying consistent model improvements across multiple samples.
}

\value{
A list of results from the CFA analysis for each bootstrap sample. Each element in the list contains:
\itemize{
  \item{FinalModel}{The final adjusted CFA model for the bootstrap sample.}
  \item{FitMeasuresDf}{A dataframe of fit measures for each iteration within the bootstrap sample.}
  \item{StandardizedSolutions}{A list of standardized solutions for each iteration.}
  \item{ModificationsDf}{A dataframe listing all model modifications applied during the iterative process, including modification details and MI values.}
}
}

\examples{
# Define the initial CFA model
initialModel <- '
  Factor1 =~ x1 + x2 + x3
  Factor2 =~ x4 + x5 + x6
'

# Perform bootstrap CFA analysis
bootstrapResults <- BootstrapCFA_Algorithm(originalData = myData, initialModel = initialModel, n_samples = 100, seed = 123)

# Access results from the first bootstrap sample
print(bootstrapResults[[1]]$FinalModel)
print(bootstrapResults[[1]]$FitMeasuresDf)
}

\author{
Professor José Ventura León
}
