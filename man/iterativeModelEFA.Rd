\name{iterativeModelEFA}
\alias{iterativeModelEFA}
\title{Iterative Model Refinement for Exploratory Factor Analysis (EFA)}

\description{
  The `iterativeModelEFA` function conducts an iterative refinement process for Exploratory Factor Analysis (EFA) by using the `lavaan` package. It aims to enhance the interpretability of the factor structure by progressively excluding items with significant cross-loadings. This process is based on an initial configuration of the model and the dataset provided.
}

\usage{
iterativeModelEFA(data, n_factors, estimator = "WLSMV", name_items = "RAS", n_items = NULL, specific_items = NULL, rotation = "oblimin", verbose = FALSE)
}

\arguments{
  \item{data}{The dataframe containing observed variables for the EFA.}
  \item{n_factors}{The number of factors to extract in the EFA.}
  \item{estimator}{The estimation method to use. Default is "WLSMV".}
  \item{name_items}{The prefix used for naming items in the dataset. Default is "RAS".}
  \item{n_items}{The total number of items to consider from the dataset. This is optional if 'specific_items' is provided.}
  \item{specific_items}{A vector of specific item names to include in the analysis. This is optional if 'n_items' is provided.}
  \item{rotation}{The rotation method to use for factor analysis. Default is "oblimin".}
  \item{verbose}{Logical; if TRUE, additional details are printed during the execution. Default is FALSE.}
}

\details{
  The function iteratively conducts EFA, each time excluding items that show significant cross-loadings (cross-loadings criteria can be adjusted as needed) across factors. This iterative process aims to refine the factor structure for improved simplicity and interpretability. The process continues until no items meet the exclusion criteria or adjustments no longer improve the model.
}

\value{
  Returns a list containing:
  \itemize{
    \item{final_model}{The final EFA model after iterative refinement.}
    \item{final_items}{The items included in the final model.}
    \item{excluded_items}{Items excluded during the iterative process.}
    \item{all_specifications}{Specifications of all models considered during iterations.}
    \item{all_results}{Results of all models considered during iterations.}
    \item{all_fit_measures}{Fit measures for each iteration.}
    \item{all_interFactors}{Inter-factor correlations for each iteration.}
  }
}

\examples{
# Assuming 'df_filtered' is a pre-prepared dataset
result <- iterativeModelEFA(
  data = df_filtered,
  n_factors = 3,
  estimator = "WLSMV",
  rotation = "oblimin",
  name_items = "RAS",
  n_items = 20
)
print(result)
}

\author{
  Professor José Ventura León
}
