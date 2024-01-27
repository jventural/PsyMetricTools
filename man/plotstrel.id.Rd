\name{plotstrel.id}
\alias{plotstrel.id}
\title{Bayesian Estimation Item Deletion Effect Visualization Tool}

\description{
This function is based on the first version of the Bayesrel library published on January 20, 2019 by author Julius Pfadt and Donvanden Bergh. However, since it does not appear in the latest version of the Bayesrel library, it has been modified to be compatible with it.
This function generates a graphical visualization of how the removal of different elements affects a specific estimate in a Bayesian model. It is used to assess the individual influence of items on the overall estimate, providing a deeper understanding of the contribution of each item. This is particularly useful in sensitivity analyses, where the robustness of estimates to changes in the data is examined.
}

\usage{
plotstrel.id(x, estimate, ordering = FALSE)
}

\arguments{
  \item{x}{A list containing the elements 'Bayes' and 'data', where 'Bayes' is a subset of results from a Bayesian model and 'data' is the dataset used in the model.}
  \item{estimate}{A string indicating the specific estimate to be examined.}
  \item{ordering}{Logical. If TRUE, items are ordered based on their effect on the estimate. Default is FALSE.}
}

\details{
The function first checks that the necessary elements are present in the list 'x'. Then, it extracts the values of the specified estimate and compares them with and without the inclusion of each item. It uses ridge density plots to visualize the distribution of the estimate values with and without each item, allowing for a visual comparison of their effects.
}

\value{
A data frame where each row corresponds to an item, and columns represent the standardized loadings on each factor. Loadings below the specified threshold (if `apply_threshold` is TRUE) are set to zero, simplifying the interpretation of the factor structure.
}

\examples{
# Assuming 'Specifications' is a list of fitted CFA models and 'CCOV' is the item prefix
result_df <- Standardized_solutions(Specifications[[5]], name_items = "CCOV", apply_threshold = TRUE)
print(result_df)
}

\author{
Professor José Ventura León
}
