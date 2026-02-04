\name{Standardized_solutions}
\alias{Standardized_solutions}
\title{Standardized Factor Loadings Extraction Tool}

\description{
This function extracts and formats the standardized factor loadings from a given confirmatory factor analysis (CFA) model. It is aimed at simplifying the interpretation of how observed variables (items) relate to their respective latent factors. The function allows for the application of a threshold to focus on more significant factor-item relationships, thereby facilitating a clearer understanding of the factor structure.
}

\usage{
Standardized_solutions(specification, name_items, apply_threshold = TRUE)
}

\arguments{
  \item{specification}{A fitted CFA model object, usually obtained from the `specification_models` function or other similar functions in the `lavaan` package.}
  \item{name_items}{The prefix or base name of the item variables used in the CFA model.}
  \item{apply_threshold}{Logical. If TRUE, factor loadings less than 0.30 are set to zero. This helps in highlighting stronger relationships between items and factors. Default is TRUE.}
}

\details{
The function retrieves the standardized factor loadings from the specified CFA model, allowing for a quick and clear assessment of each item's contribution to its respective factor. By applying a threshold, the function can help in identifying and focusing on the most significant item-factor associations, which is particularly useful in large models with numerous items.
}

\value{
A data frame where each row corresponds to an item, and columns represent the standardized loadings on each factor. Loadings below the specified threshold (if `apply_threshold` is TRUE) are set to zero, simplifying the interpretation of the factor structure.
}

\examples{
# Assuming 'Specifications' is a list of fitted CFA models and 'CCOV' is the item prefix
result_df <- Standardized_solutions(
  Specifications[[5]],
  name_items = "CCOV",
  apply_threshold = TRUE
)
print(result_df)
}

\author{
Dr. José Ventura-León
}
