\name{Standardized_solutions_cfa}
\alias{Standardized_solutions_cfa}
\title{Factor Loading Extraction Tool for Confirmatory Factor Analysis}

\description{
This function is designed to extract and format the standardized factor loadings from a confirmatory factor analysis (CFA) model. It is particularly useful for interpreting and evaluating the relationships between observed variables (items) and their respective latent factors. The function also offers the option to apply a threshold, filtering out loadings below a specified value, which aids in focusing on more significant item-factor relationships.
}

\usage{
Standardized_solutions_cfa(specification, name_items, apply_threshold = TRUE)
}

\arguments{
  \item{specification}{A fitted CFA model object, obtained from the `specification_models` function or other similar functions in the `lavaan` package.}
  \item{name_items}{The prefix or base name of the item variables used in the CFA model.}
  \item{apply_threshold}{Logical. If TRUE, factor loadings less than 0.30 are set to zero, highlighting more significant relationships. Default is TRUE.}
}

\details{
The function retrieves the standardized factor loadings from the specified CFA model. Loadings are then arranged and formatted for easier interpretation. The user can specify whether to apply a threshold to the loadings, which can be particularly useful in large models with many items, helping to clarify the structure by emphasizing stronger item-factor associations.
}

\value{
A data frame where each row corresponds to an item, and columns represent the standardized loadings on each factor. Loadings below the specified threshold (if `apply_threshold` is TRUE) are set to zero.
}

\examples{
# Assuming 'Specifications.fit' is a fitted CFA model and 'CCOV' is the item prefix
result_df <- Standardized_solutions_cfa(
  Specifications.fit,
  name_items = "CCOV",
  apply_threshold = TRUE
)
print(result_df)
}

\author{
Dr. José Ventura-León
}
