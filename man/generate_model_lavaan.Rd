\name{generate_model_lavaan}
\alias{generate_model_lavaan}
\title{Factor Model Specification Generator for Lavaan}

\description{
This function automates the generation of a confirmatory factor analysis (CFA) model specification for use with the Lavaan package in R. It is designed to create a factor model based on a specified number of items, their loadings, threshold values for categorization, and optionally excluding certain items. This function simplifies the process of model specification, especially useful in scenarios with multiple items and complex factor structures.
}

\usage{
generate_model_lavaan(num_items, item_value, t_values, excluded_items = "none")
}

\arguments{
  \item{num_items}{Number of items to include in the factor model.}
  \item{item_value}{Numeric vector containing the loadings of each item on the factor.}
  \item{t_values}{A numeric vector of threshold values for categorizing item responses.}
  \item{excluded_items}{Optional; a vector of item names to be excluded from the model. Default is "none".}
}

\details{
The function begins by creating item names and initializing the factor model equation. It then iteratively adds each item to the model, incorporating specified loadings and threshold values. If certain items are to be excluded (as specified in `excluded_items`), these are commented out in the generated model syntax. This approach allows for flexible and tailored model specifications, catering to a wide range of CFA model designs.
}

\value{
A string representing the Lavaan syntax for the specified CFA model. This can be directly used in Lavaan functions for model estimation.
}

\examples{
# To generate a model with 5 items, loadings of 1, and threshold values at 1 and 2
model_spec <- generate_model_lavaan(
  num_items = 5,
  item_value = rep(1, 5),
  t_values = c(1, 2)
)
print(model_spec)
}

\author{
Dr. José Ventura-León
}
