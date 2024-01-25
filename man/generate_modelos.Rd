\name{generate_modelos}
\alias{generate_modelos}
\title{Exploratory Factor Analysis Model Generation Tool}

\description{
This function dynamically generates a series of exploratory factor analysis (EFA) model specifications for use with the `lavaan` package. It is designed to facilitate the creation of multiple EFA models, each with a different number of factors, allowing for a comprehensive exploratory analysis. The function can also exclude specified items from the model, providing flexibility in model specification.
}

\usage{
generate_modelos(n_factors, n_items, name_items, exclude_items = NULL)
}

\arguments{
  \item{n_factors}{The number of factors to include in the largest model. Models with fewer factors (down to 1) will also be generated.}
  \item{n_items}{The total number of items to be considered in the model.}
  \item{name_items}{The base name or prefix of the item variables. The function assumes item names are structured as the base name followed by a sequential number.}
  \item{exclude_items}{An optional vector of item names to be excluded from the model. Defaults to NULL, meaning no items are excluded.}
}

\details{
The function generates a list of EFA model specifications. Each model in the list includes a different number of factors, ranging from 1 to the specified `n_factors`. The item variables are created by concatenating `name_items` with a sequence of numbers up to `n_items`, and any specified `exclude_items` are omitted. This allows for a systematic exploration of factor structures with varying complexity.
}

\value{
A list of EFA model specifications as strings. Each string can be directly used in functions from the `lavaan` package to specify an exploratory factor analysis model.
}

\examples{
# Generate EFA models with 5 factors, 27 items named 'CCOV', excluding 'CCOV1' and 'CCOV5'
modelos <- generate_modelos(n_factors = 5, n_items = 27, name_items = "CCOV", exclude_items = c("CCOV1", "CCOV5"))
print(modelos)
}

\author{
Professor José Ventura León
}
