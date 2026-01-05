\name{generate_modelos}
\alias{generate_modelos}
\title{Generate EFA Model Specifications}

\description{
This function generates lavaan model specifications for exploratory factor analysis (EFA) with varying numbers of factors. It creates model formulas that can be used with lavaan's EFA functionality.
}

\usage{
generate_modelos(n_factors, specific_items = NULL, name_items = NULL,
                 n_items = NULL, exclude_items = NULL)
}

\arguments{
  \item{n_factors}{Integer specifying the maximum number of factors to generate models for.}
  \item{specific_items}{Optional character vector of specific item names to use.}
  \item{name_items}{Prefix for item names (e.g., "IS" for IS1, IS2, etc.).}
  \item{n_items}{Number of items when using name_items prefix.}
  \item{exclude_items}{Character vector of item names to exclude from the model.}
}

\details{
The function generates a list of lavaan model specifications for 1 to n_factors. Each model uses the efa() modifier for exploratory factor analysis. If specific_items is provided, those items are used directly. Otherwise, items are generated using name_items prefix and n_items count.
}

\value{
A list of character strings, each containing a lavaan model specification for the corresponding number of factors.
}

\examples{
\dontrun{
# Generate models for 1 to 3 factors with items IS1 to IS12
models <- generate_modelos(n_factors = 3, name_items = "IS", n_items = 12)

# Generate models with specific items
models <- generate_modelos(n_factors = 2,
                           specific_items = c("item1", "item2", "item3", "item4"))
}
}

\author{
Dr. José Ventura-León
}
