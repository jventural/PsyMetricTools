
\name{createInitialModel}
\alias{createInitialModel}
\title{Create Initial Model Configuration for Confirmatory Factor Analysis (CFA)}

\description{
  This function initializes a configuration for a confirmatory factor analysis (CFA) model by specifying the number of factors, the number of items, the naming convention for items, and optionally, any items to be excluded from the analysis. It creates a structured list that can be used as input for further model generation and analysis processes, facilitating the automation of CFA model preparation and enhancing the reproducibility and efficiency of psychometric analyses.
}

\usage{
createInitialModel(n_factors, n_items, name_items, exclude_items = NULL)
}

\arguments{
  \item{n_factors}{An integer indicating the number of latent factors to be included in the CFA model.}
  \item{n_items}{An integer specifying the total number of observed variables (items) to be considered for the model.}
  \item{name_items}{A character string that serves as a prefix or naming convention for the observed variables. This aids in the automated generation of item names within the model syntax.}
  \item{exclude_items}{An optional vector of item names to be excluded from the model. This can be used to omit specific items from the analysis based on prior considerations or empirical findings. Default is \code{NULL}, indicating that no items are excluded.}
}

\details{
  The function packages the provided parameters into a structured list that delineates the basic configuration for a CFA model. This includes the number of factors and items, the naming convention for items, and any exclusions. The output of this function is intended to streamline the preparation phase of CFA modeling, particularly when working with the `lavaan` package for structural equation modeling in R. It allows for a more dynamic and flexible approach to specifying initial model parameters, especially in scenarios where model configurations might need to be adjusted or replicated across multiple analyses.
}

\value{
  Returns a list containing the initial configuration for a CFA model, including the specified number of factors, items, item naming convention, and any excluded items. This list can be directly utilized in subsequent functions for generating and analyzing CFA models.
}

\examples{
# Initialize a model configuration for a CFA
# with 2 factors, 10 items named 'IR'
initialModel <- createInitialModel(
  n_factors = 2,
  n_items = 10,
  name_items = "IR"
)

# To inspect the initial model configuration
print(initialModel)
}

\author{
Dr. José Ventura-León
}
