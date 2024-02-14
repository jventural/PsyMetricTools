\name{iterativeModelEFA}
\alias{iterativeModelEFA}
\title{Iterative Model Exploration for Exploratory Factor Analysis (EFA)}

\description{
  Performs iterative explorations of EFA model configurations based on an initial setup and dataset. It refines the model by identifying and excluding items with significant cross-loadings, aiming for a simpler, interpretable factor structure. Utilizes the `lavaan` package for factor analysis and fit measure extraction.
}

\usage{
iterativeModelEFA(data, initialModel, MAX_ITERATIONS = 10)
}

\arguments{
  \item{data}{Dataframe with observed variables for EFA.}
  \item{initialModel}{List with initial EFA configuration: \code{n_factors} (number of factors), \code{n_items} (total items), \code{name_items} (item naming convention), and optionally \code{exclude_items} (items to exclude).}
  \item{MAX_ITERATIONS}{Maximum iterations for the exploratory process. Default is \code{10}.}
}

\details{
  The function iteratively conducts EFA, each time excluding items that show significant cross-loadings (>0.3) across factors. This process continues until no items meet the exclusion criteria or the max iterations are reached, enhancing model simplicity and interpretability.
}

\value{
  Returns a list with final model configurations, fit measures, excluded items, iteration count, and iteration results, facilitating detailed exploration and final model understanding.
}

\examples{
# Assuming 'df_filtered' is a dataset and 'modelos1' is the initial model setup
result <- iterativeModelEFA(data = df_filtered, initialModel = modelos1)
print(result)  # View exploration summary
}

\author{
Professor José Ventura León
}

