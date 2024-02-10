\name{iterativeModelEFA}
\alias{iterativeModelEFA}
\title{Iterative Model Exploration for Exploratory Factor Analysis (EFA)}

\description{
  This function iteratively explores different configurations for an exploratory factor analysis (EFA) model based on an initial model setup and data. It aims to identify and exclude items with significant cross-loadings across factors, thereby refining the model to achieve a simpler and more interpretable factor structure. The process is guided by iterative adjustments and evaluations, leveraging the flexibility of exploratory factor analysis to uncover underlying factor structures within the data.
}

\usage{
iterativeModelEFA(data, initialModel, MAX_ITERATIONS = 10)
}

\arguments{
  \item{data}{A dataframe containing the observed variables (items) to be analyzed in the EFA model.}
  \item{initialModel}{A list detailing the initial configuration for the EFA model, including the number of factors (\code{n_factors}), the total number of items (\code{n_items}), the naming convention for items (\code{name_items}), and optionally, any items to be excluded from the analysis (\code{exclude_items}).}
  \item{MAX_ITERATIONS}{An optional integer specifying the maximum number of iterations to be performed in the exploratory process. Default is \code{10}.}
}

\details{
  The function begins with an initial model configuration and performs a series of exploratory factor analyses, each time identifying and excluding items that exhibit significant cross-loadings across multiple factors. This iterative refinement process continues until no further items need to be excluded or the maximum number of iterations is reached. The goal is to simplify the factor structure, enhancing the clarity and interpretability of the resulting model. The exploration process leverages the \code{lavaan} package for conducting factor analysis and extracting model fit measures.
}

\value{
  A list containing the final model configurations, model fit measures, excluded items, the total number of iterations completed, and a comprehensive record of the results from each iteration. This output facilitates a detailed examination of the exploratory process and the final refined model.
}

\examples{
# Assuming 'df_filtered' is your dataset and 'modelos1' is the initial model configuration
result <- iterativeModelEFA(data = df_filtered, initialModel = modelos1)

# To view the summary of the exploration process
print(result)
}

\author{
Professor José Ventura León
}
