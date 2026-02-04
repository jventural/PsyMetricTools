\name{multi_cfa}
\alias{multi_cfa}
\title{Multi-Model Confirmatory Factor Analysis (CFA)}

\description{
  A comprehensive function for conducting Confirmatory Factor Analysis (CFA) across multiple models simultaneously. This function facilitates the assessment of model fit, reliability, and modification indices for several CFA models, integrating conditional orthogonal settings based on user specifications. It enables streamlined evaluation and comparison of different CFA models using various goodness-of-fit measures.
}

\usage{
multi_cfa(modelos, data, estimator, ordered = TRUE, orthogonal_indices = NULL)
}

\arguments{
  \item{modelos}{A list of lavaan model syntaxes or predefined CFA models to be analyzed.}
  \item{data}{DataFrame containing the observed variables for CFA analysis.}
  \item{estimator}{The estimation method to be used in the CFA, such as "MLM" or "WLSMV".}
  \item{ordered}{Boolean flag indicating whether the data variables are ordered (ordinal) factors. Defaults to TRUE.}
  \item{orthogonal_indices}{Optional; a vector of indices specifying which models should include orthogonal (uncorrelated) factors by setting orthogonal = TRUE. NULL indicates no specific orthogonal settings.}
}

\details{
  This function automates the fitting of multiple CFA models, adjusting for orthogonal constraints as specified. It computes a range of goodness-of-fit measures for each model, extracts modification indices for model improvement suggestions, evaluates factor correlations, and calculates reliability metrics. The function is designed to facilitate comprehensive comparative analysis of multiple CFA models within a single execution frame.
}

\value{
  Returns a list containing components for each model: 'fits'—lavaan objects for each fitted model; 'bondades_ajuste'—goodness-of-fit measures; 'indices_modificacion'—suggestions for model modifications; 'correlaciones_factores'—factor correlation matrices; and 'fiabilidad'—reliability measures. These outputs enable detailed evaluation and comparison of model performances.
}

\examples{
# Define multiple CFA models in 'modelList' and your dataset as 'data'
results <- multi_cfa(modelos = modelList,
                      data = yourDataset,
                      estimator = "MLM",
                      ordered = TRUE,
                      orthogonal_indices = c(2, 4))
# To explore goodness-of-fit measures for the first model
print(results$bondades_ajuste[["Modelo1"]])
# To examine factor correlations in the second model
print(results$correlaciones_factores[["Modelo2"]])
}

\author{
Dr. José Ventura-León
}

