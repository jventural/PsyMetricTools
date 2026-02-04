\name{comparation_metric}
\alias{comparation_metric}
\title{Model Fit Metrics Comparison Tool}

\description{
This function extracts and organizes key fit metrics from multiple fitted model objects. It is designed to streamline the process of retrieving important model fit indices, such as degrees of freedom, chi-square, AIC, BIC, and others, providing a clear and concise summary in a well-structured format. This tool is particularly useful for comparing model performance and assessing model adequacy in structural equation modeling (SEM) and other multivariate analyses.
}

\usage{
comparation_metric(...)

# Example usage:
# comparation_metric(CFA = modelo_fit1, LNM = modelo_fit2)
}

\arguments{
  \item{...}{One or more fitted model objects from which the fit metrics will be extracted. These objects should contain the fit measures in a format compatible with the function's operations. The names of these arguments will be used as the model names in the output.}
}

\details{
The function begins by capturing the list of fitted model objects and their names. It then uses an internal function to convert the fit measures from each model object into a tibble for easier manipulation. The internal function filters the tibble to retain only the specified fit metrics (degrees of freedom, chi-square, AIC, BIC, EBIC, RMSEA, TLI, CFI) and renames these metrics to a standardized format. The metrics are ordered in a predefined sequence and reshaped into a wide format. The function adds the name of each model as a column for easy identification. Finally, the function combines the results from all models into a single data frame and returns it.
}

\value{
A data frame containing the extracted fit metrics in a wide format. Each row represents a model, and each column represents a different fit metric. The data frame includes the model names for easy reference.
}

\examples{
# Call the function with named arguments
resultados <- comparation_metric(CFA = modelo_fit1, LNM = modelo_fit2)

# Print the results
print(resultados)
}

\author{
José Ventura-León
}

