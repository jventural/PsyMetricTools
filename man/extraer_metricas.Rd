\name{extraer_metricas}
\alias{extraer_metricas}
\title{Model Fit Metrics Extraction Tool}

\description{
This function extracts and organizes key fit metrics from a fitted model object. It is designed to streamline the process of retrieving important model fit indices, such as degrees of freedom, chi-square, AIC, BIC, and others, providing a clear and concise summary in a well-structured format. This tool is particularly useful for comparing model performance and assessing model adequacy in structural equation modeling (SEM) and other multivariate analyses.
}

\usage{
extraer_metricas(modelo_fit)
}

\arguments{
  \item{modelo_fit}{The fitted model object from which the fit metrics will be extracted. This object should contain the fit measures in a format compatible with the function's operations.}
}

\details{
The function begins by loading the necessary libraries (`dplyr` and `tidyr`). It then converts the fit measures from the model object into a tibble for easier manipulation. The function filters the tibble to retain only the specified fit metrics (degrees of freedom, chi-square, AIC, BIC, EBIC, RMSEA, TLI, CFI) and renames these metrics to a standardized format. The metrics are ordered in a predefined sequence and reshaped into a wide format. Finally, the function adds the name of the model as a column for easy identification and returns the organized data frame.
}

\value{
A data frame containing the extracted fit metrics in a wide format. Each row represents a model, and each column represents a different fit metric. The data frame includes the model name for easy reference.
}

\examples{
# Assuming `mod` is a fitted model object with the necessary fit measures
modelo_fit <- fit(mod)
metricas <- extraer_metricas(modelo_fit)
print(metricas)
}

\author{
José Ventura-León
}
