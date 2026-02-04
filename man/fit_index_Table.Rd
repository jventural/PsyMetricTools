\name{fit_index_Table}
\alias{fit_index_Table}
\title{Create a Summary Table of Model Fit Indices and Reliability}

\description{
The `fit_index_Table` function generates a summary table that combines key model fit indices and reliability measures from the results of a structural equation modeling (SEM) analysis. The function extracts and formats these indices into a tidy tibble, ready for further analysis or reporting.
}

\usage{
fit_index_Table(resultados)
}

\arguments{
  \item{resultados}{An object containing the results of a structural equation modeling (SEM) analysis. This object should include fit indices and reliability measures.}
}

\details{
The `fit_index_Table` function processes the output from an SEM analysis by extracting key model fit indices (e.g., Chi-square, SRMR, CFI, TLI, RMSEA) and reliability coefficients (e.g., omega coefficient). It combines these into a single tibble, where each row corresponds to a different model, and the columns represent the extracted indices and reliability measures.

During the function execution, messages and warnings are temporarily suppressed to ensure a clean output. The function also renames the columns to ensure they are consistent with the desired naming conventions for reporting.

This function is particularly useful for summarizing and comparing model fit across multiple models in a concise and standardized format.
}

\value{
A \code{tibble} containing the following columns:
\itemize{
  \item \code{Model} - The model identifier.
  \item \code{x2} - The Chi-square statistic.
  \item \code{df} - The degrees of freedom associated with the Chi-square statistic.
  \item \code{SRMR} - The Standardized Root Mean Square Residual.
  \item \code{CFI} - The Comparative Fit Index.
  \item \code{TLI} - The Tucker-Lewis Index.
  \item \code{RMSEA} - The Root Mean Square Error of Approximation.
  \item \code{CRMR} - The Coefficient of Residual Mean Square.
  \item \code{omega} - The omega reliability coefficient.
}
}

\examples{
# Example usage of the Table_fit_index function

# Assuming `resultados` is the output object from an SEM analysis
summary_table <- fit_index_Table(resultados)

# Display the summary table
print(summary_table)
}

\author{
Dr. José Ventura-León
}
