\name{extract_fit_measures}
\alias{extract_fit_measures}
\title{Extraction of Goodness-of-Fit Measures for Factor Models}

\description{
This function is designed to extract and compile key goodness-of-fit measures for one or more factor models analyzed using the `lavaan` package. It is particularly useful for evaluating and comparing the performance of different factor models in exploratory or confirmatory factor analysis by providing a concise summary of various fit indices.
}

\usage{
extract_fit_measures(Specifications)
}

\arguments{
  \item{Specifications}{A list of `lavaan` model objects. Each object represents a factor model for which goodness-of-fit measures are to be extracted. Typically obtained from the `specification_models` function or similar model fitting functions in `lavaan`.}
}

\details{
The function iterates over each model in the `Specifications` list and extracts a set of standard goodness-of-fit measures, including the chi-squared test statistic, degrees of freedom, Standardized Root Mean Square Residual (SRMR), Weighted Root Mean Square Residual (WRMR), Comparative Fit Index (CFI), Tucker-Lewis Index (TLI), and Root Mean Square Error of Approximation (RMSEA). These measures are key indicators of how well the model fits the data.
}

\value{
A data frame containing the extracted goodness-of-fit measures for each model. The measures are presented in a structured format, making it easy to compare the fit of different models.
}

\examples{
# Assuming 'Specifications' is a list of lavaan model objects
fit_results <- extract_fit_measures(Specifications)
print(fit_results)
}

\author{
Dr. José Ventura-León
}
