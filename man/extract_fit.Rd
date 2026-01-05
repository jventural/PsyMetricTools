\name{extract_fit}
\alias{extract_fit}
\title{Extraction of Fit Measures from Factor Analysis Results}

\description{
Extracts fit measures from a factor analysis object created by the psych package.
}

\usage{
extract_fit(factors_data)
}

\arguments{
  \item{factors_data}{An object resulting from a factor analysis in the psych package.}
}

\value{
A dataframe with fit indices (RMSEA, TLI, CFI, BIC, etc.).
}

\examples{
\dontrun{
fit_summary <- extract_fit(factors_data)
print(fit_summary)
}
}

\author{
Dr. Jose Ventura-Leon
}

