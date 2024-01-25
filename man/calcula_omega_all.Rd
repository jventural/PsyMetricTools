\name{calcula_omega_all}
\alias{calcula_omega_all}
\title{Omega coefficient calculation}

\description{
This function calculates the reliability of all test factors in a dataset. It employs structural equation models to estimate the composite reliability, known as Omega, for each factor specified in the `extracted` object. The function is useful in psychometric analysis and for the validation of research instruments.
}

\usage{
calcula_omega_all(extracted, data)
}

\arguments{
  \item{extracted}{An object obtained using the `extract_items` function, containing a list of factors and their associated items.}
  \item{data}{The database containing item responses of the test or questionnaire. Must be in data frame format.}
}

\details{
The function performs the following steps:
1. Initializes a data frame to store the results.
2. Uses an internal function to calculate the composite reliability for each set of items associated with a factor.
3. Iterates over each factor defined in `extracted`, applying the structural equation model and extracting the Omega value.
4. Returns a data frame with the names of the factors and their corresponding Omega values.
}

\value{
A data frame with two columns: `Variables`, containing the names of the factors, and `Omega`, containing the composite reliability value for each factor.
}

\examples{
extracted <- extract_items(text)
final_result <- calcula_omega_all(extracted, data)
print(final_result)
}

\note{
Installation and loading of the `tidyverse`, `lavaan`, and `semTools` libraries are required for the proper functioning of this function.
}

\author{
Professor José Ventura León
}

