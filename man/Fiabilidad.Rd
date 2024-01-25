\name{Fiabilidad}
\alias{Fiabilidad}
\title{Reliability Assessment Tool using Omega Coefficient}

\description{
This function is designed to calculate the composite reliability of factors in a dataset using the Omega coefficient. It is particularly useful in psychometric analysis for assessing the internal consistency of each factor. The function constructs and estimates a confirmatory factor analysis (CFA) model for each factor and then calculates the Omega reliability coefficient for these factors.
}

\usage{
Fiabilidad(vars, data)
}

\arguments{
  \item{vars}{A vector of variable names or an object obtained from the `extracted` function, indicating the items associated with a specific factor.}
  \item{data}{The dataset containing the responses to the items. It should be a data frame where columns represent items or variables.}
}

\details{
The function selects the variables specified in `vars` from the `data`, creates a CFA model using the `lavaan` package, and estimates it. The Omega reliability coefficient is then calculated for the factor using the `semTools` package. This method provides a more robust measure of reliability compared to traditional Cronbach's alpha, especially in cases of multidimensional constructs.
}

\value{
A numeric value representing the Omega reliability coefficient for the specified factor. This value indicates the internal consistency of the factor, with higher values suggesting greater reliability.
}

\examples{
# Assuming 'extracted' is an object with factor item names and 'Data_Rodriguez_renamed' is a dataset
vars <- extracted$Insensibilidad
resultado_final <- Fiabilidad(vars = vars, data = Data_Rodriguez_renamed)
print(resultado_final)
}

\author{
Professor José Ventura León
}

