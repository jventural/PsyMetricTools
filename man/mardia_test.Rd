\name{mardia_test}
\alias{mardia_test}
\title{Multivariate Normality Assessment Tool}

\description{
This function facilitates the assessment of multivariate normality in a dataset using Mardia's measures of skewness and kurtosis. It calculates Mardia's coefficients to determine whether the data deviate significantly from the multivariate normal distribution. This assessment is crucial in many statistical analyses, such as factor analysis, which assume multivariate normality of the data.
}

\usage{
mardia_test(data)
}

\arguments{
  \item{data}{A data frame or matrix where each column represents a variable to be included in the test for multivariate normality.}
}

\details{
Mardia's test evaluates multivariate skewness and kurtosis. The function returns both coefficients along with their associated p-values to assess the significance of the deviation from normality. A p-value less than 0.05 typically indicates a significant deviation from multivariate normality.
}

\value{
A data frame containing Mardia's skewness and kurtosis statistics, their corresponding p-values, and a result indicating whether each test suggests a deviation from normality (YES for significant deviation, NO otherwise).
}

\examples{
# Assuming 'data' is a data frame with multiple variables
test_results <- mardia_test(data)
print(test_results)
}

\author{
Professor José Ventura León
}

