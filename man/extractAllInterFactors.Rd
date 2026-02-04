\name{extractAllInterFactors}
\alias{extractAllInterFactors}
\title{Extraction of Inter-Factor Correlations from Iterative EFA Results}

\description{
  The `extractAllInterFactors` function consolidates inter-factor correlations from each iteration within each replication of an iterative Exploratory Factor Analysis (EFA) process. It creates a comprehensive dataframe that lists all inter-factor correlations, facilitating an in-depth analysis of the relationships between factors across the entire EFA modeling process. This function is invaluable for examining the stability and consistency of factor correlations over iterations, aiding in the interpretation and validation of the factor structure.
}

\usage{
extractAllInterFactors(res)
}

\arguments{
  \item{res}{A list containing the results of an iterative EFA modeling process, which must include a `processed_results` component with detailed iteration-wise inter-factor correlations.}
}

\details{
  `extractAllInterFactors` iterates through the `processed_results` provided, extracting the inter-factor correlations for each iteration across all replications. These correlations are transformed into a standardized dataframe format, where each row represents a pair of factors and their correlation for a specific iteration and replication. The dataframe also includes replication and iteration identifiers, enabling comparisons and analyses of factor relationships throughout the EFA process.
}

\value{
  Returns a dataframe where each row corresponds to a pair of factors, detailing their correlation along with identifiers for the replication and iteration. This organized and comprehensive view allows researchers to easily assess the consistency of inter-factor relationships and their impact on the overall factor structure.
}

\examples{
# Assuming 'iterativeEFAResults' contains the output from an iterative EFA process
interFactorsDF <- extractAllInterFactors(iterativeEFAResults)
print(interFactorsDF)  # Display inter-factor correlations across all iterations
}

\author{
Dr. José Ventura-León
}
