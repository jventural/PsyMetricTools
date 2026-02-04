\name{extractAllFitMeasures}
\alias{extractAllFitMeasures}
\title{Extraction of All Fit Measures from Iterative EFA Results}

\description{
  The `extractAllFitMeasures` function retrieves all fit measures from each iteration within each replication of an iterative EFA (Exploratory Factor Analysis) process. It compiles these fit measures into a single dataframe for easy analysis and comparison. This function is particularly useful for evaluating the consistency and adequacy of model fit across iterations and replications, aiding in the refinement and validation of EFA models.
}

\usage{
extractAllFitMeasures(res)
}

\arguments{
  \item{res}{A list containing the results of an iterative EFA modeling process. This list must include a `processed_results` component that contains detailed results of each iteration, including fit measures.}
}

\details{
  `extractAllFitMeasures` parses the `processed_results` from the output of iterative EFA modeling to collect fit measures from each iteration across all replications. This comprehensive collection includes a wide range of fit indices, offering a nuanced view of model performance. By aggregating these measures, researchers can assess the overall stability and fit of their exploratory factor analysis models over multiple iterations and replications, facilitating a deeper understanding of model dynamics.
}

\value{
  Returns a dataframe where each row represents the fit measures from a specific iteration of a specific replication. Columns include the measure names, their values, and identifiers for both replication and iteration, enabling detailed analysis of model fit across the iterative EFA process.
}

\examples{
# Assuming 'iterativeEFAresult' is the output from an iterative EFA modeling process
fitMeasuresDF <- extractAllFitMeasures(iterativeEFAresult)
print(fitMeasuresDF)  # Display all fit measures collected across iterations
}

\author{
Dr. José Ventura-León
}
