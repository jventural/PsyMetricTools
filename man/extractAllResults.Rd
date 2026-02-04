\name{extractAllResults}
\alias{extractAllResults}
\title{Comprehensive Extraction of EFA Results Across Iterations}

\description{
  The `extractAllResults` function is designed to systematically gather and compile detailed results from each iteration of an Exploratory Factor Analysis (EFA) process conducted iteratively. This function extracts results across multiple replications and iterations, consolidating them into a single dataframe. This enables a thorough evaluation of parameter estimates, standard errors, Z-values, P-values, and confidence intervals for each variable analyzed during the EFA process.
}

\usage{
extractAllResults(res)
}

\arguments{
  \item{res}{A list structure containing the results of an iterative EFA modeling process, specifically including a `processed_results` component. This component must contain detailed iteration-wise results, including parameter estimates and statistical tests.}
}

\details{
  Upon execution, `extractAllResults` navigates through the `processed_results` of the provided EFA results, extracting and formatting the detailed results from each iteration into a standardized dataframe format. Each row in the resulting dataframe represents the analysis outcome for a variable in a particular iteration, including its estimates and statistical testing results. Additional columns are appended to identify the replication and iteration numbers, facilitating a nuanced analysis of the model's performance across different exploratory phases.
}

\value{
  Returns a comprehensive dataframe where each row pertains to the results of a specific variable from a specific iteration and replication. The columns detail the variable name, its estimate, standard error, Z-value, P-value, and confidence intervals, alongside identifiers for the replication and iteration. This consolidated view aids in the systematic review and comparison of EFA results.
}

\examples{
# Assuming 'iterativeEFAResults' contains the output from an iterative EFA process
allResultsDF <- extractAllResults(iterativeEFAResults)
print(allResultsDF)  # Display detailed results across all iterations
}

\author{
Dr. José Ventura-León
}
