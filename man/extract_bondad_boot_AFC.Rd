\name{extract_bondad_boot_AFC}
\alias{extract_bondad_boot_AFC}
\title{Extraction of Goodness-of-Fit Measures from Bootstrap AFC Results}

\description{
This function extracts goodness-of-fit measures from a set of bootstrap results generated during confirmatory factor analysis (CFA) with bootstrapping. It is designed to work with the output structure of a custom CFA bootstrap analysis, specifically targeting the extraction of fit measures from each bootstrap sample's combined results. The function streamlines the process of aggregating these measures across all bootstrap samples, facilitating subsequent analysis and interpretation.
}

\usage{
extract_bondad_boot_AFC(results)
}

\arguments{
  \item{results}{A list containing the results of bootstrap CFA analysis. This list should have a specific structure, with each element representing a bootstrap sample and containing combined results of the CFA analysis, including goodness-of-fit measures.}
}

\details{
The function internally iterates over each bootstrap sample result, converting fit measures into a tabular format and adding columns to indicate the sample number and iteration within the sample. It utilizes 'dplyr' and 'bind_rows' to efficiently aggregate these tables across all bootstrap samples. The resulting dataframe provides a comprehensive view where each row corresponds to a specific goodness-of-fit measure from a distinct bootstrap sample, including an identifier for the sample and iteration.
}

\value{
A dataframe where each row represents a goodness-of-fit measure extracted from a bootstrap sample's CFA results. Columns include the goodness-of-fit measure identifiers, the sample number (muestra), and iteration within each bootstrap sample, facilitating easy comparison and analysis across samples.
}

\examples{
# Assuming 'results' is the output from a custom bootstrap CFA analysis
fit_measures_df <- extract_bondad_boot_AFC(results)

# To view the first few rows of the extracted goodness-of-fit measures
head(fit_measures_df)
}

\author{
Dr. José Ventura-León
}
