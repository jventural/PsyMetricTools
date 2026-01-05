\name{extract_ModIndex_AFC}
\alias{extract_ModIndex_AFC}
\title{Extraction of Modification Indices from Bootstrap AFC Results}

\description{
This function extracts modification indices from a set of bootstrap results generated during confirmatory factor analysis (CFA) with bootstrapping. It is designed to work with the output structure of a custom CFA bootstrap analysis, specifically targeting the extraction of modification indices from each bootstrap sample's combined results. The function streamlines the process of aggregating these indices across all bootstrap samples, facilitating subsequent analysis and interpretation.
}

\usage{
extract_ModIndex_AFC(results)
}

\arguments{
  \item{results}{A list containing the results of bootstrap CFA analysis. This list should have a specific structure, with each element representing a bootstrap sample and containing combined results of the CFA analysis, including modification indices.}
}

\details{
The function internally iterates over each bootstrap sample result, converting modification indices into a tabular format and adding columns to indicate the sample number. It utilizes 'dplyr' and 'bind_rows' to efficiently aggregate these tables across all bootstrap samples. The resulting dataframe provides a comprehensive view where each row corresponds to a specific modification suggestion from a distinct bootstrap sample, including an identifier for the sample.
}

\value{
A dataframe where each row represents a modification index extracted from a bootstrap sample's CFA results. Columns include the modification suggestion identifiers, the modification index (MI), and the sample number (muestra), facilitating easy comparison and analysis across samples.
}

\examples{
# Assuming 'results' is the output from a custom bootstrap CFA analysis
mod_indices_df <- extract_ModIndex_AFC(results)

# To view the first few rows of the extracted modification indices
head(mod_indices_df)
}

\author{
Dr. José Ventura-León
}
