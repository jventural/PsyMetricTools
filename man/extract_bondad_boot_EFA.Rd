\name{extract_bondad_boot_EFA}
\alias{extract_bondad_boot_EFA}
\title{Extraction of Goodness-of-Fit Measures from Bootstrap EFA Results}

\description{
This function extracts goodness-of-fit measures from a set of bootstrap results generated during exploratory factor analysis (EFA). It is designed to work with the output structure of a custom EFA bootstrap analysis, specifically targeting the extraction of 'Bondades' (goodness-of-fit measures) from each bootstrap sample's combined results. The function streamlines the process of aggregating these measures across all bootstrap samples, facilitating subsequent analysis and interpretation.
}

\usage{
extract_bondad_boot_EFA(resultados_bootstrap)
}

\arguments{
  \item{resultados_bootstrap}{A list containing the results of bootstrap EFA analysis. This list should have a specific structure, with each element representing a bootstrap sample and containing combined results of the EFA analysis, including 'Bondades'.}
}

\details{
  The function internally utilizes two key operations: converting the 'Bondades' into a tabular format and then aggregating these tables across all bootstrap samples. It leverages the 'purrr', 'dplyr', and 'tibble' packages to perform these operations efficiently. The output is a comprehensive dataframe where each row corresponds to a goodness-of-fit measure from a specific bootstrap sample, and includes an identifier for the sample.
}

\value{
  A dataframe where each row represents a goodness-of-fit measure extracted from a bootstrap sample's EFA results. Columns include the measure identifiers and the sample number, facilitating easy comparison and analysis across samples.
}

\examples{
# Assuming 'resultados_bootstrap' is the output from a custom bootstrap EFA analysis
bondades_df <- extract_bondad_boot_EFA(resultados_bootstrap)

# To view the first few rows of the extracted goodness-of-fit measures
head(bondades_df)
}

\author{
Dr. José Ventura-León
}
